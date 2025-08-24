# R/cegs.R
#' @title CEGS estimators
#' @description ML and likelihood-density grid estimators for the CEGS model.
#' @export
cegs_ml <- function(n) {
      data <- prep_data(n)
  if (length(data$u) < 3 || max(n) < 3) {
    return(map(
      list(richness = NA, scale = NA, shape = NA, AICc = NA,
           fitted_RAD = NA, fitted_SAD = NA),
      ~ .x
    ))
  }
  
  # Define likelihood for stats4::mle
  like_fn <- function(l, g) neg_loglik(c(l, g), data$s, data$u)
  
  # Attempt optimization from two different starts
  fit1 <- try(
    mle(like_fn,
        start = list(l = 1, g = 2),
        lower = c(l = 0, g = -1),
        upper = c(l = 1e8, g = 10),
        method = "L-BFGS-B"),
    silent = TRUE
  )
  fit2 <- try(
    mle(like_fn,
        start = list(l = 1, g = -2),
        lower = c(l = 0, g = -10),
        upper = c(l = 1e8, g = 1),
        method = "L-BFGS-B"),
    silent = TRUE
  )
  
  # Choose best-fitting model
  fits <- compact(list(fit1, fit2))
  if (length(fits) == 0) {
    return(map(
      list(richness = NA, scale = NA, shape = NA, AICc = NA,
           fitted_RAD = NA, fitted_SAD = NA),
      ~ .x
    ))
  }
  best_fit <- fits[[which.min(map_dbl(fits, ~ neg_loglik(coef(.x), data$s, data$u)))]]
  cf <- coef(best_fit)
  l <- cf["l"]
  g <- cf["g"]
  
  # Check for boundary solutions
  if (l %in% c(0, 1e8) || g %in% c(-10, 10)) {
    return(map(
      list(richness = NA, scale = NA, shape = NA, AICc = NA,
           fitted_RAD = NA, fitted_SAD = NA),
      ~ .x
    ))
  }
  
  # AICc calculation
  aic_val <- 2 * neg_loglik(c(l, g), data$s, data$u)
  AICc_val <- aic_val + 4 + 12 / (data$S2 - 3)
  
  # Compute predicted probabilities across full range
  max_n <- 2^14
  p_full <- map_dbl(
    1:max_n,
    ~ integrate(px_fun, i = .x, l = l, g = g,
                lower = 1e-20, upper = 1 - 1e-20, stop.on.error = FALSE)$value
  )
  p0 <- integrate(p0_fun,
                  l = l, g = g,
                  lower = 1e-20, upper = 1 - 1e-20, stop.on.error = FALSE)$value
  p_full <- p_full / (1 - p0)
  
  richness_est <- data$S / (1 - p0)
  
  list(
    richness   = as.numeric(richness_est),
    scale       = as.numeric(l),
    shape       = as.numeric(g),
    AICc        = AICc_val,
    fitted_RAD  = sadrad(length(n), p_full),
    fitted_SAD  = p_full[1:(2^12)]
  )
}

#' @export
cegs_ld <- function(n, penalty = TRUE) { 
  data <- prep_data(n)
  if (length(data$u) < 3 || max(n) < 3) {
    return(map(
      list(richness = NA, scale = NA, shape = NA, AICc = NA,
           fitted_RAD = NA, fitted_SAD = NA),
      ~ .x
    ))
  }
  
  # Create raw parameter grid
  seq_raw <- (1:51) / 10 - 2.6
  # L_raw: each column is seq_raw; G_raw: each row is seq_raw
  L_raw <- matrix(seq_raw, nrow = 51, ncol = 51)
  G_raw <- matrix(seq_raw, nrow = 51, ncol = 51, byrow = TRUE)
  # Direct vectorized transforms into parameter space
  L_grid <- ifelse(L_raw > 0, exp(L_raw^2), exp(-L_raw^2))
  G_grid <- ifelse(G_raw > 0,   G_raw^2,     -G_raw^2)
  # Evaluate negative log-likelihood on grid
  ll_mat <- matrix(Inf, nrow = 53, ncol = 53)
  for (i in seq_len(51)) {
    for (j in seq_len(51)) {
      ll_mat[i+1, j+1] <- neg_loglik(c(L_grid[i,j], G_grid[i,j]), data$s, data$u)
    }
  }
  
  # Convert to likelihood density
  ell <- exp(-ll_mat + min(ll_mat) + 5)
  
  # Compute gradient-based weights
  dr <- abs(ell[2:52,2:52] - ell[2:52,1:51]) + abs(ell[2:52,2:52] - ell[2:52,3:53])
  dc <- abs(ell[2:52,2:52] - ell[1:51,2:52]) + abs(ell[2:52,2:52] - ell[3:53,2:52])
  weights <- dr * dc
  weights <- weights / sum(weights)
  
  # Penalty towards reasonable parameter regions
  log_L <- log(L_grid)
  if (penalty) {
    penalty_mat <- exp(-abs(log_L)) * exp(-abs(abs(G_grid) - 1))
    weights <- (weights * penalty_mat) / sum(weights * penalty_mat)
  }
  
  # Weighted parameter estimates
  l_est <- exp(sum(weights * log_L))
  g_est <- sum(weights * G_grid)
  
  # AICc for estimated parameters
  aic_val <- 2 * neg_loglik(c(l_est, g_est), data$s, data$u)
  AICc_val <- aic_val + 4 + 12 / (data$S2 - 3)
  
  # Predicted probabilities
  max_n <- 2^14
  p_full <- map_dbl(
    1:max_n,
    ~ integrate(px_fun, i = .x, l = l_est, g = g_est,
                lower = 1e-20, upper = 1 - 1e-20, stop.on.error = FALSE)$value
  )
  p0 <- integrate(p0_fun,
                  l = l_est, g = g_est,
                  lower = 1e-20, upper = 1 - 1e-20, stop.on.error = FALSE)$value
  p_full <- p_full / (1 - p0)
  
  richness_est <- data$S / (1 - p0)
  
  list(
    richness   = as.numeric(richness_est),
    scale       = as.numeric(l_est),
    shape       = as.numeric(g_est),
    AICc        = AICc_val,
    fitted_RAD  = sadrad(length(n), p_full),
    fitted_SAD  = p_full[1:(2^12)]
  )
}

# --- helpers (internal; not exported) ---
sadrad    <- function(S, p) { ... }            # unchanged
px_fun    <- function(i, l, g, U) { ... }
p0_fun    <- function(U, l, g) { ... }
prep_data <- function(n, max_n = 2^14) { ... }
neg_loglik<- function(params, s, u) { ... }
