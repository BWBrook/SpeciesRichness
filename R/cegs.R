"#' CEGS estimators" 
#' ML and likelihood-density grid estimators for the CEGS model.
#'
#' @param n Integer vector of species counts.
#' @return A list with fields `richness`, `scale`, `shape`, `AICc`, `fitted_RAD`, `fitted_SAD`.
#' @export
cegs_ml <- function(n) {
  n <- as.integer(n)
  if (length(unique(n)) < 3L) {
    return(list(richness = NA_real_, scale = NA_real_, shape = NA_real_, AICc = NA_real_, fitted_RAD = NA, fitted_SAD = NA))
  }

  import::from("stats4", mle)

  pd <- .prep_data(n)
  like <- function(l, g) .neg_loglik(l = l, g = exp(g), s = pd$s, u = pd$u, adjust = 1e-8)

  cf <- try(stats4::mle(like,
                        lower = list(l = 0, g = -10),
                        upper = list(l = 1e8, g = 10),
                        start = list(l = 1, g = 2)),
            silent = TRUE)
  if (inherits(cf, "try-error")) {
    return(list(richness = NA_real_, scale = NA_real_, shape = NA_real_, AICc = NA_real_, fitted_RAD = NA, fitted_SAD = NA))
  }
  co <- coef(cf)
  l <- unname(co[["l"]]); g_log <- unname(co[["g"]])
  if (is.na(l) || is.na(g_log) || l %in% c(0, 1e8) || g_log %in% c(-10, 10)) {
    return(list(richness = NA_real_, scale = NA_real_, shape = NA_real_, AICc = NA_real_, fitted_RAD = NA, fitted_SAD = NA))
  }
  aicc <- 2 * like(l, g_log) + 4 + 12 / (pd$S - 3)
  g <- exp(g_log)

  mx <- max(2^12, 2^ceiling(log2(max(n))))
  p <- vapply(seq_len(mx), function(i) stats::integrate(.px_fun, l = l, g = g, i = i, adjust = 1e-8, lower = 1e-20, upper = 1 - 1e-20, stop.on.error = FALSE)$value, numeric(1))
  p0 <- stats::integrate(.p0_fun, l = l, g = g, lower = 1e-20, upper = 1 - 1e-20, stop.on.error = FALSE)$value
  p <- p / (1 - p0)

  list(
    richness   = as.numeric(pd$S / (1 - p0)),
    scale      = as.numeric(l),
    shape      = as.numeric(g),
    AICc       = as.numeric(aicc),
    fitted_RAD = .sadrad(length(n), p),
    fitted_SAD = p[1:(2^12)]
  )
}

#' @export
cegs_ld <- function(n) {
  n <- as.integer(n)
  if (length(unique(n)) < 3L) {
    return(list(richness = NA_real_, scale = NA_real_, shape = NA_real_, AICc = NA_real_, fitted_RAD = NA, fitted_SAD = NA))
  }
  pd <- .prep_data(n)
  like <- function(l, g) .neg_loglik(l = l, g = g, s = pd$s, u = pd$u, adjust = 1e-4)

  ml <- matrix(52 / (1:51) - 1, 51, 51)
  mg <- t(ml)
  ml <- ml / exp(mean(log(n)))
  ll <- matrix(Inf, 53, 53)
  for (i in 1:51) {
    for (j in 1:51) {
      ll[i + 1, j + 1] <- like(ml[i, j], mg[i, j])
    }
  }
  ell <- exp(-ll + min(ll, na.rm = TRUE) + 5)
  dr <- abs(ell[2:52, 2:52] - ell[2:52, 1:51]) + abs(ell[2:52, 2:52] - ell[2:52, 3:53])
  dc <- abs(ell[2:52, 2:52] - ell[1:51, 2:52]) + abs(ell[2:52, 2:52] - ell[3:53, 2:52])
  d <- dr * dc
  d <- d / sum(d)
  ml2 <- 1 / (ml + 1)
  mg2 <- 1 / (mg + 1)
  l <- sum(d * ml2)
  g <- sum(d * mg2)
  l <- 1 / l - 1
  g <- 1 / g - 1

  aicc <- 2 * like(l, g) + 4 + 12 / (pd$S - 3)
  mx <- max(2^12, 2^ceiling(log2(max(n))))
  p <- vapply(seq_len(mx), function(i) stats::integrate(.px_fun, l = l, g = g, i = i, adjust = 1e-4, lower = 1e-20, upper = 1 - 1e-20, stop.on.error = FALSE)$value, numeric(1))
  p0 <- stats::integrate(.p0_fun, l = l, g = g, lower = 1e-20, upper = 1 - 1e-20, stop.on.error = FALSE)$value
  p <- p / (1 - p0)

  list(
    richness   = as.numeric(pd$S / (1 - p0)),
    scale      = as.numeric(l),
    shape      = as.numeric(g),
    AICc       = as.numeric(aicc),
    fitted_RAD = .sadrad(length(n), p),
    fitted_SAD = p[1:(2^12)]
  )
}

# --- helpers (internal; not exported) ---

.sadrad <- function(S, p) {
  p <- p / sum(p)
  r <- numeric(S)
  q <- (1:S) / (S + 1)
  cs <- cumsum(p)
  w <- 1L
  for (i in 1:S) {
    for (j in w:length(p)) {
      if (cs[j] > q[i]) break
    }
    w <- j
    if (w == 1L) r[i] <- 1 else r[i] <- w
  }
  r
}

.px_fun <- function(U, l, g, i, adjust = 1e-8) {
  p <- 1 / (((-log(U))^g) / l + 1)
  p[p == 0] <- adjust
  stats::dgeom(i, p)
}

.p0_fun <- function(U, l, g) {
  1 / (((-log(U))^g) / l + 1)
}

.prep_data <- function(n) {
  S <- length(n)
  s <- rep.int(0L, max(n))
  t <- table(n)
  s[as.integer(names(t))] <- as.integer(t)
  u <- which(s > 0L)
  list(S = S, s = s, u = u)
}

.neg_loglik <- function(l, g, s, u, adjust = 1e-8) {
  p <- vapply(seq_along(u), function(idx) {
    i <- u[[idx]]
    stats::integrate(function(U) {
      pp <- 1 / (((-log(U))^g) / l + 1)
      pp[pp == 0] <- adjust
      stats::dgeom(i, pp)
    }, lower = 1e-20, upper = 1 - 1e-20, stop.on.error = FALSE)$value
  }, numeric(1))
  if (is.nan(p[1]) || min(p) == 0) return(1e10)
  p0 <- stats::integrate(.p0_fun, l = l, g = g, lower = 1e-20, upper = 1 - 1e-20, stop.on.error = FALSE)$value
  p <- p / (1 - p0)
  ll <- -sum(s[u] * log(p))
  if (!is.finite(ll) || is.nan(ll)) return(1e10)
  ll
}
