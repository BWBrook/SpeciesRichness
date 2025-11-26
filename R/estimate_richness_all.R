#' Compute a suite of richness estimators for a single abundance vector
#'
#' This is a convenience wrapper that aggregates common abundance-based
#' richness estimators from several ecosystems packages.  Optional
#' dependencies are used opportunistically; missing packages or failed fits
#' simply yield `NA` for the corresponding estimator.
#'
#' @param counts Integer vector of species counts (one entry per observed species).
#'
#' @return A one-row `tibble` with summary features (S_obs, n, f1–f5, coverage)
#'   and estimator columns: `S_chao1_bc`, `S_ace`, `S_chiu_gp`, `S_gp_mle`,
#'   `S_cb`, `S_logseries`, `S_poilog`, `S_cegs_ml`, `S_cegs_ld`,
#'   `S2x_preseq`, `S5x_preseq`, `S_cov95_inext`.
#' @export
estimate_richness_all <- function(counts) {
  counts <- as.integer(counts)
  counts <- counts[!is.na(counts) & counts > 0L]
  if (!length(counts)) {
    rlang::abort("counts must contain at least one positive integer")
  }

  n <- sum(counts)
  S_obs <- length(counts)
  freq <- tabulate(counts)
  f1 <- if (length(freq) >= 1) freq[[1]] else 0L
  f2 <- if (length(freq) >= 2) freq[[2]] else 0L
  f3 <- if (length(freq) >= 3) freq[[3]] else 0L
  f4 <- if (length(freq) >= 4) freq[[4]] else 0L
  f5 <- if (length(freq) >= 5) freq[[5]] else 0L
  coverage <- if (n > 0) 1 - f1 / n else NA_real_

  out <- tibble::tibble(
    S_obs = S_obs,
    n = n,
    f1 = f1, f2 = f2, f3 = f3, f4 = f4, f5 = f5,
    coverage = coverage,
    S_chao1_bc = NA_real_,
    S_ace = NA_real_,
    S_chiu_gp = NA_real_,
    S_gp_mle = NA_real_,
    S_cb = NA_real_,
    S_logseries = NA_real_,
    S_poilog = NA_real_,
    S_cegs_ml = NA_real_,
    S_cegs_ld = NA_real_,
    S2x_preseq = NA_real_,
    S5x_preseq = NA_real_,
    S_cov95_inext = NA_real_
  )

  # vegan: Chao1 bias-corrected and ACE
  if (requireNamespace("vegan", quietly = TRUE)) {
    est <- try(vegan::estimateR(counts), silent = TRUE)
    if (!inherits(est, "try-error")) {
      if ("S.chao1" %in% rownames(est)) out$S_chao1_bc <- as.numeric(est["S.chao1", 1])
      if ("S.ACE"   %in% rownames(est)) out$S_ace       <- as.numeric(est["S.ACE", 1])
    }
  }

  # breakaway: Chao-Bunge estimator
  if (requireNamespace("breakaway", quietly = TRUE)) {
    cb <- try(breakaway::chao_bunge(counts), silent = TRUE)
    if (!inherits(cb, "try-error") && !is.null(cb$estimate)) {
      out$S_cb <- as.numeric(cb$estimate)
    }
  }

  # Log-series richness (Fisher alpha)
  if (requireNamespace("sads", quietly = TRUE)) {
    fit_ls <- try(sads::fitsad(counts, "ls"), silent = TRUE)
    if (!inherits(fit_ls, "try-error")) {
      alpha <- try(as.numeric(coef(fit_ls)[["alpha"]]), silent = TRUE)
      if (!inherits(alpha, "try-error") && is.finite(alpha)) {
        out$S_logseries <- alpha * log(1 + n / alpha)
      }
    }
    fit_pl <- try(sads::fitsad(counts, "poilog"), silent = TRUE)
    if (!inherits(fit_pl, "try-error")) {
      pars <- try(coef(fit_pl), silent = TRUE)
      if (!inherits(pars, "try-error") && all(c("mu","sig") %in% names(pars))) {
        mu <- pars[["mu"]]; sig <- pars[["sig"]]
        p0 <- try(poilog::ppoilog(0, mu = mu, sig = sig, lower.tail = TRUE), silent = TRUE)
        if (!inherits(p0, "try-error") && is.finite(p0) && p0 < 1) {
          out$S_poilog <- S_obs / (1 - p0)
        }
      }
    }
  }

  # CEGS (modern implementations)
  fit_ml <- try(cegs_ml(counts), silent = TRUE)
  if (!inherits(fit_ml, "try-error") && is.list(fit_ml)) {
    out$S_cegs_ml <- fit_ml$richness
  }
  fit_ld <- try(cegs_ld(counts), silent = TRUE)
  if (!inherits(fit_ld, "try-error") && is.list(fit_ld)) {
    out$S_cegs_ld <- fit_ld$richness
  }

  # SpadeR / SPECIES: Chao1/ACE/iChao1/GP-MLE if available
  if (requireNamespace("SpadeR", quietly = TRUE)) {
    gp <- try(SpadeR::ChaoSpecies(counts, datatype = "abundance"), silent = TRUE)
    if (!inherits(gp, "try-error") && is.list(gp) && !is.null(gp$Species_table)) {
      tbl <- gp$Species_table
      rn <- rownames(tbl)
      grab <- function(name) {
        if (name %in% rn) as.numeric(tbl[name, "Estimate"]) else NA_real_
      }
      out$S_chao1_bc <- ifelse(is.na(out$S_chao1_bc), grab("Chao1-bc"), out$S_chao1_bc)
      out$S_ace      <- ifelse(is.na(out$S_ace),      grab("ACE (Chao & Lee, 1992)"), out$S_ace)
      out$S_chiu_gp  <- grab("iChao1 (Chiu et al. 2014)")
      out$S_gp_mle   <- grab("Homogeneous (MLE)")
    }
  } else if (requireNamespace("SPECIES", quietly = TRUE)) {
    gp <- try(SPECIES::ChaoSpecies(counts, datatype = "abundance"), silent = TRUE)
    if (!inherits(gp, "try-error") && is.list(gp) && !is.null(gp$Species_table)) {
      tbl <- gp$Species_table
      rn <- rownames(tbl)
      grab <- function(name) {
        if (name %in% rn) as.numeric(tbl[name, "Estimate"]) else NA_real_
      }
      out$S_chiu_gp <- grab("iChao1 (Chiu et al. 2014)")
      out$S_gp_mle  <- grab("Homogeneous (MLE)")
    }
  }

  # iNEXT coverage-based richness at 95% coverage
  if (requireNamespace("iNEXT", quietly = TRUE)) {
    inext <- try(iNEXT::estimateD(counts, datatype = "abundance",
                                  base = "coverage", level = 0.95),
                 silent = TRUE)
    if (!inherits(inext, "try-error") && "qD" %in% names(inext)) {
      out$S_cov95_inext <- as.numeric(inext$qD[1])
    }
  }

  # preseqR discovery extrapolation to 2x and 5x sampling effort
  if (requireNamespace("preseqR", quietly = TRUE)) {
    fvec <- tabulate(counts)
    idx <- which(fvec > 0)
    mat <- cbind(idx, fvec[idx])
    sac_fun <- try(preseqR::ds.rSAC(mat), silent = TRUE)
    if (!inherits(sac_fun, "try-error") && is.function(sac_fun)) {
      preds <- try(as.numeric(sac_fun(c(n * 2, n * 5))), silent = TRUE)
      if (!inherits(preds, "try-error") && length(preds) >= 2) {
        out$S2x_preseq <- preds[[1]]
        out$S5x_preseq <- preds[[2]]
      }
    }
  }

  out
}
