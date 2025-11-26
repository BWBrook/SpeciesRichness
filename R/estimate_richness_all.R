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
      if ("S.chao1" %in% names(est)) out$S_chao1_bc <- as.numeric(est[["S.chao1"]])
      if ("S.ACE"   %in% names(est)) out$S_ace       <- as.numeric(est[["S.ACE"]])
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
  }

  # Poisson-lognormal richness (zero-truncation adjustment)
  if (requireNamespace("poilog", quietly = TRUE)) {
    fit_pl <- try(poilog::poilogMLE(counts), silent = TRUE)
    if (!inherits(fit_pl, "try-error") && all(c("mu", "sig") %in% names(fit_pl$par))) {
      mu  <- fit_pl$par[["mu"]]
      sig <- fit_pl$par[["sig"]]
      p0 <- try(poilog::ppoilog(0, mu = mu, sig = sig, lower.tail = TRUE), silent = TRUE)
      if (!inherits(p0, "try-error") && is.finite(p0) && p0 < 1) {
        out$S_poilog <- S_obs / (1 - p0)
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

  # SpadeR / SPECIES: generalized-Poisson variants (best-effort)
  if (requireNamespace("SpadeR", quietly = TRUE)) {
    gp <- try(SpadeR::ChaoSpecies(counts, datatype = "abundance"), silent = TRUE)
    if (!inherits(gp, "try-error")) {
      # ChaoSpecies returns a data.frame of estimates; use available columns if present
      if ("Species.richness" %in% names(gp)) {
        out$S_chiu_gp <- as.numeric(gp[["Species.richness"]])
      } else if (!is.null(gp$Estimator) && "Chao.Bunge" %in% gp$Estimator$Estimator) {
        out$S_chiu_gp <- as.numeric(gp$Estimator$`Estimate`[gp$Estimator$Estimator == "Chao.Bunge"])
      }
      if (!is.null(gp$info) && "GPMLE" %in% names(gp$info)) {
        out$S_gp_mle <- as.numeric(gp$info[["GPMLE"]])
      }
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
    fvec <- fvec[fvec > 0]
    # preseqR expects a vector of frequency-of-frequency counts (f1, f2, ...)
    pr <- try(preseqR::ds.rSAC(f = fvec, t = c(n * 2, n * 5)), silent = TRUE)
    if (!inherits(pr, "try-error") && is.matrix(pr)) {
      # ds.rSAC returns a matrix with rows = t, cols = estimates
      out$S2x_preseq <- as.numeric(pr[1, 2])
      if (nrow(pr) >= 2) out$S5x_preseq <- as.numeric(pr[2, 2])
    }
  }

  out
}
