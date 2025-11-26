#' Fit CEGS models (ML and LD) for a single abundance vector
#'
#' Returns point estimates of richness, scale, shape, and AICc for both
#' maximum-likelihood (ML) and likelihood-density (LD) variants. Failed fits
#' are returned as `NA`.
#'
#' @param counts Integer vector of species counts (one per observed species).
#'
#' @return A one-row tibble with columns:
#'   `S_obs`, `n`,
#'   `S_cegs_ml`, `scale_ml`, `shape_ml`, `AICc_ml`, `ml_converged`,
#'   `S_cegs_ld`, `scale_ld`, `shape_ld`, `AICc_ld`, `ld_converged`.
#' @export
fit_cegs_params <- function(counts) {
  counts <- as.integer(counts)
  counts <- counts[!is.na(counts) & counts > 0L]
  S_obs <- length(counts)
  n <- sum(counts)

  res_ml <- try(cegs_ml(counts), silent = TRUE)
  res_ld <- try(cegs_ld(counts), silent = TRUE)

  ok_ml <- !inherits(res_ml, "try-error") && is.list(res_ml) &&
    !any(is.na(unlist(res_ml[c("richness", "scale", "shape", "AICc")])))
  ok_ld <- !inherits(res_ld, "try-error") && is.list(res_ld) &&
    !any(is.na(unlist(res_ld[c("richness", "scale", "shape", "AICc")])))

  tibble::tibble(
    S_obs = S_obs,
    n = n,
    S_cegs_ml = if (ok_ml) res_ml$richness else NA_real_,
    scale_ml = if (ok_ml) res_ml$scale else NA_real_,
    shape_ml = if (ok_ml) res_ml$shape else NA_real_,
    AICc_ml = if (ok_ml) res_ml$AICc else NA_real_,
    ml_converged = ok_ml,
    S_cegs_ld = if (ok_ld) res_ld$richness else NA_real_,
    scale_ld = if (ok_ld) res_ld$scale else NA_real_,
    shape_ld = if (ok_ld) res_ld$shape else NA_real_,
    AICc_ld = if (ok_ld) res_ld$AICc else NA_real_,
    ld_converged = ok_ld
  )
}
