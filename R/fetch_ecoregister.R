#' Download the Ecological Register archive (Dryad)
#'
#' @param version Character Dryad version tag (e.g., "v20250703").
#' @param destdir Directory for cached downloads (untracked).
#' @param doi DOI string for the dataset (used for provenance).
#'
#' @return Path to the downloaded ZIP file (character scalar).
#' @export
fetch_ecoregister_zip <- function(version = "v20250703",
                                  destdir = "data-raw/cache",
                                  doi = "10.5061/dryad.brv15dvdc") {
  dir.create(destdir, recursive = TRUE, showWarnings = FALSE)
  out <- file.path(destdir, sprintf("Ecological_Register_%s.zip", version))
  if (!file.exists(out)) {
    cli::cli_alert_info("Downloading Ecological Register {version} to {out}")
    fname <- sprintf("doi_10_5061_dryad_brv15dvdc__%s.zip", version)
    url1 <- sprintf("https://datadryad.org/stash/downloads/download?filename=%s", utils::URLencode(fname, reserved = TRUE))
    ok <- FALSE
    ok <- ok || isTRUE(tryCatch({ curl::curl_download(url1, out); file.exists(out) && file.info(out)$size > 0 }, error = function(e) FALSE))
    if (!ok) {
      r <- tryCatch({ httr::RETRY("GET", url1, httr::write_disk(out, overwrite = TRUE), times = 3) }, error = identity)
      ok <- inherits(r, "response") && file.exists(out) && file.info(out)$size > 0
    }
    if (!ok) stop("Failed to download Ecological Register zip for version ", version)
  }
  out
}
