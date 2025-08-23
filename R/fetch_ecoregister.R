#' Download the Ecological Register archive (Dryad)
#' @param version character, Dryad version tag (e.g., "v20250703")
#' @param destdir directory for cached downloads (untracked)
#' @return path to the downloaded ZIP file
fetch_ecoregister_zip <- function(version = "v20250703",
                                  destdir = "data-raw/cache",
                                  doi = "10.5061/dryad.brv15dvdc") {
  dir.create(destdir, recursive = TRUE, showWarnings = FALSE)
  url <- sprintf("https://datadryad.org/stash/downloads/file_stream/%%s", version)
  # Stable fallback via DOI landing if the direct versioned stream changes later
  out <- file.path(destdir, sprintf("Ecological_Register_%s.zip", version))
  if (!file.exists(out)) {
    cli::cli_alert_info("Downloading Ecological Register {version} to {out}")
    # Try direct version stream first; if it fails, fall back to DOI resolver
    ok <- tryCatch({
      curl::curl_download(sub("%s", "download?filename=doi_10_5061_dryad_brv15dvdc__" %+% version %+% ".zip", url), out); TRUE
    }, error = function(e) FALSE)
    if (!ok) {
      httr::RETRY("GET",
        sprintf("https://datadryad.org/stash/doi/%s", doi),
        httr::write_disk(out, overwrite = TRUE), times = 3)
    }
  }
  out
}

`%+%` <- function(a, b) paste0(a, b)

