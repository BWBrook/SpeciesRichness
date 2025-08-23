#' Download the Ecological Register archive (Dryad)
#'
#' @param version Character Dryad version tag label used in filenames (e.g., "v20250703").
#' @param destdir Directory for cached downloads (untracked).
#' @param doi DOI string for the dataset (for provenance only).
#' @param file_stream_id Integer Dryad file_stream id for
#'   'Ecological_Register_data.txt.gz' (defaults to current known id).
#'
#' @return Path to the downloaded ZIP file (character scalar).
#' @export
fetch_ecoregister_zip <- function(version = "v20250703",
                                  destdir = "data-raw/cache",
                                  doi = "10.5061/dryad.brv15dvdc",
                                  file_stream_id = 4140130) {
  dir.create(destdir, recursive = TRUE, showWarnings = FALSE)
  # Prefer fetching the gz data file directly via file_stream id
  gz_out <- file.path(destdir, sprintf("Ecological_Register_data_%s.txt.gz", version))
  if (!file.exists(gz_out) || isTRUE(file.info(gz_out)$size == 0)) {
    url_gz <- sprintf("https://datadryad.org/downloads/file_stream/%s", as.integer(file_stream_id))
    cli::cli_alert_info("Downloading Ecological Register data (file_stream {file_stream_id}) to {gz_out}")
    ok <- FALSE
    ok <- ok || isTRUE(tryCatch({ curl::curl_download(url_gz, gz_out); file.exists(gz_out) && file.info(gz_out)$size > 0 }, error = function(e) FALSE))
    if (!ok) {
      r <- tryCatch({ httr::RETRY("GET", url_gz, httr::write_disk(gz_out, overwrite = TRUE), times = 3) }, error = identity)
      ok <- inherits(r, "response") && file.exists(gz_out) && file.info(gz_out)$size > 0
    }
    if (!ok) stop("Failed to download Ecological Register data.")
  }
  gz_out
}
