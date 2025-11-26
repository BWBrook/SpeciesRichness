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
  # Allow explicit local override via environment variable
  env_path <- Sys.getenv("ECOREG_DATA_PATH", unset = "")
  if (nzchar(env_path) && file.exists(env_path)) return(env_path)

  is_gz <- function(p) {
    if (!file.exists(p)) return(FALSE)
    con <- file(p, "rb"); on.exit(close(con), add = TRUE)
    sig <- tryCatch(readBin(con, "raw", n = 2L), error = function(e) raw(0))
    length(sig) >= 2L && identical(as.integer(sig[1:2]), c(0x1fL, 0x8bL))
  }
  have_valid <- file.exists(gz_out) && isTRUE(file.info(gz_out)$size > 0) && is_gz(gz_out)
  if (!have_valid) {
    # Prefer Dryad v2 API download endpoint; fall back to legacy file_stream URLs
    candidates <- c(
      sprintf("https://datadryad.org/api/v2/files/%s/download", as.integer(file_stream_id)),
      sprintf("https://datadryad.org/stash/downloads/file_stream/%s?download=1", as.integer(file_stream_id)),
      sprintf("https://datadryad.org/downloads/file_stream/%s?download=1", as.integer(file_stream_id))
    )
    ua <- httr::user_agent("SpeciesRichness/0.0.0.9000 (R httr)")
    for (u in candidates) {
      cli::cli_alert_info("Trying Dryad -> {u}")
      ok <- isTRUE(tryCatch({ curl::curl_download(u, gz_out); TRUE }, error = function(e) FALSE)) && is_gz(gz_out)
      if (!ok) {
        r <- tryCatch({ httr::RETRY("GET", u, ua, httr::write_disk(gz_out, overwrite = TRUE), times = 3) }, error = identity)
        ok <- inherits(r, "response") && is_gz(gz_out)
      }
      if (ok) break
    }
    have_valid <- file.exists(gz_out) && is_gz(gz_out)
  }
  if (!have_valid) {
    rlang::abort(
      message = paste0(
        "Failed to obtain Ecological Register data (.gz). ",
        "Place a valid file under '", destdir, "' and re-run, or pass a working ",
        "file_stream_id to fetch_ecoregister_zip()."
      ),
      class = "speciesrichness_fetch_failed",
      destdir = destdir,
      file_stream_id = file_stream_id
    )
  }
  gz_out
}
