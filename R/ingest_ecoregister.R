#' Read species-level counts from the Ecological Register ZIP
#'
#' Returns a `data.table` with columns: `sample_id`, `taxon`, `count`.
#' Uses `count` if present, else `count 2`; drops zeros/NA.
#'
#' @param zip_path Path to the ZIP file returned by [fetch_ecoregister_zip()].
#' @param data_member Name of the gz member inside the ZIP to read.
#'
#' @return A data.table of long-form counts.
#' @export
read_ecoregister_counts <- function(zip_path,
                                    data_member = "Ecological_Register_data.txt.gz") {
  stopifnot(file.exists(zip_path))
  # Allow either a direct .gz path or a .zip containing the gz member
  is_zip <- grepl("\\.zip$", zip_path, ignore.case = TRUE)
  if (is_zip) {
    exdir <- file.path(tempdir(), "ecoregister_unzip")
    utils::unzip(zip_path, files = data_member, exdir = exdir, overwrite = TRUE)
    gz <- file.path(exdir, data_member)
  } else {
    gz <- zip_path
  }
  dt <- data.table::fread(gz, sep = "\t", quote = "", na.strings = c("", "NA"), showProgress = FALSE)
  # Column names in archive use spaces:
  req <- c("sample no","genus","species","subspecies","count","count 2")
  miss <- setdiff(req, names(dt))
  if (length(miss)) stop("Missing expected columns: ", paste(miss, collapse=", "))
  cnt <- data.table::fifelse(!is.na(dt[["count"]]) & dt[["count"]] > 0, dt[["count"]], dt[["count 2"]])
  cnt <- as.integer(round(cnt))
  taxon <- trimws(paste(dt[["genus"]], dt[["species"]], dt[["subspecies"]]))
  out <- data.table::data.table(
    sample_id = as.character(dt[["sample no"]]),
    taxon = data.table::fifelse(nchar(taxon) == 0L, NA_character_, taxon),
    count = cnt
  )
  out <- out[!is.na(count) & count > 0L]
  data.table::setkey(out, sample_id)
  out[]
}

#' Summarise per-inventory richness and individuals
#'
#' @param x A data.table as returned by [read_ecoregister_counts()].
#'
#' @return A data.table with columns `sample_id`, `S`, `N`.
#' @export
summarise_per_inventory <- function(x) {
  stopifnot(all(c("sample_id","count") %in% names(x)))
  x[, .(S = data.table::uniqueN(taxon), N = sum(count)), by = sample_id][]
}

#' Build flat co/sample objects (as files) expected by the legacy harness
#'
#' Writes CSV with columns `sample_id,count`; returns file path.
#'
#' @param x Data.table with columns `sample_id`, `count`.
#' @param out Output CSV path under `inst/extdata/`.
#'
#' @return The `out` path (invisibly).
#' @export
build_co_sample_flat <- function(x, out = "inst/extdata/co_sample_flat.csv") {
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(x[, .(sample_id, count)], out)
  invisible(out)
}
