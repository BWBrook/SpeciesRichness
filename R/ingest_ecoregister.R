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
  if (!file.exists(zip_path)) {
    rlang::abort(
      message = "Input archive not found.",
      class = "speciesrichness_file_missing",
      zip_path = zip_path
    )
  }
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
  if (length(miss)) {
    rlang::abort(
      message = paste0("Missing expected columns: ", paste(miss, collapse = ", ")),
      class = "speciesrichness_bad_schema",
      missing = miss
    )
  }
  cnt <- data.table::fifelse(!is.na(dt[["count"]]) & dt[["count"]] > 0, dt[["count"]], dt[["count 2"]])
  cnt <- as.integer(round(cnt))
  sp  <- dt[["species"]]
  sub <- dt[["subspecies"]]
  base <- ifelse(!is.na(sp) & nzchar(sp), paste(dt[["genus"]], sp), dt[["genus"]])
  taxon <- trimws(ifelse(!is.na(sub) & nzchar(sub), paste(base, sub), base))
  taxon[!nzchar(taxon)] <- NA_character_
  out <- data.table::data.table(
    sample_id = as.character(dt[["sample no"]]),
    taxon = taxon,
    count = cnt
  )
  out <- as.data.frame(out)
  out <- out[!is.na(out$count) & out$count > 0L, , drop = FALSE]

  # Aggregate duplicates within (sample_id, taxon) using base aggregate to
  # avoid data.table NSE issues on non-attached namespaces.
  if (!nrow(out)) {
    return(out)
  }
  agg <- stats::aggregate(
    count ~ sample_id + taxon,
    data = out[!is.na(out$taxon), , drop = FALSE],
    FUN = sum
  )
  x <- data.table::as.data.table(agg)
  data.table::setkey(x, sample_id)
  x[]
}

#' Summarise per-inventory richness and individuals
#'
#' @param x A data.table as returned by [read_ecoregister_counts()].
#'
#' @return A data.table with columns `sample_id`, `S`, `N`.
#' @export
summarise_per_inventory <- function(x) {
  need <- c("sample_id", "taxon", "count")
  if (!all(need %in% names(x))) {
    rlang::abort(
      message = paste0("Input missing columns: ", paste(setdiff(need, names(x)), collapse = ", ")),
      class = "speciesrichness_missing_columns",
      missing = setdiff(need, names(x))
    )
  }
  x <- as.data.frame(x)
  s <- stats::aggregate(
    taxon ~ sample_id,
    data = x,
    FUN = function(v) length(unique(v[!is.na(v)]))
  )
  n <- stats::aggregate(
    count ~ sample_id,
    data = x,
    FUN = sum
  )
  out <- merge(s, n, by = "sample_id", all = TRUE)
  names(out) <- c("sample_id", "S", "N")
  data.table::as.data.table(out)
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
build_co_sample_flat <- function(x, out = NULL) {
  if (is.null(out)) {
    out <- here::here("inst", "extdata", "co_sample_flat.csv")
  }
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(x[, .(sample_id, count)], out)
  invisible(out)
}
