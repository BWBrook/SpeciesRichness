#' Internal helper to silence "not imported from" NOTES
#'
#' R CMD check flags packages listed in Imports that are not referenced
#' in the package code. We use some of these in non-package files (e.g.,
#' `_targets.R`). This hidden function references them safely so that
#' checks pass without altering runtime behavior.
#'
#' @keywords internal
#' @noRd
.silence_unused_imports <- function() {
  if (FALSE) {
    targets::tar_config_get
    tarchetypes::tar_combine
    readr::read_lines
    jsonlite::fromJSON
    glue::glue("")
  }
  invisible(TRUE)
}
