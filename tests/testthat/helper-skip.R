# tests/testthat/helper-skip.R
skip_heavy <- function() {
  testthat::skip_on_cran(); testthat::skip_on_ci()
}
