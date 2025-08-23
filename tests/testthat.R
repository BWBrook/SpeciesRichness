# Minimal testthat harness; will be overwritten by usethis if run
if (requireNamespace("testthat", quietly = TRUE)) {
  testthat::test_that("bootstrap placeholder", {
    testthat::expect_true(TRUE)
  })
}

