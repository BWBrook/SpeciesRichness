test_that("read_ecoregister_counts parses and filters counts", {
  skip_on_ci(); skip_if_not_installed("data.table"); skip_if_not_installed("R.utils")
  # Use a tiny synthetic snippet to avoid network in unit tests
  tmp <- tempfile(fileext = ".txt")
  cat("sample no\tgenus\tspecies\tsubspecies\tcount\tcount 2\n", file=tmp)
  cat("42\tAquila\tchrysaetos\t\t5\t\n", file=tmp, append=TRUE)
  cat("42\tAquila\taudax\t\t\t3\n", file=tmp, append=TRUE)
  gz <- tempfile(fileext = ".gz"); R.utils::gzip(tmp, destname = gz, overwrite = TRUE)
  z <- tempfile(fileext = ".zip"); utils::zip(z, files = gz, flags = "-j")
  dt <- read_ecoregister_counts(z, data_member = basename(gz))
  expect_equal(nrow(dt), 2L)
  s <- summarise_per_inventory(dt)
  expect_equal(s$S, 2L); expect_equal(s$N, 8L)
})

