test_that("estimate_richness_all returns expected core fields", {
  counts <- c(4, 3, 3, 2, 1, 1, 5, 7)
  res <- estimate_richness_all(counts)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1L)
  needed <- c("S_obs", "n", "f1", "f2", "coverage",
              "S_chao1_bc", "S_ace", "S_cegs_ml", "S_cegs_ld")
  expect_true(all(needed %in% names(res)))
  expect_equal(res$S_obs, length(counts))
  expect_equal(res$n, sum(counts))
  expect_equal(res$f1, sum(counts == 1))
  expect_equal(res$f2, sum(counts == 2))
})
