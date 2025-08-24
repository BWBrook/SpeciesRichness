test_that("cegs_ml matches legacy cegsML on random sample", {
  set.seed(42)
  n <- sample(1:50, size = 100, replace = TRUE)
  o <- cegsML(n); m <- cegs_ml(n)
  expect_equal(m$richness, o$richness, tolerance = 1e-6)
  expect_equal(m$scale,    o$scale,    tolerance = 1e-6)
  expect_equal(m$shape,    o$shape,    tolerance = 1e-6)
  expect_equal(m$AICc,     o$AICc,     tolerance = 1e-4)
})

test_that("cegs_ld matches legacy cegsLD on random sample", {
  set.seed(43)
  n <- sample(1:60, size = 120, replace = TRUE)
  o <- cegsLD(n); d <- cegs_ld(n)
  expect_equal(d$richness, o$richness, tolerance = 1e-6)
  expect_equal(d$scale,    o$scale,    tolerance = 1e-6)
  expect_equal(d$shape,    o$shape,    tolerance = 1e-6)
  expect_equal(d$AICc,     o$AICc,     tolerance = 1e-4)
})
