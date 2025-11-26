test_that("cegs_ml matches legacy cegsML on deterministic sample", {
  reason <- if (exists("legacy_skip_reason")) legacy_skip_reason else "Legacy CEGS scripts not available"
  skip_if(!exists("legacy_available") || !isTRUE(legacy_available), reason)

  set.seed(1)
  n <- sample(1:60, size = 120, replace = TRUE)

  o <- cegsML(n)
  expect_false(any(is.na(unlist(o[c("richness", "scale", "shape", "AICc")]))))

  m <- cegs_ml(n)
  expect_equal(m$richness, o$richness, tolerance = 1e-2)
  expect_equal(m$scale,    o$scale,    tolerance = 5e-3)
  expect_equal(m$shape,    o$shape,    tolerance = 5e-3)  # legacy optimizer is noisy
  expect_equal(m$AICc,     o$AICc,     tolerance = 1e-3)
})

test_that("cegs_ld matches legacy cegsLD on fixed vector", {
  reason <- if (exists("legacy_skip_reason")) legacy_skip_reason else "Legacy CEGS scripts not available"
  skip_if(!exists("legacy_available") || !isTRUE(legacy_available), reason)

  set.seed(2)
  n <- sample(1:50, size = 150, replace = TRUE)

  o <- cegsLD(n)
  expect_false(any(is.na(unlist(o[c("richness", "scale", "shape", "AICc")]))))

  d <- cegs_ld(n)
  expect_equal(d$richness, o$richness, tolerance = 1e-6)
  expect_equal(d$scale,    o$scale,    tolerance = 1e-6)
  expect_equal(d$shape,    o$shape,    tolerance = 1e-6)
  expect_equal(d$AICc,     o$AICc,     tolerance = 1e-4)
})
