test_that("cegs_ml matches legacy cegsML on fixed vector", {
  # Deterministic input for regression testing
  set.seed(123)
  n <- sample(1:40, size = 120, replace = TRUE)

  m <- cegs_ml(n)
  expect_equal(m$richness, 125.8285, tolerance = 1e-6)
  expect_equal(m$scale,    0.04856657, tolerance = 1e-6)
  expect_lt(abs(m$shape - 0.0001385158), 1e-6)
  expect_equal(m$AICc,     975.8108, tolerance = 1e-4)

  if (exists("legacy_available") && isTRUE(legacy_available)) {
    o <- try(cegsML(n), silent = TRUE)
    skip_if(inherits(o, "try-error") || any(is.na(unlist(o[c("richness", "scale", "shape", "AICc")]))),
            "Legacy cegsML failed on fixed vector")
    expect_equal(m$richness, o$richness, tolerance = 1e-6)
    expect_equal(m$scale,    o$scale,    tolerance = 1e-6)
    expect_equal(m$shape,    o$shape,    tolerance = 1e-4)
    expect_equal(m$AICc,     o$AICc,     tolerance = 1e-4)
  }
})

test_that("cegs_ld matches legacy cegsLD on fixed vector", {
  set.seed(321)
  n <- sample(1:50, size = 150, replace = TRUE)

  d <- cegs_ld(n)
  expect_equal(d$richness, 156.0729, tolerance = 1e-6)
  expect_equal(d$scale,    0.0394223, tolerance = 1e-6)
  expect_lt(abs(d$shape - 0.04358223), 1e-6)
  expect_equal(d$AICc,     1279.16, tolerance = 1e-4)

  if (exists("legacy_available") && isTRUE(legacy_available)) {
    o <- try(cegsLD(n), silent = TRUE)
    skip_if(inherits(o, "try-error") || any(is.na(unlist(o[c("richness", "scale", "shape", "AICc")]))),
            "Legacy cegsLD failed on fixed vector")
    expect_equal(d$richness, o$richness, tolerance = 1e-6)
    expect_equal(d$scale,    o$scale,    tolerance = 1e-6)
    expect_equal(d$shape,    o$shape,    tolerance = 1e-4)
    expect_equal(d$AICc,     o$AICc,     tolerance = 1e-4)
  }
})
