# tests/testthat/test_cegs_equivalence.R
skip_heavy()
test_that("modern CEGS ≡ originals", {
  n <- sample(1:60, 120, TRUE)
  o_ml <- cegsML(n); o_ld <- cegsLD(n)  # from Alroy/
  m_ml <- cegs_ml(n); d_ld <- cegs_ld(n)
  expect_equal(m_ml$richness, o_ml$richness, tol = 1e-6)
  expect_equal(d_ld$richness, o_ld$richness, tol = 1e-6)
  # …scale/shape/AICc likewise
})
