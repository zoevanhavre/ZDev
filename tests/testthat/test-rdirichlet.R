context("rdirichlet")

test_that("rdirichlet returns vector of probabilities", {
  expect_equal(sum(rdirichlet(c(1,1,1))), 1)
  expect_equal(sum(is.na(rdirichlet(c(1+0.000001,0.000001,0.0000001)))), 0)
})
