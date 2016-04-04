context("loglike_UNIV")

test_that("test loglike_UNIV", {
  y<-rnorm(5,1)
  z<- c(1,2,1,1,2)
  Weights <- c(0.7, 0.3)
  Means   <- c(0,1)
  Var     <- c(.5, 1)

  loglike_UNIV(z, y, Weights, Means, Var ) %>% length() %>% expect_equal(., 1)
  loglike_UNIV(z, y, Weights, Means, Var ) %>% class() %>% expect_equal(., "numeric")

  z<- c(2,2,2,2,2)
  loglike_UNIV(z, y, Weights, Means, Var ) %>% length() %>% expect_equal(., 1)

})
