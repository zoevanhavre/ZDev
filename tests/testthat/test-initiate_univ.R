context("initiate_univ")

test_that("test initiate_univ", {
  y <- rnorm(5,1)
  k <- 2

  expect_equal( length(initiate_univ(y, k=3, init.method="uniform")), 2)
  expect_equal( class(initiate_univ(y,k=2,init.method="uniform")), "list")
  expect_equal( class(initiate_univ(y,k=2,init.method="Kmeans")[[2]]), "integer")
  expect_error( initiate_univ(y, k=10, init.method="Kmeans"))
 initiate_univ(y,k=2,init.method="Kmeans")[[2]] %>% is.na() %>% sum() %>% expect_equal(., 0)
})
y
