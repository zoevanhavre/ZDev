context("initiate_Z")

test_that("test initiate_Z works for MVN - output integers", {
  .y1 <-SimMVN(   Means = list( c(3,5), c(1,5)),
                  Covs = list( matrix(c(10,0.1,0.1,0.5), nrow=2),
                  matrix(c(0.5,0.1,0.1,10), nrow=2)),
                  P=c(0.7,0.3),
                  N=10)$Y

  expect_equal( class(initiate_Z(.y1, k=5,"Kmeans")), "integer" )
  expect_equal( class(initiate_Z(.y1, k=5,"random")), "integer" )
  expect_equal( class(initiate_Z(.y1, k=5,"single")), "integer")

})


test_that("test initiate_Z works for Univ - output integers", {
  .y1<-c(rnorm(5, 1, 1), rnorm(5, 3, 2))
    .y1<-.y1[sample(1:length(.y1), length(.y1))] # randomize

  expect_equal( class(initiate_Z(.y1, k=5,"Kmeans")), "integer" )
  expect_equal( class(initiate_Z(.y1, k=5,"random")), "integer" )
  expect_equal( class(initiate_Z(.y1, k=5,"single")), "integer")

})


test_that("test initiate_Z stops if k is larger than n", {
    .y1<-c(rnorm(5, 1, 1), rnorm(5, 3, 2))
  expect_error( initiate_Z(.y1, k=10, "Kmeans") )

    .y1 <-SimMVN(   Means = list( c(3,5), c(1,5)), Covs = list( matrix(c(10,0.1,0.1,0.5), nrow=2), matrix(c(0.5,0.1,0.1,10), nrow=2)),P=c(0.7,0.3), N=10)$Y
  expect_error( initiate_Z(.y1, k=20, "Kmeans"))
})
