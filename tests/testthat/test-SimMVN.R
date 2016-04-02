context("SimMVN")

test_that("SimMVN returns non-empty lists of same number of rows", {
  yz<-SimMVN(     Means = list( c(3,5), c(1,5)),
                  Covs = list( matrix(c(10,0.1,0.1,0.5), nrow=2),
                  matrix(c(0.5,0.1,0.1,10), nrow=2)),
                  P=c(0.7,0.3),
                  N=10)

  expect_equal( dim(yz$Y)[1] , dim(yz$Z)[1]) # test y and z are equal lengths
  expect_equal( dim(yz$Z)[2] , 1)
  expect_equal( length(yz), 2)
})
