context("getYbar")

test_that("test getYbar", {
  .yz<-SimMVN(    Means = list( c(3,5), c(1,5)),
                  Covs = list( matrix(c(10,0.1,0.1,0.5), nrow=2),
                  matrix(c(0.5,0.1,0.1,10), nrow=2)),
                  P=c(0.7,0.3),
                  N=10)

  y<-.yz$Y
  Z<-.yz$Z %>% as.vector()
  
  expect_equal( length(getYbar(y,Z,k=2)), 2 )
  expect_equal( class(getYbar(y,Z,k=2)), "list" )
})
