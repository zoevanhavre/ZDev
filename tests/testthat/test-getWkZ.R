context("getWkZ")

test_that("test getWkZ", {
  .yz<-SimMVN(    Means = list( c(3,5), c(1,5)),
                  Covs = list( matrix(c(10,0.1,0.1,0.5), nrow=2),
                  matrix(c(0.5,0.1,0.1,10), nrow=2)),
                  P=c(0.7,0.3),
                  N=10)

  y<-.yz$Y
  Z<-.yz$Z %>% as.vector()

  expect_equal( length(getWkZ(y,Z,k=2)), 2 )
  expect_equal( class(getWkZ(y,Z,k=2)), "list" )
  getWkZ(y,Z, k=3) %>% is.na(.) %>% sum(.) %>% expect_equal(., 1)
})
