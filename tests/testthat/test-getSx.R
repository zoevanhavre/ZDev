context("getSx")

test_that("test getSx", {
  y<-rnorm(5,1)
  z<- c(1,2,1,1,2)

  expect_equal( length(getSx(y,Z,k=3)), 3)
  expect_equal( class(getSx(y,Z,k=3)), "numeric")
  expect_error( getSx(y, Z,k=1))

})
