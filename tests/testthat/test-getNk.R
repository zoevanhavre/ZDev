context("getNk")

test_that("test getNk", {
.Z<-c(1,1,2)

  expect_equal( length(getNK(.Z,k=2)), 2 )
  expect_equal( length(getNK(.Z,k=3)), 3 )
  expect_equal( sum(getNK(.Z,k=3)==0), 1 )
})
