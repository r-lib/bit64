test_that("base generic overwrites work", {
  x = c(2L, 4L, 3L)
  expect_identical(rank(x), c(1.0, 3.0, 2.0))
  expect_identical(order(x), c(1L, 3L, 2L))
})
