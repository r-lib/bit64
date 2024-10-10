test_that("order basics work", {
  x = as.integer64(c(2L, 4L, 3L))
  expect_identical(order(x), c(1L, 3L, 2L))
  expect_identical(order(x, decreasing=TRUE), c(2L, 3L, 1L))

  x = c(x, NA_integer64_)
  expect_identical(order(x), c(1L, 3L, 2L, 4L))
  expect_identical(order(x, decreasing=TRUE), c(2L, 3L, 1L, 4L))
  expect_identical(order(x, na.last=FALSE), c(4L, 1L, 3L, 2L))
  expect_identical(order(x, na.last=FALSE, decreasing=TRUE), c(4L, 2L, 3L, 1L))
})
