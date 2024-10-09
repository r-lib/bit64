test_that("order basics work", {
  expect_identical(order(as.integer64(c(2L, 4L, 3L))), c(1L, 3L, 2L))
})
