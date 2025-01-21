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

# adapted from old if(FALSE) region which used 10000000L to benchmark
test_that("ramorder and sortordercache work", {
  withr::local_seed(348594L)

  x <- as.integer64(c(sample.int(10L), NA))
  sortordercache(x)

  for (na.last in c(FALSE, TRUE)) {
    for (decreasing in c(FALSE, TRUE)) {
      expect_identical(
        order(x, na.last=na.last, decreasing=decreasing),
        {
          xo = seq_along(x)
          ramorder(x, xo, na.last=na.last, decreasing=decreasing)
          xo
        }
      )
    }
  }
})
