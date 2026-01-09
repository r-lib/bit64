test_that("is.sorted works", {
  expect_true(bit::is.sorted(integer64()))
  expect_true(bit::is.sorted(as.integer64(1:10)))
  expect_false(bit::is.sorted(as.integer64(10:1)))
})

test_that("na-last works", {
  x_32 = c(1L, NA, 2L)
  x_64 = as.integer64(x_32)

  sortcache(x_64, na.last=FALSE)
  expect_identical(cache(x_64)$sort, as.integer64(sort(x_32, na.last=FALSE)))
  remcache(x_64)
  sortcache(x_64, na.last=TRUE)
  expect_identical(cache(x_64)$sort, as.integer64(sort(x_32, na.last=TRUE)))
  remcache(x_64)

  sortordercache(x_64, na.last=FALSE)
  expect_identical(cache(x_64)$sort, as.integer64(sort(x_32, na.last=FALSE)))
  expect_identical(cache(x_64)$order, order(x_32, na.last=FALSE))
  remcache(x_64)
  sortordercache(x_64, na.last=TRUE)
  expect_identical(cache(x_64)$sort, as.integer64(sort(x_32, na.last=TRUE)))
  expect_identical(cache(x_64)$order, order(x_32, na.last=TRUE))
  remcache(x_64)

  ordercache(x_64, na.last=FALSE)
  expect_identical(cache(x_64)$order, order(x_32, na.last=FALSE))
  remcache(x_64)
  ordercache(x_64, na.last=TRUE)
  expect_identical(cache(x_64)$order, order(x_32, na.last=TRUE))
  remcache(x_64)
})

