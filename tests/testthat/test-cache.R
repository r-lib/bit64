test_that("is.sorted works", {
  expect_true(bit::is.sorted(integer64()))
  expect_true(bit::is.sorted(as.integer64(1:10)))
  expect_false(bit::is.sorted(as.integer64(10:1)))
})

test_that("na-last works", {
  x = as.integer64(c(1L, NA, 2L))
  
  sortcache(x, na.last=FALSE)
  expect_identical(cache(x)$sort, sort(x, na.last=FALSE))
  remcache(x)
  sortcache(x, na.last=TRUE)
  expect_identical(cache(x)$sort, sort(x, na.last=TRUE))
  remcache(x)
  
  sortordercache(x, na.last=FALSE)
  expect_identical(cache(x)$sort, sort(x, na.last=FALSE))
  expect_identical(cache(x)$order, order(x, na.last=FALSE))
  remcache(x)
  sortordercache(x, na.last=TRUE)
  expect_identical(cache(x)$sort, sort(x, na.last=TRUE))
  expect_identical(cache(x)$order, order(x, na.last=TRUE))
  remcache(x)
  
  ordercache(x, na.last=FALSE)
  expect_identical(cache(x)$order, order(x, na.last=FALSE))
  remcache(x)
  ordercache(x, na.last=TRUE)
  expect_identical(cache(x)$order, order(x, na.last=TRUE))
  remcache(x)
})

