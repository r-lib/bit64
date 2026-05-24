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

test_that("basic cache operations and outdating", {
  x = as.integer64(1:3)

  # newcache
  cn = newcache(x)
  expect_s3_class(cn, "cache_integer64")
  expect_true(bit::still.identical(x, cn$x))

  # jamcache (creates new cache)
  x2 = as.integer64(1:3)
  cj = jamcache(x2)
  expect_s3_class(cj, "cache_integer64")
  expect_identical(attr(x2, "cache"), cj)

  # jamcache (returns existing cache if still identical)
  cj2 = jamcache(x2)
  expect_identical(cj, cj2)

  # Outdate the cache by modifying x2
  x2_old_cache = attr(x2, "cache")
  x2[1L] = 4L
  expect_warning({
    cj3 <- jamcache(x2)
  }, "replaced outdated cache")
  expect_false(identical(x2_old_cache, cj3))
  expect_identical(attr(x2, "cache"), cj3)

  # cache() returns NULL and warns if outdated
  x3 = as.integer64(1:3)
  jamcache(x3)
  x3[1L] = 4L
  expect_warning({
    c_out <- cache(x3)
  }, "removed outdated cache")
  expect_null(c_out)
  expect_null(attr(x3, "cache"))

  # setcache and getcache
  x4 = as.integer64(1:3)
  setcache(x4, "testkey", "testval")
  expect_identical(getcache(x4, "testkey"), "testval")
  expect_null(getcache(x4, "nonexistent"))

  # getcache on outdated cache
  x4[1L] = 4L
  expect_warning({
    g_out <- getcache(x4, "testkey")
  }, "removed outdated cache")
  expect_null(g_out)

  # remcache
  x5 = as.integer64(1:3)
  jamcache(x5)
  expect_false(is.null(attr(x5, "cache")))
  remcache(x5)
  expect_null(attr(x5, "cache"))
})

test_that("print.cache works", {
  x = as.integer64(1:3)
  ch = jamcache(x)
  setcache(x, "a", 1L)
  setcache(x, "b", 2L)

  expect_output(print(ch), "cache_integer64: a - b - x")
})

test_that("hashcache works", {
  x = as.integer64(c(1:3, 2:1))
  hashcache(x)
  ch = cache(x)
  expect_false(is.null(ch))
  expect_true(exists("hashmap", envir=ch, inherits=FALSE))
  expect_true(exists("na.count", envir=ch, inherits=FALSE))

  expect_true(exists("nunique", envir=ch, inherits=FALSE))
  expect_identical(getcache(x, "nunique"), 3L)
})

test_that("S3 methods benefit from cache", {
  x = as.integer64(c(3L, 1L, 2L, 2L, NA))

  # Without cache
  expect_identical(bit::na.count(x), 1L)
  expect_identical(bit::nvalid(x), 4L)
  expect_false(bit::is.sorted(x))
  expect_identical(bit::nunique(x), 4L)
  expect_identical(bit::nties(x), 2L)

  # With cache (empty) - use fresh x to avoid side effects
  x_cached = as.integer64(c(3L, 1L, 2L, 2L, NA))
  jamcache(x_cached)
  ch = cache(x_cached)
  expect_false(is.null(ch))
  expect_false(exists("na.count", envir=ch, inherits=FALSE))
  expect_false(exists("is.sorted", envir=ch, inherits=FALSE))
  expect_false(exists("nunique", envir=ch, inherits=FALSE))

  # Calling them should populate cache
  expect_identical(bit::na.count(x_cached), 1L)
  expect_true(exists("na.count", envir=ch, inherits=FALSE))
  expect_identical(getcache(x_cached, "na.count"), 1L)

  expect_false(bit::is.sorted(x_cached))
  expect_true(exists("is.sorted", envir=ch, inherits=FALSE))

  expect_false(exists("nunique", envir=ch, inherits=FALSE))
  expect_identical(bit::nunique(x_cached), 4L)
  expect_true(exists("nunique", envir=ch, inherits=FALSE))

  expect_null(getcache(x_cached, "nties"))
  expect_identical(bit::nties(x_cached), 2L)
})

test_that("S3 methods with sorted x", {
  x = as.integer64(c(1L, 2L, 2L, 3L))
  jamcache(x)
  ch = cache(x)

  expect_true(bit::is.sorted(x))
  expect_identical(bit::nunique(x), 3L)
  expect_identical(getcache(x, "nunique"), 3L)
  expect_identical(getcache(x, "nties"), 2L)
})
