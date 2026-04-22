test_that("shellsort with > 100 elements (triggers shellincs logic)", {
  # The shellincs bug triggers when n > 100 because for small n, 
  # the loop might not reach the problematic t index.
  # n=150 should be enough to exercise the loop.
  set.seed(42)
  n <- 150
  x <- as.integer64(sample.int(n))
  
  # This calls ram_integer64_shellsort_asc (or desc)
  # In the buggy version, this should trigger ASAN/UBSAN
  expect_error(bit::shellsort(x), NA)
  expect_identical(x, as.integer64(1:n))
  
  # Test descending
  x <- as.integer64(sample.int(n))
  expect_error(bit::shellsort(x, decreasing=TRUE), NA)
  expect_identical(x, as.integer64(n:1))
})

test_that("shellorder with > 100 elements", {
  set.seed(42)
  n <- 150
  x <- as.integer64(sample.int(n))
  ix <- seq_along(x)
  
  expect_error(bit::shellorder(x, ix), NA)
  expect_identical(x[ix], as.integer64(1:n))
})
