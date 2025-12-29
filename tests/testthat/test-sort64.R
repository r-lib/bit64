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
local({
  set.seed(348594L)

  x <- as.integer64(c(sample.int(10L), NA))
  sortordercache(x)
  
  test_with_cases({
    test_that(sprintf("ramorder and sortordercache work for na.last=%s, decreasing=%s", na.last, decreasing), {
      expect_identical(
        order(x, na.last=na.last, decreasing=decreasing),
        {
          xo = seq_along(x)
          ramorder(x, xo, na.last=na.last, decreasing=decreasing)
          xo
        }
      )
    })
  }, 
  cases=expand.grid(
    na.last=c(FALSE, TRUE), 
    decreasing=c(FALSE, TRUE)
    )
  )
})

test_that("sorting methods for integer64 work", {
  test_with_cases({
    withr::local_options(list(bit64.warn.exported.s3.method = FALSE))
    x = as.integer64(1:10)

    na_entries = rep(NA_integer64_, n_missing)
    y = sample(c(x, if (duplicates) x[1L], na_entries))
    expect_identical(sort_function(y, decreasing=decreasing, na.last=na.last), n_missing)
    # TODO(#154): Drop explicit 'else' branches
    expected_value = c(
      if (na.last) integer64() else na_entries,
      if (duplicates && !decreasing) x[1L],
      if (decreasing) rev(x) else x,
      if (duplicates && decreasing) x[1L],
      if (na.last) na_entries else integer64()
    )
    expect_identical(y, expected_value)
  },
  cases=expand.grid(
    sort_function=list(mergesort, quicksort, radixsort, ramsort, shellsort),
    na.last=c(FALSE, TRUE),
    decreasing=c(FALSE, TRUE),
    duplicates=c(FALSE, TRUE),
    n_missing=0:2
    )
  )
})

test_that("order methods for integer64 work", {
  test_with_cases({
    withr::local_options(list(bit64.warn.exported.s3.method = FALSE))
    x = as.integer64(1:10)

    na_entries = rep(NA_integer64_, n_missing)
    y = sample(c(x, if (duplicates) x[1L], na_entries))
    i = seq_along(y)
    expect_identical(order_function(y, i, decreasing=decreasing, na.last=na.last), n_missing)
    # TODO(#154): Drop explicit 'else' branches
    expected_value = c(
      if (na.last) integer64() else na_entries,
      if (duplicates && !decreasing) x[1L],
      if (decreasing) rev(x) else x,
      if (duplicates && decreasing) x[1L],
      if (na.last) na_entries else integer64()
    )
    expect_identical(y[i], expected_value)
  }, 
  cases=expand.grid(
    order_function=list(mergeorder, quickorder, radixorder, ramorder, shellorder),
    na.last=c(FALSE, TRUE),
    decreasing=c(FALSE, TRUE),
    duplicates=c(FALSE, TRUE),
    n_missing=0:2
    )
  )
})

test_that("sortorder methods for integer64 work", {
  test_with_cases({
    withr::local_options(list(bit64.warn.exported.s3.method = FALSE))
    x = as.integer64(1:10)

    na_entries = rep(NA_integer64_, n_missing)
    y = sample(c(x, if (duplicates) x[1L], na_entries))
    i = seq_along(y)
    expect_identical(sortorder_function(y, i, decreasing=decreasing, na.last=na.last), n_missing)
    # TODO(#154): Drop explicit 'else' branches
    expected_value = c(
      if (na.last) integer64() else na_entries,
      if (duplicates && !decreasing) x[1L],
      if (decreasing) rev(x) else x,
      if (duplicates && decreasing) x[1L],
      if (na.last) na_entries else integer64()
    )
    # TODO(#159): Also add expectations for the update to i
    expect_identical(y, expected_value,
      info=sprintf(
        "(na.last, decreasing, duplicates, n_missing)=(%s, %s, %s, %d)",
        na.last, decreasing, duplicates, n_missing
      )
    )
  }, 
  cases=expand.grid(
    sortorder_function=list(mergesortorder, quicksortorder, radixsortorder, ramsortorder, shellsortorder),
    na.last=c(FALSE, TRUE),
    decreasing=c(FALSE, TRUE),
    duplicates=c(FALSE, TRUE),
    n_missing=0:2
    )
  )
})

