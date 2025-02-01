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

test_that("ramsort method for integer64 works", {
  x = as.integer64(1:10)

  for (na.last in c(FALSE, TRUE)) {
    for (decreasing in c(FALSE, TRUE)) {
      for (n_missing in 0:2) {
        na_entries = rep(NA_integer64_, n_missing)
        y = sample(c(x, na_entries))
        expect_identical(ramsort(y, decreasing=decreasing, na.last=na.last), n_missing)
        # TODO(#154): Drop explicit 'else' branches
        expected_value = c(
          if (na.last) integer64() else na_entries,
          if (decreasing) rev(x) else x,
          if (na.last) na_entries else integer64()
        )
        expect_identical(y, expected_value,
          info=sprintf(
            "(na.last, decreasing, n_missing)=(%s, %s, %d)",
            na.last, decreasing, n_missing
          )
        )
      }
    }
  }
})

test_that("shellsort method for integer64 works", {
  x = as.integer64(1:10)

  for (na.last in c(FALSE, TRUE)) {
    for (decreasing in c(FALSE, TRUE)) {
      for (n_missing in 0:2) {
        na_entries = rep(NA_integer64_, n_missing)
        y = sample(c(x, na_entries))
        expect_identical(shellsort(y, decreasing=decreasing, na.last=na.last), n_missing)
        # TODO(#154): Drop explicit 'else' branches
        expected_value = c(
          if (na.last) integer64() else na_entries,
          if (decreasing) rev(x) else x,
          if (na.last) na_entries else integer64()
        )
        expect_identical(y, expected_value,
          info=sprintf(
            "(na.last, decreasing, n_missing)=(%s, %s, %d)",
            na.last, decreasing, n_missing
          )
        )
      }
    }
  }
})

test_that("mergesort method for integer64 works", {
  x = as.integer64(1:10)

  for (na.last in c(FALSE, TRUE)) {
    for (decreasing in c(FALSE, TRUE)) {
      for (n_missing in 0:2) {
        na_entries = rep(NA_integer64_, n_missing)
        y = sample(c(x, na_entries))
        expect_identical(mergesort(y, decreasing=decreasing, na.last=na.last), n_missing)
        # TODO(#154): Drop explicit 'else' branches
        expected_value = c(
          if (na.last) integer64() else na_entries,
          if (decreasing) rev(x) else x,
          if (na.last) na_entries else integer64()
        )
        expect_identical(y, expected_value,
          info=sprintf(
            "(na.last, decreasing, n_missing)=(%s, %s, %d)",
            na.last, decreasing, n_missing
          )
        )
      }
    }
  }
})

test_that("radixsort method for integer64 works", {
  x = as.integer64(1:10)

  for (na.last in c(FALSE, TRUE)) {
    for (decreasing in c(FALSE, TRUE)) {
      for (n_missing in 0:2) {
        na_entries = rep(NA_integer64_, n_missing)
        y = sample(c(x, na_entries))
        expect_identical(radixsort(y, decreasing=decreasing, na.last=na.last), n_missing)
        # TODO(#154): Drop explicit 'else' branches
        expected_value = c(
          if (na.last) integer64() else na_entries,
          if (decreasing) rev(x) else x,
          if (na.last) na_entries else integer64()
        )
        expect_identical(y, expected_value,
          info=sprintf(
            "(na.last, decreasing, n_missing)=(%s, %s, %d)",
            na.last, decreasing, n_missing
          )
        )
      }
    }
  }
})

test_that("quicksort method for integer64 works", {
  x = as.integer64(1:10)

  for (na.last in c(FALSE, TRUE)) {
    for (decreasing in c(FALSE, TRUE)) {
      for (n_missing in 0:2) {
        na_entries = rep(NA_integer64_, n_missing)
        y = sample(c(x, na_entries))
        expect_identical(quicksort(y, decreasing=decreasing, na.last=na.last), n_missing)
        # TODO(#154): Drop explicit 'else' branches
        expected_value = c(
          if (na.last) integer64() else na_entries,
          if (decreasing) rev(x) else x,
          if (na.last) na_entries else integer64()
        )
        expect_identical(y, expected_value,
          info=sprintf(
            "(na.last, decreasing, n_missing)=(%s, %s, %d)",
            na.last, decreasing, n_missing
          )
        )
      }
    }
  }
})
