test_that("integer64 coercion to/from other types work", {
  # from integer64
  expect_identical(as.logical(as.integer64(0:1)), c(FALSE, TRUE))
  expect_identical(as.integer(as.integer64(1:10)), 1:10)
  expect_identical(as.character(as.integer64(1:10)), as.character(1:10))
  expect_identical(as.double(as.integer64(1:10)), as.double(1:10))
  expect_identical(as.numeric(as.integer64(1:10)), as.numeric(1:10))

  # to integer64
  expect_identical(as.integer64(TRUE), as.integer64(1L))
  expect_identical(as.integer64(as.character(1:10)), as.integer64(1:10))
  expect_identical(as.integer64(as.double(1:10)), as.integer64(1:10))
  expect_identical(as.integer64(NULL), as.integer64())
  x = as.integer64(1:10)
  expect_identical(as.integer64(x), x)

  # S4 version
  expect_identical(methods::as(as.character(1:10), "integer64"), as.integer64(1:10))
  expect_identical(methods::as(as.integer64(1:10), "character"), as.character(1:10))

  # now for NA
  expect_identical(as.logical(NA_integer64_), NA)
  expect_identical(as.integer(NA_integer64_), NA_integer_)
  expect_identical(as.double(NA_integer64_), NA_real_)
  expect_identical(as.character(NA_integer64_), NA_character_)
  expect_identical(as.integer64(NA), NA_integer64_)
  expect_identical(as.integer64(NA_integer_), NA_integer64_)
  expect_identical(as.integer64(NA_real_), NA_integer64_)
  expect_identical(as.integer64(NA_character_), NA_integer64_)
})

test_that("S3 class basics work", {
  x = as.integer64(1:10)
  expect_s3_class(x, "integer64")
  expect_true(is.integer64(x))

  length(x) = 11L
  expect_length(x, 11L)
  expect_identical(x[11L], as.integer64(0L))
})

test_that("indexing works", {
  x = as.integer64(1:10)

  x[1.0] = 2.0
  x[2L] = 3L
  expect_identical(x, as.integer64(c(2:3, 3:10)))

  x[[1.0]] = 3.0
  x[[2L]] = 4L
  expect_identical(x, as.integer64(c(3:4, 3:10)))

  expect_identical(x[3L], as.integer64(3L))
  expect_identical(x[[4L]], as.integer64(4L))
})

test_that("arithmetic & basic math works", {
  x = as.integer64(1:10)
  y = as.integer64(10:1)

  expect_identical(x + y, as.integer64(rep(11L, 10L)))
  expect_identical(y - x, as.integer64(seq(9L, -9L, by=-2L)))
  expect_identical(x * y, as.integer64(c(10L, 18L, 24L, 28L, 30L, 30L, 28L, 24L, 18L, 10L)))
  # output is double even though it fits in integer [and integer64]
  expect_identical(x[seq(2L, 10L, by=2L)] / 2L, as.double(1:5))
  expect_identical(x ^ 2L, as.integer64((1:10)^2L))

  expect_identical(x %/% 2L, as.integer64(c(0L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L)))
  expect_identical(x %% 2L, as.integer64(rep_len(c(1L, 0L), 10L)))

  expect_identical(sign(x - 6L), as.integer64(rep(c(-1L, 0L, 1L), c(5L, 1L, 4L))))
  expect_identical(abs(x - 6.0), as.integer64(c(5:0, 1:4)))

  expect_identical(sqrt(as.integer64(c(0L, 1L, 4L, 9L))), as.numeric(0:3))
  expect_identical(log(x), log(as.numeric(x)))
  expect_identical(log(as.integer64(c(1L, 2L, 4L, 8L)), base=2L), as.numeric(0:3))
  expect_identical(log2(as.integer64(c(1L, 2L, 4L, 8L))), as.numeric(0:3))
  # TODO(#48): Improve the numerical precision here.
  expect_identical(log10(as.integer64(c(1L, 10L, 100L, 1000L))), as.numeric(0:3), tolerance=1e-7)

  expect_identical(trunc(x), x)
  expect_identical(floor(x), x)
  expect_identical(ceiling(x), x)
  expect_identical(signif(x), x)
  expect_identical(round(x), x)

  expect_identical(round(x, -1L), as.integer64(rep(c(0L, 10L), each=5L)))

  expect_identical(sum(x), as.integer64(55L))
  expect_identical(prod(x), as.integer64(factorial(10L)))
  expect_identical(min(x), x[1L])
  expect_identical(max(x), x[10L])
  expect_identical(diff(x), as.integer64(rep(1L, 9L)))
})

test_that("display methods work", {
  x = as.integer64(1:3)
  expect_identical(format(x), as.character(1:3))
  expect_output(print(x), "integer64.*\\s*1\\s*2\\s*3")
  expect_output(str(x), "integer64 [1:3] 1 2 3", fixed=TRUE)
})

test_that("vector builders of integer64 work", {
  x = as.integer64(1:3)
  expect_identical(c(x, FALSE), as.integer64(c(1:3, 0L)))
  expect_identical(c(x, 4:6), as.integer64(1:6))
  expect_identical(c(x, 4.0, 5.0, 6.0), as.integer64(1:6))
  expect_identical(c(x, as.integer64(4:6)), as.integer64(1:6))

  # TODO(#45): use matrix() directly
  matrix64 <- function(x, nrow=1L, ncol=1L, byrow=FALSE) {
    x = as.integer64(x)
    if (byrow) {
      dim(x) = c(ncol, nrow)
      t(x)
    } else {
      dim(x) = c(nrow, ncol)
      x
    }
  }
  expect_identical(cbind(x, FALSE), matrix64(c(1:3, 0L, 0L, 0L), nrow=3L, ncol=2L))
  expect_identical(cbind(x, 4:6), matrix64(1:6, nrow=3L, ncol=2L))
  expect_identical(cbind(x, 0.0), matrix64(c(1:3, 0L, 0L, 0L), nrow=3L, ncol=2L))
  expect_identical(cbind(x, as.integer64(4:6)), matrix64(1:6, nrow=3L, ncol=2L))

  expect_identical(rbind(x, FALSE), matrix64(c(1:3, 0L, 0L, 0L), nrow=2L, ncol=3L, byrow=TRUE))
  expect_identical(rbind(x, 4:6), matrix64(1:6, nrow=2L, ncol=3L, byrow=TRUE))
  expect_identical(rbind(x, 0.0), matrix64(c(1:3, 0L, 0L, 0L), nrow=2L, ncol=3L, byrow=TRUE))
  expect_identical(rbind(x, as.integer64(4:6)), matrix64(1:6, nrow=2L, ncol=3L, byrow=TRUE))

  expect_identical(rep(x, 2L), c(x, x))
  expect_identical(rep(x, each=2L), as.integer64(c(1L, 1L, 2L, 2L, 3L, 3L)))

  expect_identical(x[1L]:x[3L], x)
  expect_identical(x[3L]:x[1L], x[3:1]) # rev() a separate method

  expect_identical(seq(x[1L], x[3L], by=1L), x)
  expect_identical(seq(x[1L], x[3L], by=x[1L]), x)
  expect_identical(seq(x[1L], to=10L, by=1L), as.integer64(1:10))
  expect_identical(seq(x[1L], to=11L, by=2L), as.integer64(c(1L, 3L, 5L, 7L, 9L, 11L)))
  # TODO(#47): More tests when the behavior is corrected.
})
