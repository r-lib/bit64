test_that("integer64 coercion to/from other types work for atomic vectors", {
  # from integer64
  i32 = 1:10
  i64 = as.integer64(i32)
  expect_identical(as.logical(as.integer64(0:1)), c(FALSE, TRUE))
  expect_identical(as.integer(i64), i32)
  expect_identical(as.character(i64), as.character(i32))
  expect_identical(as.double(i64), as.double(i32))
  expect_identical(as.numeric(i64), as.numeric(i32))

  # to integer64
  expect_identical(as.integer64(TRUE), as.integer64(1L))
  expect_identical(as.integer64(as.character(1:10)), i64)
  expect_identical(as.integer64(as.double(1:10)), i64)
  expect_identical(as.integer64(as.complex(1:10)), i64)
  expect_identical(as.integer64(as.raw(1:10)), i64)
  expect_identical(as.integer64(NULL), as.integer64())
  expect_identical(as.integer64(i64), i64)
})

test_that("integer64 coercion to/from other types works via S4 coercion", {
  expect_identical(methods::as(as.character(1:10), "integer64"), as.integer64(1:10))
  expect_identical(methods::as(as.factor(11:20), "integer64"), as.integer64(1:10))
  expect_identical(methods::as(as.ordered(11:20), "integer64"), as.integer64(1:10))
  expect_warning(
    expect_identical(methods::as(as.complex(1:10) + 1.0i, "integer64"), as.integer64(1:10)),
    "imaginary parts discarded in coercion"
  )
  expect_identical(methods::as(as.numeric(1:10), "integer64"), as.integer64(1:10))
  expect_identical(methods::as(as.integer(1:10), "integer64"), as.integer64(1:10))
  expect_identical(methods::as(as.raw(1:10), "integer64"), as.integer64(1:10))
  expect_identical(methods::as(as.logical(0:2), "integer64"), as.integer64(c(0L, 1L, 1L)))
  expect_identical(methods::as(as.integer64(1:10), "character"), as.character(1:10))
  expect_identical(methods::as(as.integer64(1:10), "factor"), as.factor(1:10))
  expect_identical(methods::as(as.integer64(1:10), "ordered"), as.ordered(1:10))
  expect_identical(methods::as(as.integer64(1:10), "numeric"), as.numeric(1:10))
  expect_identical(methods::as(as.integer64(1:10), "integer"), as.integer(1:10))
  expect_identical(methods::as(as.integer64(1:10), "logical"), as.logical(1:10))
})

test_that("integer64 coercion to/from other types works for NA", {
  expect_identical(as.logical(NA_integer64_), NA)
  expect_identical(as.integer(NA_integer64_), NA_integer_)
  expect_identical(as.double(NA_integer64_), NA_real_)
  expect_identical(as.character(NA_integer64_), NA_character_)
  expect_identical(as.integer64(NA), NA_integer64_)
  expect_identical(as.integer64(NA_integer_), NA_integer64_)
  expect_identical(as.integer64(NA_real_), NA_integer64_)
  expect_identical(as.integer64(NA_character_), NA_integer64_)
})

test_that("integer64 coercion to/from factor types works", {
  i32 = 1:10
  i64 = as.integer64(i32)
  expect_identical(as.factor(i64), as.factor(i32))
  expect_identical(as.ordered(i64), as.ordered(i32))
  expect_identical(as.integer64(as.factor(11:20)), as.integer64(1:10))
  expect_identical(as.integer64(as.ordered(11:20)), as.integer64(1:10))
})

test_that("integer64 coercion to/from time types works", {
  posixct = Sys.time()
  posixct = c(posixct, posixct + 100)
  posix_delta = difftime(posixct + 1000, posixct)

  expect_identical(
    as.integer64(posix_delta), 
    as.integer64(as.integer(posix_delta))
  )
  # as.integer.difftime does not work with `units`
  expect_identical(
    as.integer64(posix_delta, units="secs"), 
    as.integer64(as.numeric(posix_delta, units="secs"))
  )
  expect_identical(
    as.integer64(posix_delta, units="mins"), 
    as.integer64(as.numeric(posix_delta, units="mins"))
  )

  expect_identical(as.integer64(posixct), as.integer64(as.integer(posixct)))
  # as.integer.POSIXlt does not work properly
  expect_identical(
    as.integer64(as.POSIXlt(posixct)),
    as.integer64(as.numeric(as.POSIXlt(posixct)))
  )
  expect_identical(
    as.integer64(as.Date(posixct)),
    as.integer64(as.integer(as.Date(posixct)))
  )

  # S4 version
  expect_identical(methods::as(posix_delta, "integer64"), as.integer64(posix_delta))
  expect_identical(methods::as(posixct, "integer64"), as.integer64(posixct))
  expect_identical(methods::as(as.POSIXlt(posixct), "integer64"), as.integer64(as.POSIXlt(posixct)))
  expect_identical(methods::as(as.Date(posixct), "integer64"), as.integer64(as.Date(posixct)))
})

test_that("integer64 coercion from generic object works", {
  x = structure(
    as.integer64(1:10),
    class=c("otherClass", "integer64"),
    dim=c(2L, 5L),
    dimnames=list(LETTERS[1:2], letters[1:5]),
    otherAttr="some other attribute"
  )
  expect_identical(as.integer64(x), as.integer64(1:10))
})

test_that("integer64 coercion to/from other types work for R >=4.0.0", {
  skip_unless_r(">= 4.0.0")
  # from integer64
  i32 = 1:10
  i64 = as.integer64(i32)

  expect_identical(as.complex(i64), as.complex(i32))
  expect_identical(as.raw(i64), as.raw(i32))
  expect_identical(methods::as(as.integer64(1:10), "complex"), as.complex(1:10))

  expect_identical(as.Date(i64), as.Date(as.numeric(i32)))
  expect_identical(as.Date(i64, origin=10), as.Date(as.numeric(i32), origin=10))
  expect_identical(as.POSIXct(i64), as.POSIXct(as.numeric(i32)))
  expect_identical(as.POSIXct(i64, origin=10), as.POSIXct(as.numeric(i32), origin=10))
  expect_identical(as.POSIXct(i64, tz="UTC", origin=10), as.POSIXct(as.numeric(i32), tz="UTC", origin=10))
  expect_identical(as.POSIXct(i64, tz="CET", origin=10), as.POSIXct(as.numeric(i32), tz="CET", origin=10))
  expect_identical(as.POSIXlt(i64), as.POSIXlt(i32))
  expect_identical(as.POSIXlt(i64, origin=10), as.POSIXlt(i32, origin=10))
  expect_identical(as.POSIXlt(i64, tz="UTC", origin=10), as.POSIXlt(i32, tz="UTC", origin=10))
  expect_identical(as.POSIXlt(i64, tz="CET", origin=10), as.POSIXlt(i32, tz="CET", origin=10))
  expect_error(as.difftime(i32), "need explicit units for numeric conversion", fixed=TRUE)
  expect_error(as.difftime(i64), "need explicit units for numeric conversion", fixed=TRUE)
  expect_identical(as.difftime(i64, units="secs"), as.difftime(i32, units="secs"))
  
  # S4 version
  expect_identical(methods::as(as.integer64(1:10), "raw"), as.raw(1:10))
  expect_identical(methods::as(as.integer64(1:10), "difftime"), as.difftime(1:10, units="secs"))
  expect_identical(methods::as(as.integer64(1:10), "POSIXct"), as.POSIXct(as.numeric(1:10)))
  expect_identical(methods::as(as.integer64(1:10), "POSIXlt"), as.POSIXlt(1:10))
  expect_identical(methods::as(as.integer64(1:10), "Date"), as.Date(as.numeric(1:10)))

})

test_that("integer64 coercion from character works for numbers near +/- 2^63", {
  expect_identical(
    as.character(as.integer64(c("-9223372036854775807", "-9223372036854775806", "9223372036854775806", "9223372036854775807", ""))),
    c("-9223372036854775807", "-9223372036854775806", "9223372036854775806", "9223372036854775807", NA)
  )
})

test_that("Conversion from hex is supported", {
  expect_identical(
    as.integer64(c("0x1", "0xF", "0x7FFFFFFFFFFFFFFF", "-0x1", "-0xF", "-0x7FFFFFFFFFFFFFFF", "0x0")),
    as.integer64(c(1, 15, "9223372036854775807", -1, -15, "-9223372036854775807", 0))
  )
})

test_that("as.integer64 can trim whitespace", {
  expect_identical(as.integer64("  123   "), as.integer64("123"))
  expect_identical(as.integer64("  123"), as.integer64("123"))
  expect_identical(as.integer64("123   "), as.integer64("123"))
  expect_identical(as.integer64(c("  1", "2  ", " 3 ")), as.integer64(1:3))
  expect_identical(as.integer64("  0xAAA   "), as.integer64("0xAAA"))
  expect_identical(as.integer64("  0xBBB"), as.integer64("0xBBB"))
  expect_identical(as.integer64("0xCCC   "), as.integer64("0xCCC"))
  expect_identical(as.integer64(c("  0x1", "0x2  ", " 0x3 ")), as.integer64(c("0x1", "0x2", "0x3")))
})

with_parameters_test_that(
  "base-10 edge cases return missing",
  expect_warning(
    expect_identical(as.integer64(string), rep(NA_integer64_, length(string))),
    "NAs introduced by coercion to integer64 range", fixed=TRUE
  ),
  string = list(
    strrep("9", 63L),
    "9223372036854775808",
    "-9223372036854775808",
    "999x",
    "-999x",
    c("9223372036854775809", "-9223372036854775809") # vector case from #175
  )
)

with_parameters_test_that(
  "hex edge cases return missing",
  expect_warning(
    expect_identical(as.integer64(string), NA_integer64_),
    "NAs introduced by coercion to integer64 range", fixed=TRUE
  ),
  string = c(
    "-0x8000000000000000",
    "0x8000000000000000",
    "0x",
    "-0x",
    "0xx",
    " 0x",
    "0x ",
    "0x0Z"
  )
)

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

  names(x) = letters[1:10]
  expect_identical(x[c("b", "c")], x[2:3])
  expect_identical(x[["d"]], x[[4L]])
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
  expect_identical(-x, as.integer64(-(1:10)))

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

  # regression snuck through, caught by #149
  expect_identical(as.integer64(1L) * 1:5, as.integer64(1:5))
  expect_identical(1:5 * as.integer64(1L), as.integer64(1:5))
})

test_that("basic statistics work", {
  x = as.integer64(1:10)

  expect_identical(sum(x), as.integer64(55L))
  expect_identical(sum(x, x), as.integer64(110L))
  expect_identical(prod(x), as.integer64(factorial(10L)))
  expect_identical(prod(x[1:5], x[6:10]), as.integer64(factorial(10L)))
  expect_identical(diff(x), as.integer64(rep(1L, 9L)))

  expect_identical(cummin(x), as.integer64(rep(1L, 10L)))
  expect_identical(cummax(x), x)
  expect_identical(cumsum(x), as.integer64(choose(2:11, 2L)))
  expect_identical(cumprod(x), as.integer64(factorial(1:10)))
})

test_that("min, max, and range work", {
  x = as.integer64(1:10)

  expect_identical(min(x), x[1L])
  expect_identical(max(x), x[10L])
  expect_identical(range(x), x[c(1L, 10L)])

  expect_identical(min(x, as.integer64(0L), as.integer64(11L)), as.integer64(0L))
  expect_identical(max(x, as.integer64(0L), as.integer64(11L)), as.integer64(11L))
  expect_identical(range(x, as.integer64(0L), as.integer64(11L)), as.integer64(c(0L, 11L)))

  expect_identical(range(x, NA_integer64_, finite=TRUE), x[c(1L, 10L)])

  expect_identical(min(x, integer64()), x[1L])
  expect_identical(max(x, integer64()), x[10L])
  expect_identical(range(x, integer64()), x[c(1L, 10L)])

  expect_identical(min(x, integer64(), na.rm=TRUE), x[1L])
  expect_identical(max(x, integer64(), na.rm=TRUE), x[10L])
  expect_identical(range(x, integer64(), na.rm=TRUE), x[c(1L, 10L)])
})

test_that("min, max, and range work in edge cases", {
  expect_warning(
    expect_identical(min(integer64()), lim.integer64()[2L]),
    "no non-NA value"
  )
  expect_warning(
    expect_identical(max(integer64()), lim.integer64()[1L]),
    "no non-NA value"
  )
  expect_warning(
    expect_identical(range(integer64()), lim.integer64()[2:1]),
    "no non-NA value"
  )

  expect_warning(
    expect_identical(min(integer64(), na.rm=TRUE), lim.integer64()[2L]),
    "no non-NA value"
  )
  expect_warning(
    expect_identical(max(integer64(), na.rm=TRUE), lim.integer64()[1L]),
    "no non-NA value"
  )
  expect_warning(
    expect_identical(range(integer64(), na.rm=TRUE), lim.integer64()[2:1]),
    "no non-NA value"
  )

  expect_no_warning(
    expect_identical(min(lim.integer64()[2L]), lim.integer64()[2L])
  )
  expect_no_warning(
    expect_identical(max(lim.integer64()[1L]), lim.integer64()[1L])
  )
  expect_no_warning(
    expect_identical(range(lim.integer64()), lim.integer64())
  )

  expect_no_warning(
    expect_identical(min(lim.integer64()[2L], NA_integer64_, na.rm=TRUE), lim.integer64()[2L])
  )
  expect_no_warning(
    expect_identical(max(lim.integer64()[1L], NA_integer64_, na.rm=TRUE), lim.integer64()[1L])
  )
  expect_no_warning(
    expect_identical(range(lim.integer64(), NA_integer64_, na.rm=TRUE), lim.integer64())
  )

  expect_no_warning(
    expect_identical(min(integer64(), NA_integer64_), NA_integer64_)
  )
  expect_no_warning(
    expect_identical(max(integer64(), NA_integer64_), NA_integer64_)
  )
  expect_no_warning(
    expect_identical(range(integer64(), NA_integer64_), rep(NA_integer64_, 2L))
  )

  expect_warning(
    expect_identical(min(integer64(), NA_integer64_, na.rm=TRUE), lim.integer64()[2L]),
    "no non-NA value"
  )
  expect_warning(
    expect_identical(max(integer64(), NA_integer64_, na.rm=TRUE), lim.integer64()[1L]),
    "no non-NA value"
  )
  expect_warning(
    expect_identical(range(integer64(), NA_integer64_, na.rm=TRUE), lim.integer64()[2:1]),
    "no non-NA value"
  )

  expect_no_warning(
    expect_identical(min(integer64(1), NA_integer64_), NA_integer64_)
  )
  expect_no_warning(
    expect_identical(max(integer64(1), NA_integer64_), NA_integer64_)
  )
  expect_no_warning(
    expect_identical(range(integer64(1), NA_integer64_), rep(NA_integer64_, 2L))
  )

  expect_no_warning(
    expect_identical(min(integer64(1), NA_integer64_, na.rm=TRUE), as.integer64(0L))
  )
  expect_no_warning(
    expect_identical(max(integer64(1), NA_integer64_, na.rm=TRUE), as.integer64(0L))
  )
  expect_no_warning(
    expect_identical(range(integer64(1), NA_integer64_, na.rm=TRUE), rep(as.integer64(0L), 2L))
  )

  expect_no_warning(
    expect_identical(min(integer64(1), integer64(), NA_integer64_), NA_integer64_)
  )
  expect_no_warning(
    expect_identical(max(integer64(1), integer64(), NA_integer64_), NA_integer64_)
  )
  expect_no_warning(
    expect_identical(range(integer64(1), integer64(), NA_integer64_), rep(NA_integer64_, 2L))
  )

  expect_no_warning(
    expect_identical(min(integer64(1), integer64(), NA_integer64_, na.rm=TRUE), as.integer64(0L))
  )
  expect_no_warning(
    expect_identical(max(integer64(1), integer64(), NA_integer64_, na.rm=TRUE), as.integer64(0L))
  )
  expect_no_warning(
    expect_identical(range(integer64(1), integer64(), NA_integer64_, na.rm=TRUE), rep(as.integer64(0L), 2L))
  )

  expect_no_warning(
    expect_identical(min(integer64(1), integer64()), as.integer64(0L))
  )
  expect_no_warning(
    expect_identical(max(integer64(1), integer64()), as.integer64(0L))
  )
  expect_no_warning(
    expect_identical(range(integer64(1), integer64()), as.integer64(c(0L, 0L)))
  )

  expect_no_warning(
    expect_identical(min(integer64(1), integer64(), na.rm=TRUE), as.integer64(0L))
  )
  expect_no_warning(
    expect_identical(max(integer64(1), integer64(), na.rm=TRUE), as.integer64(0L))
  )
  expect_no_warning(
    expect_identical(range(integer64(1), integer64(), na.rm=TRUE), rep(as.integer64(0L), 2L))
  )
})

test_that("display methods work", {
  x = as.integer64(1:3)
  expect_identical(format(x), as.character(1:3))
  expect_output(print(x), "integer64.*\\s*1\\s*2\\s*3")
  expect_output(print(x[0L]), "integer64(0)", fixed=TRUE)
  expect_output(str(x), "integer64 [1:3] 1 2 3", fixed=TRUE)
})

test_that("vector builders of integer64 work", {
  x = as.integer64(1:3)
  expect_identical(c(x, FALSE), as.integer64(c(1:3, 0L)))
  expect_identical(c(x, 4:6), as.integer64(1:6))
  expect_identical(c(x, 4.0, 5.0, 6.0), as.integer64(1:6))
  expect_identical(c(x, as.integer64(4:6)), as.integer64(1:6))

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
})

with_parameters_test_that(
  "seq method works analogously to integer: 1 argument (except along.with);",
  {
    n64 = as.integer64(n)
    expect_identical(seq(n64), as.integer64(seq(n)))
    expect_identical(seq(from=n64), as.integer64(seq(from=n)))
    expect_identical(seq(to=n64), as.integer64(seq(to=n)))
    expect_identical(seq(by=n64), as.integer64(seq(by=n)))
    if (n < 0L) {
      expect_error(seq(length.out=n64), "'length.out' must be a non-negative number")
    } else {
      expect_identical(seq(length.out=n64), as.integer64(seq(length.out=n)))
    }
  },
  n = c(0L, 5L, -1L)
)

with_parameters_test_that(
  "seq method works analogously to integer: 2 arguments (except along.with)",
  {
    # be sure to coerce (truncate) consistently across actual/expected
    n1_32 = as.integer(n1)
    n2_32 = as.integer(n2)

    n1_64 = as.integer64(n1)
    n2_64 = as.integer64(n2)

    # TODO(#211): restore parity to seq() here
    if (n2 %% 1L == 0L) expect_identical(seq(n1_64, n2), as.integer64(seq(n1_32, n2)))
    expect_identical(seq(n1_64, n2_64), as.integer64(seq(n1_32, n2_32)))


    if (n2 == 0L) {
      err_msg = "invalid '(to - from)/by'"
      expect_error(seq(n1_64, by=n2), err_msg, fixed=TRUE)
      expect_error(seq(to=n1_64, by=n2), err_msg, fixed=TRUE)
    } else if (sign(1L - n1) == sign(n2)) {
      if (n2 %% 1L == 0L) expect_identical(seq(n1_64, by=n2), as.integer64(seq(n1, by=n2)))
      expect_identical(seq(n1_64, by=n2_64), as.integer64(seq(n1, by=n2_32)))

      expect_error(seq(to=n1_64, by=n2), "wrong sign in 'by' argument", fixed=TRUE)
    } else {
      expect_error(seq(n1_64, by=n2), "wrong sign in 'by' argument", fixed=TRUE)

      # TODO(#211): restore parity to seq() here
      if (n2 %% 1L == 0L) expect_identical(seq(to=n1_64, by=n2), as.integer64(seq(to=n1, by=n2)))
      expect_identical(seq(to=n1_64, by=n2_64), as.integer64(seq(to=n1, by=n2_32)))
    }

    if (n2 >= 0L) {
      expect_identical(seq(n1_64, length.out=n2), as.integer64(seq(n1_32, length.out=n2)))
      expect_identical(seq(n1_64, length.out=n2_64), as.integer64(seq(n1_32, length.out=n2_32)))

      expect_identical(seq(to=n1_64, length.out=n2), as.integer64(seq(to=n1_32, length.out=n2)))
      expect_identical(seq(to=n1_64, length.out=n2_64), as.integer64(seq(to=n1_32, length.out=n2_32)))

      expect_identical(seq(by=n1_64, length.out=n2), as.integer64(seq(by=n1_32, length.out=n2)))
      expect_identical(seq(by=n1_64, length.out=n2_64), as.integer64(seq(by=n1_32, length.out=n2_32)))
    } else {
      err_msg = "'length.out' must be a non-negative number"
      expect_error(seq(n1_64, length.out=n2), err_msg, fixed=TRUE)
      expect_error(seq(to=n1_64, length.out=n2), err_msg, fixed=TRUE)
      expect_error(seq(by=n1_64, length.out=n2), err_msg, fixed=TRUE)
    }
  },
  .cases = expand.grid(n1=c(0L, 5L, -1L), n2=c(0.0, 5.0, -1.0, 1.5))
)

with_parameters_test_that(
  "seq method works analogously to integer: 3 arguments (except along.with)",
  {
    # be sure to coerce (truncate) consistently across actual/expected
    n1_32 = as.integer(n1)
    n2_32 = as.integer(n2)
    n3_32 = as.integer(n3)

    n1_64 = as.integer64(n1)
    n2_64 = as.integer64(n2)
    n3_64 = as.integer64(n3)

    # TODO(#211): restore parity to seq() here
    if (n2 == n1 || sign(n2 - n1) == sign(n3)) {
      # TODO(#211): restore parity to seq() here
      if (n2 %% 1L == 0L && n3 %% 1L == 0L) {
        expect_identical(seq(n1_64, n2, by=n3), as.integer64(seq(n1_32, n2, by=n3)))
        expect_identical(seq(n1_64, n2_64, by=n3), as.integer64(seq(n1_32, n2_32, by=n3)))
        expect_identical(seq(n1_64, n2, by=n3_64), as.integer64(seq(n1_32, n2, by=n3_32)))
        expect_identical(seq(n1_64, n2_64, by=n3_64), as.integer64(seq(n1_32, n2_32, by=n3_32)))
      }
    } else {
      err_msg <- if (n3 == 0L) "invalid '(to - from)/by'" else "wrong sign in 'by' argument"
      expect_error(seq(n1_64, n2, by=n3), err_msg, fixed=TRUE)
    }

    if (n3 < 0L) {
      err_msg = "'length.out' must be a non-negative"
      expect_error(seq(n1_64, n2, length.out=n3), err_msg, fixed=TRUE)
      expect_error(seq(n1_64, by=n2, length.out=n3), err_msg, fixed=TRUE)
      expect_error(seq(to=n1_64, by=n2, length.out=n3), err_msg, fixed=TRUE)
    } else {
      # TODO(#211): restore parity to seq() here
      if (((n2 - n1) / (n3 - 1L)) %% 1L == 0L) {
        expect_identical(seq(n1_64, n2, length.out=n3), as.integer64(seq(n1_32, n2, length.out=n3)))
        expect_identical(seq(n1_64, n2_64, length.out=n3), as.integer64(seq(n1_32, n2_32, length.out=n3)))
        expect_identical(seq(n1_64, n2, length.out=n3_64), as.integer64(seq(n1_32, n2, length.out=n3_32)))
        expect_identical(seq(n1_64, n2_64, length.out=n3_64), as.integer64(seq(n1_32, n2_32, length.out=n3_32)))
      }

      # TODO(#211): restore parity to seq() here
      if (n2 %% 1L == 0L) {
        expect_identical(seq(n1_64, by=n2, length.out=n3), as.integer64(seq(n1_32, by=n2, length.out=n3)))
        expect_identical(seq(n1_64, by=n2_64, length.out=n3), as.integer64(seq(n1_32, by=n2_32, length.out=n3)))
        expect_identical(seq(n1_64, by=n2, length.out=n3_64), as.integer64(seq(n1_32, by=n2, length.out=n3_32)))
        expect_identical(seq(n1_64, by=n2_64, length.out=n3_64), as.integer64(seq(n1_32, by=n2_32, length.out=n3_32)))

        expect_identical(seq(to=n1_64, by=n2, length.out=n3), as.integer64(seq(to=n1_32, by=n2, length.out=n3)))
        expect_identical(seq(to=n1_64, by=n2_64, length.out=n3), as.integer64(seq(to=n1_32, by=n2_32, length.out=n3)))
        expect_identical(seq(to=n1_64, by=n2, length.out=n3_64), as.integer64(seq(to=n1_32, by=n2, length.out=n3_32)))
        expect_identical(seq(to=n1_64, by=n2_64, length.out=n3_64), as.integer64(seq(to=n1_32, by=n2_32, length.out=n3_32)))
      }
    }
  },
  .cases = expand.grid(n1=c(0L, 5L, -1L), n2=c(0.0, 5.0, -1.0, 1.5), n3=c(0.0, 5.0, -1.0, 1.5))
)

test_that("seq method works analogously to integer: 4 arguments", {
  n = as.integer64(5L)

  expect_error(seq(n, n, by=n, length.out=n), "too many arguments")
  expect_error(seq(n, n, by=n, along.with=n), "too many arguments")
})

test_that("seq() works back-compatibly w.r.t. mixed integer+double inputs", {
  one = as.integer64(1L)
  expect_identical(seq(one, 10L, by=1.5), as.integer64(1:10))
  expect_identical(seq(to=one, from=10L, by=-1.5), as.integer64(10:1))

  expect_identical(seq(one, 10L, by=10.0/3.0), as.integer64(c(1L, 4L, 7L, 10L)))
  expect_identical(seq(to=one, from=10L, by=-10.0/3.0), as.integer64(c(10L, 7L, 4L, 1L)))

  expect_error(seq(one, 10L, by=0.1), "invalid '(to - from)/by'", fixed=TRUE)
  expect_error(seq(to=one, from=10L, by=-0.1), "invalid '(to - from)/by'", fixed=TRUE)

  expect_identical(seq(one, 2.5), as.integer64(1:2))
  expect_identical(seq(to=one, from=2.5), as.integer64(2:1))

  expect_identical(seq(one, 5.5, by=1.5), as.integer64(1:5))
  expect_identical(seq(to=one, from=5.5, by=-1.5), as.integer64(5:1))
})

# These tests were previously kept as tests under \examples{\dontshow{...}}.
#   Converted to "proper" unit tests for clarity, after making them more
#   canonical within {testthat}, e.g. better capturing expected warnings,
#   changing stopifnot(identical(...)) to expect_identical(...).
local({
  i <- -999:999
  with_parameters_test_that(
    "Old \\dontshow{} tests in ?format.integer64 continue working",
    {
      r <- as.integer64(round(as.integer(i), s))
      r64 <- round(as.integer64(i), s)
      expect_identical(r, r64)
    },
    s = -3:3,
    .interpret_glue = FALSE
  )
})

test_that("Old \\dontshow{} tests in ?extract.replace.integer64 continue working", {
  r <- c(runif64(1000L, lim.integer64()[1L], lim.integer64()[2L]), NA, -2:2)
  expect_identical(r, as.integer64(as.bitstring(r)))
})

test_that("empty inputs give empty outputs for arithmetic", {
  x = integer64(1L)
  empty = integer64(0L)

  expect_identical(x+empty, integer64())
  expect_identical(empty+x, integer64())

  expect_identical(x-empty, integer64())
  expect_identical(empty-x, integer64())

  expect_identical(+empty, integer64())
  expect_identical(-empty, integer64())

  expect_identical(x*empty, integer64())
  expect_identical(empty*x, integer64())

  expect_identical(x/empty, double())
  expect_identical(empty/x, double())

  expect_identical(x^empty, integer64())
  expect_identical(empty^x, integer64())

  expect_identical(x %/% empty, integer64())
  expect_identical(empty %/% x, integer64())

  expect_identical(x%%empty, integer64())
  expect_identical(empty%%x, integer64())

  expect_identical(log(x, base=empty), double())
  expect_identical(log(empty, base=x), double())
  expect_identical(
    log(`attr<-`(empty, "asdf", "jkl")),
    `attr<-`(double(), "asdf", "jkl")
  )

  expect_identical(x==empty, logical())
  expect_identical(empty==x, logical())

  expect_identical(x!=empty, logical())
  expect_identical(empty!=x, logical())

  expect_identical(x>=empty, logical())
  expect_identical(empty>=x, logical())

  expect_identical(x<=empty, logical())
  expect_identical(empty<=x, logical())

  expect_identical(x>empty, logical())
  expect_identical(empty>x, logical())

  expect_identical(x<empty, logical())
  expect_identical(empty<x, logical())

  expect_identical(x&empty, logical())
  expect_identical(empty&x, logical())

  expect_identical(x|empty, logical())
  expect_identical(empty|x, logical())

  expect_identical(xor(x, empty), logical())
  expect_identical(xor(empty, x), logical())
})

test_that("semantics about mixed types for multiplication are respected", {
  int = 5L
  i64 = as.integer64(2L)
  dbl = 3.5

  # default: "old" semantics, to be deprecated
  expect_identical(i64 * dbl, as.integer64(7L))
  expect_identical(dbl * i64, as.integer64(6L))
  expect_identical(i64 * int, as.integer64(10L))
  expect_identical(int * i64, as.integer64(10L))
  expect_identical(i64 * i64, as.integer64(4L))

  withr::with_options(list(integer64_semantics = "new"), {
    expect_identical(i64 * dbl, as.integer64(7L))
    expect_identical(dbl * i64, as.integer64(7L))
    expect_identical(i64 * int, as.integer64(10L))
    expect_identical(int * i64, as.integer64(10L))
    expect_identical(i64 * i64, as.integer64(4L))
  })
})

test_that("semantics about mixed types for division are respected", {
  int = 10L
  i64 = as.integer64(5L)
  dbl = 2.5

  # default: "old" semantics, to be deprecated
  expect_identical(i64 / dbl, 2.0)
  expect_identical(dbl / i64, 0.4)
  expect_identical(i64 / int, 0.5)
  expect_identical(int / i64, 2.0)
  expect_identical(i64 / i64, 1.0)

  withr::with_options(list(integer64_semantics = "new"), {
    expect_identical(i64 / dbl, 2.0)
    expect_identical(dbl / i64, 0.5)
    expect_identical(i64 / int, 0.5)
    expect_identical(int / i64, 2.0)
    expect_identical(i64 / i64, 1.0)
  })
})

test_that("all.equal.integer64 reflects changes for vector scale= from all.equal.numeric", {
  # same test as for base R, multiplied by 1000 so the inputs are all integer64
  expect_identical(
    all.equal(
      as.integer64(c(1000L, 1000L)),
      as.integer64(c(1010L, 1010L)),
      scale = c(10.0, 10.0)
    ),
    "Mean scaled difference: 1"
  )
  # same test as for base R, multiplied by 1e9
  one_e9 = as.integer64(1000000000L)
  expect_true(all.equal(
    rep(one_e9, 5L),
    one_e9 + (-1L:3), # TODO(r-lib/lintr#): no 'L'
    scale = (1:5)*one_e9
  ))
})

test_that("all.equal works", {
  x = y = as.integer64(1L)

  expect_true(all.equal(x, x))

  class(y) = c("xx", "integer64")
  expect_match(all.equal(x, y), "target is integer64, current is xx", fixed=TRUE, all=FALSE)
  expect_match(all.equal(x[0L], x[1L]), "integer64: lengths.*differ", all=FALSE)

  class(y) = "integer64"
  attr(y, "xx") = "zz"
  expect_match(all.equal(x, y), "Attributes", fixed=TRUE)
  expect_no_match(
    expect_match(all.equal(x[0L], y), "integer64: lengths.*differ", all=FALSE),
    "Lengths:", fixed = TRUE
  )

  y = NA_integer64_
  expect_match(all.equal(x, y), "'is.NA' value mismatch", fixed=TRUE)

  x = as.integer64(1000000000L)
  expect_true(all.equal(x, x+1L))
  expect_true(all.equal(x, x+1L, tolerance=1.0e9)) # forcing scale=1
  expect_match(all.equal(x, x+100L), "Mean relative difference", fixed=TRUE)
  expect_match(all.equal(x, x+1L, scale=1.0), "Mean absolute difference", fixed=TRUE)
})


test_that("anyNA method", {
  expect_identical(anyNA(as.integer64(c(1L, 1L))), anyNA(c(1L, 1L)))
  expect_identical(anyNA(as.integer64(c(1L, NA))), anyNA(c(1L, NA)))
  expect_identical(anyNA(as.integer64(c(NA, NA))), anyNA(c(NA, NA)))
  expect_identical(anyNA(integer64()), anyNA(integer()))
})


test_that("match works with zero length input", {
  x32 = 1:10
  x64 = as.integer64(1:10)
  expect_identical(match(x64, integer()), match(x32, integer()))
  expect_identical(match(x64, integer(), nomatch=NULL), match(x32, integer(), nomatch=NULL))
  expect_identical(match(x64, integer(), nomatch=integer()), match(x32, integer(), nomatch=integer()))
  expect_identical(match(x64, integer(), nomatch=10L), match(x32, integer(), nomatch=10L))
  expect_identical(match(integer(), x64), match(integer(), x32))
})


with_parameters_test_that("union works with basic R types (except double)", {
    if (getRversion() > "3.6.0" || !(type_x %in% c("POSIXct", "Date"))) {
      y = 5:10
      if (!is.na(type_x))
        x = eval(parse(text=paste0("as.", type_x, "(x)")))
      if (type_y == "integer64" && type_x %in% c(NA, "logical", "raw", "integer", "double")) {
        expected_result_x_y = as.integer64(base::union(x, y))
        expected_result_y_x = as.integer64(base::union(y, x))
      } else if (type_y == "integer64" && type_x %in% c("POSIXct", "Date")) {
        # as.POSIXct(1L) and as.Date(1L) stay internally integer
        # as.POSIXct(1) and as.Date(1) stay internally double
        y = as.double(y)
        expected_result_x_y = base::union(x, y)
        expected_result_y_x = base::union(y, x)
      } else {
        expected_result_x_y = base::union(x, y)
        expected_result_y_x = base::union(y, x)
      }
      y = as(y, type_y)
  
      expect_identical(
        union(x, y),
        expected_result_x_y
      )
      expect_identical(
        union(y, x),
        expected_result_y_x
      )
    }
  }, 
  .cases=expand.grid(x=I(list(NULL, c(1:7, 2L, 11L))), type_x=c(NA, "double", "logical", "integer", "character", "POSIXct", "Date", "complex", "factor", "ordered"), type_y=c("integer", "integer64"), stringsAsFactors=FALSE)
)

test_that("union works with special logic for double", {
  
  expect_identical(union(1.5:7, 5:10), base::union(1.5:7, 5:10))
  expect_identical(union(1.5:7, as.integer64(5:10)), as.integer64(1:10))
  expect_identical(union(as.integer64(5:10), 1.5:7), as.integer64(c(5:10, 1:4)))
  
})

test_that("union works (additional cases)", {
  
  expect_identical(union(c(5, 3, 5), c(0, 2, 1, 3, 6)), base::union(c(5, 3, 5), c(0, 2, 1, 3, 6)))
  expect_identical(union(c(5L, 3L, 5L), as.integer64(c(0, 2, 1, 3, 6))), as.integer64(c(5, 3, 0, 2, 1, 6)))
  
  expect_identical(union(NA, NA_integer_), base::union(NA, NA_integer_))
  expect_identical(union(NA_integer_, NA), base::union(NA_integer_, NA))
  expect_identical(union(NA, NA_integer64_), NA_integer64_)
  expect_identical(union(NA_integer64_, NA), NA_integer64_)
  
})


with_parameters_test_that("intersect works with basic R types (except double)", {
    if (getRversion() > "3.6.0" || !(type_x %in% c("POSIXct", "Date"))) {
      y = 5:10
      if (!is.na(type_x))
        x = eval(parse(text=paste0("as.", type_x, "(x)")))
      if (type_y == "integer64" && type_x %in% c(NA, "logical", "raw", "integer", "double")) {
        expected_result_x_y = as.integer64(base::intersect(x, y))
        expected_result_y_x = as.integer64(base::intersect(y, x))
      } else if (type_y == "integer64" && type_x %in% c("POSIXct", "Date")) {
        # as.POSIXct(1L) and as.Date(1L) stay internally integer
        # as.POSIXct(1) and as.Date(1) stay internally double
        y = as.double(y)
        expected_result_x_y = base::intersect(x, y)
        expected_result_y_x = base::intersect(y, x)
      } else {
        expected_result_x_y = base::intersect(x, y)
        if (getRversion() <= "3.6.0" && type_x %in% c("character", "factor", "ordered") && type_y == "integer64")
          expected_result_x_y = as.character(expected_result_x_y)
        if (getRversion() <= "3.6.0" && type_x == "complex" && type_y == "integer64")
          expected_result_x_y = as.complex(expected_result_x_y)
        expected_result_y_x = base::intersect(y, x)
      }
      y = as(y, type_y)
  
      expect_identical(
        intersect(x, y),
        expected_result_x_y
      )
      expect_identical(
        intersect(y, x),
        expected_result_y_x
      )
    }
  }, 
  .cases=expand.grid(x=I(list(NULL, c(1:7, 2L, 11L))), type_x=c(NA, "double", "logical", "integer", "character", "POSIXct", "Date", "complex", "factor", "ordered"), type_y=c("integer", "integer64"), stringsAsFactors=FALSE)
)

test_that("intersect works with special logic for double", {
  
  expect_identical(intersect(1.5:7, 5:10), base::intersect(1.5:7, 5:10))
  expect_identical(intersect(1.5:7, as.integer64(5:10)), as.integer64(5:6))
  expect_identical(intersect(as.integer64(5:10), 1.5:7), as.integer64(5:6))
  
})

test_that("intersect works (additional cases)", {
  
  expect_identical(intersect(c(5, 3, 5), c(0, 2, 1, 3, 6)), base::intersect(c(5, 3, 5), c(0, 2, 1, 3, 6)))
  expect_identical(intersect(as.integer64(c(5, 3, 5)), as.integer64(c(0, 2, 1, 3, 6))), as.integer64(3L))
  
  expect_identical(intersect(NA, NA_integer_), base::intersect(NA, NA_integer_))
  expect_identical(intersect(NA_integer_, NA), base::intersect(NA_integer_, NA))
  expect_identical(intersect(NA, NA_integer64_), NA_integer64_)
  expect_identical(intersect(NA_integer64_, NA), NA_integer64_)
  
})


with_parameters_test_that("setdiff works with basic R types (except double)", {
    if (getRversion() > "3.6.0" || !(type_x %in% c("POSIXct", "Date"))) {
      y = 5:10
      if (!is.na(type_x))
        x = eval(parse(text=paste0("as.", type_x, "(x)")))
      if (type_y == "integer64") {
        expected_result_x_y = base::setdiff(x, y)
        expected_result_y_x = as.integer64(base::setdiff(y, x))
      } else {
        expected_result_x_y = base::setdiff(x, y)
        expected_result_y_x = base::setdiff(y, x)
      }
      y = as(y, type_y)
  
      expect_identical(
        setdiff(x, y),
        expected_result_x_y
      )
      expect_identical(
        setdiff(y, x),
        expected_result_y_x
      )
    }
  }, 
  .cases=expand.grid(x=I(list(NULL, c(1:7, 2L, 11L))), type_x=c(NA, "double", "logical", "integer", "character", "POSIXct", "Date", "complex", "factor", "ordered"), type_y=c("integer", "integer64"), stringsAsFactors=FALSE)
)

test_that("setdiff works with special logic for double", {
  
  expect_identical(setdiff(1.5:7, 5:10), base::setdiff(1.5:7, 5:10))
  expect_identical(setdiff(1.5:7, as.integer64(5:10)), 1.5:4.5)
  expect_identical(setdiff(as.integer64(5:10), 1.5:7), as.integer64(7:10))
  
})

test_that("setdiff works (additional cases)", {
  
  expect_identical(setdiff(c(5, 3, 5), c(0, 2, 1, 3, 6)), base::setdiff(c(5, 3, 5), c(0, 2, 1, 3, 6)))
  expect_identical(setdiff(as.integer64(c(5, 3, 5)), as.integer64(c(0, 2, 1, 3, 6))), as.integer64(5L))
  
  expect_identical(setdiff(NA, NA_integer_), base::setdiff(NA, NA_integer_))
  expect_identical(setdiff(NA_integer_, NA), base::setdiff(NA_integer_, NA))
  expect_identical(setdiff(NA, NA_integer64_), logical())
  expect_identical(setdiff(NA_integer64_, NA), integer64())
  
})

with_parameters_test_that("setequal works with basic R types (except double)", {
    if (getRversion() > "3.6.0" || !(type_x %in% c("POSIXct", "Date"))) {
      y = 5:10
      if (!is.na(type_x))
        x = eval(parse(text=paste0("as.", type_x, "(x)")))
      expected_result_x_y = base::setequal(x, y)
      expected_result_y_x = base::setequal(y, x)
      y = as(y, type_y)
  
      expect_identical(
        setequal(x, y),
        expected_result_x_y
      )
      expect_identical(
        setequal(y, x),
        expected_result_y_x
      )
    }
  }, 
  .cases=expand.grid(x=I(list(NULL, c(1:7, 2L, 11L))), type_x=c(NA, "double", "logical", "integer", "character", "POSIXct", "Date", "complex", "factor", "ordered"), type_y=c("integer", "integer64"), stringsAsFactors=FALSE)
)

test_that("setequal works with special logic for double", {
  
  expect_identical(setequal(1.5:7, 5:10), base::setequal(1.5:7, 5:10))
  expect_identical(setequal(1.5:7, as.integer64(6:1)), TRUE)
  expect_identical(setequal(as.integer64(6:1), 1.5:7), TRUE)

})

test_that("setequal works (additional cases)", {
  
  expect_identical(setequal(c(5, 3, 5), c(0, 2, 1, 3, 6)), base::setequal(c(5, 3, 5), c(0, 2, 1, 3, 6)))
  expect_identical(setequal(as.integer64(c(5, 3, 5)), as.integer64(c(0, 2, 1, 3, 6))), FALSE)
  expect_identical(setequal(as.integer64(c(5, 3, 5)), as.integer64(c(3, 3, 3, 3, 5))), TRUE)
  
  expect_identical(setequal(NA, NA_integer_), TRUE)
  expect_identical(setequal(NA_integer_, NA), TRUE)
  expect_identical(setequal(NA, NA_integer64_), TRUE)
  expect_identical(setequal(NA_integer64_, NA), TRUE)
  
})

with_parameters_test_that("is.element works with basic R types (except double)", {
    if (getRversion() > "3.6.0" || !(type_x %in% c("POSIXct", "Date"))) {
      y = 5:10
      if (!is.na(type_x))
        x = eval(parse(text=paste0("as.", type_x, "(x)")))
      expected_result_x_y = base::is.element(x, y)
      expected_result_y_x = base::is.element(y, x)
      y = as(y, type_y)
  
      expect_identical(
        is.element(x, y),
        expected_result_x_y
      )
      expect_identical(
        is.element(y, x),
        expected_result_y_x
      )
    }
  }, 
  .cases=expand.grid(x=I(list(NULL, c(1:7, 2L, 11L))), type_x=c(NA, "double", "logical", "integer", "character", "POSIXct", "Date", "complex", "factor", "ordered"), type_y=c("integer", "integer64"), stringsAsFactors=FALSE)
)

test_that("is.element works with special logic for double", {
  
  expect_identical(is.element(1.5:7, 5:10), base::is.element(1.5:7, 5:10))
  expect_identical(is.element(1.5:7, as.integer64(6:1)), rep(TRUE, 6))
  expect_identical(is.element(as.integer64(6:1), 1.5:7), rep(TRUE, 6))

})

test_that("is.element works (additional cases)", {
  
  expect_identical(is.element(c(5, 3, 5), c(0, 2, 1, 3, 6)), base::is.element(c(5, 3, 5), c(0, 2, 1, 3, 6)))
  expect_identical(is.element(as.integer64(c(5, 3, 5)), as.integer64(c(0, 2, 1, 3, 6))), c(FALSE, TRUE, FALSE))
  expect_identical(is.element(as.integer64(c(5, 3, 5)), as.integer64(c(3, 3, 3, 3, 5))), rep(TRUE, 3))
  
  expect_identical(is.element(NA, NA_integer_), TRUE)
  expect_identical(is.element(NA_integer_, NA), TRUE)
  expect_identical(is.element(NA, NA_integer64_), TRUE)
  expect_identical(is.element(NA_integer64_, NA), TRUE)
  
})
