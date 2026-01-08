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

test_that("seq method works analogously to integer: 0 argument", {
  expect_identical(seq.integer64(), as.integer64(seq()))
})

test_that("seq method works analogously to integer: warning for unused arguments", {
  expect_identical(
    # remove call information
    gsub("^.*:\\n(.+)$", "\\1", tryCatch(seq(as.integer64(1L), extraArg=5L), warning=conditionMessage)),
    gsub("^.*:\\n(.+)$", "\\1", tryCatch(seq(1L, extraArg=5L), warning=conditionMessage))
  )
})

test_that("seq method works analogously to integer: 1 (length 0) argument", {
  n32 = integer()
  n64 = integer64()
  expect_identical(seq(n64), as.integer64(seq(n32)))
  expect_identical(seq(from=n64), as.integer64(seq(from=n32)))
  expect_identical(
    tryCatch(seq(to=n64), error=conditionMessage),
    tryCatch(seq(to=n32), error=conditionMessage)
  )
  expect_identical(
    tryCatch(seq(by=n64), error=conditionMessage),
    tryCatch(seq(by=n32), error=conditionMessage)
  )
  expect_identical(
    # for ubuntu-latest requires removing of "argument "
    tryCatch(seq(length.out=n64), error=conditionMessage),
    gsub("^argument (.*)", "\\1", tryCatch(seq(length.out=n32), error=conditionMessage))
  )
  expect_identical(seq(along.with=n64), as.integer64(seq(along.with=n32)))
})

test_that("seq method works analogously to integer: (1 length 2) argument", {
  n32 = 1:2
  n64 = as.integer64(n32)
  expect_identical(seq(n64), as.integer64(seq(n32)))
  expect_identical(seq(from=n64), as.integer64(seq(from=n32)))
  expect_identical(
    tryCatch(seq(to=n64), error=conditionMessage),
    tryCatch(seq(to=n32), error=conditionMessage)
  )
  expect_identical(
    tryCatch(seq(by=n64), error=conditionMessage),
    tryCatch(seq(by=n32), error=conditionMessage)
  )
  suppressWarnings(expect_identical(seq(length.out=n64), as.integer64(seq(length.out=n32))))
  expect_identical(
    tryCatch(seq(length.out=n64), warning=conditionMessage),
    tryCatch(seq(length.out=n32), warning=conditionMessage)
  )
  expect_identical(seq(along.with=n64), as.integer64(seq(along.with=n32)))
})

with_parameters_test_that(
  "seq method works analogously to integer: 1 argument (except along.with)",
  {
    n64 = as.integer64(n)
    expect_identical(seq(n64), as.integer64(seq(n)))
    expect_identical(seq(from=n64), as.integer64(seq(from=n)))
    expect_identical(seq(to=n64), as.integer64(seq(to=n)))
    expect_identical(seq(by=n64), as.integer64(seq(by=n)))
    if (n < 0L) {
      expect_identical(
        tryCatch(seq(length.out=n64), error=conditionMessage),
        tryCatch(seq(length.out=n), error=conditionMessage)
      )
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
    if (n2 %% 1.0 == 0.0) expect_identical(seq(n1_64, n2), as.integer64(seq(n1_32, n2)))
    expect_identical(seq(n1_64, n2_64), as.integer64(seq(n1_32, n2_32)))


    if (n2 == 0.0) {
      # error "invalid '(to - from)/by'"
      expect_identical(
        tryCatch(seq(n1_64, by=n2), error=conditionMessage),
        tryCatch(seq(n1_32, by=n2), error=conditionMessage)
      )
      expect_identical(
        tryCatch(seq(to=n1_64, by=n2), error=conditionMessage),
        tryCatch(seq(to=n1_32, by=n2), error=conditionMessage)
      )
    } else if (sign(1L - n1) == sign(n2)) {
      if (n2 %% 1.0 == 0.0) expect_identical(seq(n1_64, by=n2), as.integer64(seq(n1, by=n2)))
      expect_identical(suppressWarnings(seq(n1_64, by=n2_64)), as.integer64(seq(n1, by=n2_32)))

      # error "wrong sign in 'by' argument"
      expect_identical(
        tryCatch(suppressWarnings(seq(to=n1_64, by=n2)), error=conditionMessage),
        tryCatch(seq(to=n1_32, by=n2), error=conditionMessage)
      )
    } else {
      # error "wrong sign in 'by' argument"
      expect_identical(
        tryCatch(suppressWarnings(seq(n1_64, by=n2)), error=conditionMessage),
        tryCatch(seq(n1_32, by=n2), error=conditionMessage)
      )

      # TODO(#211): restore parity to seq() here
      if (n2 %% 1.0 == 0.0) expect_identical(seq(to=n1_64, by=n2), as.integer64(seq(to=n1, by=n2)))
      expect_identical(suppressWarnings(seq(to=n1_64, by=n2_64)), as.integer64(seq(to=n1, by=n2_32)))
    }

    if (n2 >= 0.0) {
      expect_identical(seq(n1_64, length.out=n2), as.integer64(seq(n1_32, length.out=n2)))
      expect_identical(seq(n1_64, length.out=n2_64), as.integer64(seq(n1_32, length.out=n2_32)))

      expect_identical(seq(to=n1_64, length.out=n2), as.integer64(seq(to=n1_32, length.out=n2)))
      expect_identical(seq(to=n1_64, length.out=n2_64), as.integer64(seq(to=n1_32, length.out=n2_32)))

      expect_identical(seq(by=n1_64, length.out=n2), as.integer64(seq(by=n1_32, length.out=n2)))
      expect_identical(seq(by=n1_64, length.out=n2_64), as.integer64(seq(by=n1_32, length.out=n2_32)))
    } else {
      # error "'length.out' must be a non-negative number"
      expect_identical(
        tryCatch(seq(n1_64, length.out=n2), error=conditionMessage),
        tryCatch(seq(n1_32, length.out=n2), error=conditionMessage)
      )
      expect_identical(
        tryCatch(seq(to=n1_64, length.out=n2), error=conditionMessage),
        tryCatch(seq(to=n1_32, length.out=n2), error=conditionMessage)
      )
      expect_identical(
        tryCatch(seq(by=n1_64, length.out=n2), error=conditionMessage),
        tryCatch(seq(by=n1_32, length.out=n2), error=conditionMessage)
      )
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
      if (n2 %% 1.0 == 0.0 && n3 %% 1.0 == 0.0) {
        expect_identical(seq(n1_64, n2, by=n3), as.integer64(seq(n1_32, n2, by=n3)))
        expect_identical(seq(n1_64, n2_64, by=n3), as.integer64(seq(n1_32, n2_32, by=n3)))
        expect_identical(seq(n1_64, n2, by=n3_64), as.integer64(seq(n1_32, n2, by=n3_32)))
        expect_identical(seq(n1_64, n2_64, by=n3_64), as.integer64(seq(n1_32, n2_32, by=n3_32)))
      } else if (n2 %% 1.0 == 0.0) {
        # coerce by
        expect_warning(expect_identical(seq(n1_64, n2, by=n3), as.integer64(seq(n1_32, n2, by=n3_32))), "argument 'by' is coerced to integer64", fixed=TRUE)
        expect_warning(expect_identical(seq(n1_64, n2_64, by=n3), as.integer64(seq(n1_32, n2_32, by=n3_32))), "argument 'by' is coerced to integer64", fixed=TRUE)
        expect_identical(seq(n1_64, n2, by=n3_64), as.integer64(seq(n1_32, n2, by=n3_32)))
        expect_identical(seq(n1_64, n2_64, by=n3_64), as.integer64(seq(n1_32, n2_32, by=n3_32)))
      } else if (n3 %% 1.0 == 0.0) {
        # coerce to
        expect_warning(expect_identical(seq(n1_64, n2, by=n3), as.integer64(seq(n1_32, n2_32, by=n3))), "argument 'to' is coerced to integer64", fixed=TRUE)
        expect_identical(seq(n1_64, n2_64, by=n3), as.integer64(seq(n1_32, n2_32, by=n3)))
        expect_warning(expect_identical(seq(n1_64, n2, by=n3_64), as.integer64(seq(n1_32, n2_32, by=n3_32))), "argument 'to' is coerced to integer64", fixed=TRUE)
        expect_identical(seq(n1_64, n2_64, by=n3_64), as.integer64(seq(n1_32, n2_32, by=n3_32)))
      } else {
        # coerce by and to
        if (getRversion() >= "4.0.0") {
          # not all warnings are emitted in R < 4.0.0
          expect_warning(
            expect_warning(expect_identical(seq(n1_64, n2, by=n3), as.integer64(seq(n1_32, n2_32, by=n3_32))), "argument 'by' is coerced to integer64", fixed=TRUE),
            "argument 'to' is coerced to integer64", fixed=TRUE
          )
        }
        expect_warning(expect_identical(seq(n1_64, n2_64, by=n3), as.integer64(seq(n1_32, n2_32, by=n3_32))), "argument 'by' is coerced to integer64", fixed=TRUE)
        expect_warning(expect_identical(seq(n1_64, n2, by=n3_64), as.integer64(seq(n1_32, n2_32, by=n3_32))), "argument 'to' is coerced to integer64", fixed=TRUE)
        expect_identical(seq(n1_64, n2_64, by=n3_64), as.integer64(seq(n1_32, n2_32, by=n3_32)))
      }
    } else {
      expect_identical(
        tryCatch(suppressWarnings(seq(n1_64, n2, by=n3)), error=conditionMessage),
        tryCatch(seq(n1_32, n2, by=n3), error=conditionMessage)
      )
    }

    if (n3 < 0L) {
      # error "'length.out' must be a non-negative"
      expect_identical(
        tryCatch(suppressWarnings(seq(n1_64, n2, length.out=n3)), error=conditionMessage),
        tryCatch(seq(n1_32, n2, length.out=n3), error=conditionMessage)
      )
      expect_identical(
        tryCatch(suppressWarnings(seq(n1_64, by=n2, length.out=n3)), error=conditionMessage),
        tryCatch(seq(n1_32, by=n2, length.out=n3), error=conditionMessage)
      )
      expect_identical(
        tryCatch(suppressWarnings(seq(to=n1_64, by=n2, length.out=n3)), error=conditionMessage),
        tryCatch(seq(to=n1_32, by=n2, length.out=n3), error=conditionMessage)
      )
    } else {
      # TODO(#211): restore parity to seq() here
      if (n3 == 0L || ((n2 - n1) / (n3 - 1.0)) %% 1.0 == 0.0) {
        if (n2 %% 1.0 == 0.0) {
          expect_identical(seq(n1_64, n2, length.out=n3), as.integer64(seq(n1_32, n2, length.out=n3)))
          expect_identical(seq(n1_64, n2_64, length.out=n3), as.integer64(seq(n1_32, n2_32, length.out=n3)))
          expect_identical(seq(n1_64, n2, length.out=n3_64), as.integer64(seq(n1_32, n2, length.out=n3_32)))
          expect_identical(seq(n1_64, n2_64, length.out=n3_64), as.integer64(seq(n1_32, n2_32, length.out=n3_32)))
        } else {
          expect_warning(expect_identical(seq(n1_64, n2, length.out=n3), as.integer64(seq(n1_32, n2_32, length.out=n3))), "argument 'to' is coerced to integer64", fixed=TRUE)
          expect_identical(seq(n1_64, n2_64, length.out=n3), as.integer64(seq(n1_32, n2_32, length.out=n3)))
          expect_warning(expect_identical(seq(n1_64, n2, length.out=n3_64), as.integer64(seq(n1_32, n2_32, length.out=n3_32))), "argument 'to' is coerced to integer64", fixed=TRUE)
          expect_identical(seq(n1_64, n2_64, length.out=n3_64), as.integer64(seq(n1_32, n2_32, length.out=n3_32)))
        }
      } else {
        # truncate by warning
        if (n2 %% 1.0 == 0.0) {
          by = as.integer(((n2 - n1) / (n3 - 1.0)))
          expect_warning(expect_identical(seq(n1_64, n2, length.out=n3), as.integer64(seq(n1_32, by=by, length.out=n3))), "the resulting 'by' is truncated to integer64", fixed=TRUE)
          expect_warning(expect_identical(seq(n1_64, n2_64, length.out=n3), as.integer64(seq(n1_32, by=by, length.out=n3))), "the resulting 'by' is truncated to integer64", fixed=TRUE)
          expect_warning(expect_identical(seq(n1_64, n2, length.out=n3_64), as.integer64(seq(n1_32, by=by, length.out=n3_32))), "the resulting 'by' is truncated to integer64", fixed=TRUE)
          expect_warning(expect_identical(seq(n1_64, n2_64, length.out=n3_64), as.integer64(seq(n1_32, by=by, length.out=n3_32))), "the resulting 'by' is truncated to integer64", fixed=TRUE)
        }
      }

      # TODO(#211): restore parity to seq() here
      if (n2 %% 1.0 == 0.0) {
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

test_that("seq method works analogously to integer: 3 arguments with integer64 coercion and truncation of by", {
  skip_unless_r(">= 4.0.0")
  # not all warnings are emitted in R < 4.0.0
  expect_warning(
    expect_warning(
      expect_identical(seq(as.integer64(5L), 1.5, length.out=3.5), as.integer64(seq(5L, by=-1, length.out=3.5))),
      "argument 'to' is coerced to integer64", fixed=TRUE
    ), 
    "the resulting 'by' is truncated to integer64", fixed=TRUE
  )
})

test_that("seq method works analogously to integer: 4 arguments", {
  n32 = 5L
  n64 = as.integer64(n32)

  expect_identical(
    tryCatch(seq(n64, n64, by=n64, length.out=n64), error=conditionMessage),
    tryCatch(seq(n32, n32, by=n32, length.out=n32), error=conditionMessage)
  )
  expect_identical(
    tryCatch(seq(n64, n64, by=n64, along.with=n64), error=conditionMessage),
    tryCatch(seq(n32, n32, by=n32, along.with=n32), error=conditionMessage)
  )
})

test_that("seq() works back-compatibly w.r.t. mixed integer+double inputs", {
  one = as.integer64(1L)
  expect_warning(expect_identical(seq(one, 10L, by=1.5), as.integer64(1:10)), "argument 'by' is coerced to integer64", fixed=TRUE)
  expect_warning(expect_identical(seq(to=one, from=10L, by=-1.5), as.integer64(10:1)), "argument 'by' is coerced to integer64", fixed=TRUE)

  expect_warning(expect_identical(seq(one, 10L, by=10.0/3.0), as.integer64(c(1L, 4L, 7L, 10L))), "argument 'by' is coerced to integer64", fixed=TRUE)
  expect_warning(expect_identical(seq(to=one, from=10L, by=-10.0/3.0), as.integer64(c(10L, 7L, 4L, 1L))), "argument 'by' is coerced to integer64", fixed=TRUE)

  expect_error(
    expect_warning(seq(one, 10L, by=0.1), "argument 'by' is coerced to integer64", fixed=TRUE), 
    "invalid '(to - from)/by'", fixed=TRUE
  )
  expect_error(
    expect_warning(seq(to=one, from=10L, by=-0.1), "argument 'by' is coerced to integer64", fixed=TRUE), 
    "invalid '(to - from)/by'", fixed=TRUE
  )
  expect_warning(expect_identical(seq(one, 2.5), as.integer64(1:2)), "argument 'to' is coerced to integer64", fixed=TRUE)
  expect_warning(expect_identical(seq(to=one, from=2.5), as.integer64(2:1)), "argument 'from' is coerced to integer64", fixed=TRUE)

  skip_unless_r(">= 4.0.0")
  # not all warnings are emitted in R < 4.0.0  
  expect_warning(
    expect_warning(expect_identical(seq(one, 5.5, by=1.5), as.integer64(1:5)), "argument 'to' is coerced to integer64", fixed=TRUE),
    "argument 'by' is coerced to integer64", fixed=TRUE
  )
  expect_warning(
    expect_warning(expect_identical(seq(to=one, from=5.5, by=-1.5), as.integer64(5:1)), "argument 'from' is coerced to integer64", fixed=TRUE),
    "argument 'by' is coerced to integer64", fixed=TRUE
  )
})

test_that("seq method works analogously to integer: further tests", {
  n_32 = 10L
  n_64 = as.integer64(n_32)

  expect_identical(seq(from=n_64), as.integer64(seq(from=n_32)))
  expect_identical(seq(to=n_64), as.integer64(seq(to=n_32)))
  expect_identical(seq(length.out=n_64), as.integer64(seq(length.out=n_32)))
  expect_identical(seq(along.with=n_64), as.integer64(seq(along.with=n_32)))

  expect_identical(seq(NA_integer64_), as.integer64(seq(NA)))
  expect_identical(seq(rep(NA_integer64_, 2)), as.integer64(seq(rep(NA, 2))))
  expect_identical(
    tryCatch(seq(NA_integer64_, 2L), error=conditionMessage),
    tryCatch(seq(NA, 2L), error=conditionMessage)
  )
  expect_identical(
    tryCatch(seq(to=NA_integer64_, 2L), error=conditionMessage),
    tryCatch(seq(to=NA, 2L), error=conditionMessage)
  )
  expect_identical(
    tryCatch(seq(length.out=NA_integer64_, 2L), error=conditionMessage),
    tryCatch(seq(length.out=NA, 2L), error=conditionMessage)
  )
  
  expect_identical(seq(as.integer64(0L)), as.integer64(seq(0L)))

  expect_identical(seq(integer64()), as.integer64(seq(integer())))
  
  expect_identical(
    tryCatch(seq(as.integer64(1:2), 10L), error=conditionMessage),
    tryCatch(seq(1:2, 10L), error=conditionMessage)
  )
  
  expect_warning(
    # by = as.integer(((10 - 1) / (5 - 1)))
    expect_identical(seq(as.integer64(1L), 10L, along.with=1:5), as.integer64(seq(1L, by=2L, along.with=1:5))),
    "the resulting 'by' is truncated to integer64", fixed=TRUE
  )

  expect_identical(seq(as.integer64(1L), 10L, along.with=1:10), as.integer64(seq(1L, 10L, along.with=1:10)))
  
  expect_identical(seq(as.integer64(1L), 10L, along.with=NULL), as.integer64(seq(1L, 10L, along.with=NULL))) 
  
  expect_identical(
    tryCatch(seq(as.integer64(1L), 10L, by=NULL), error=conditionMessage),
    tryCatch(seq(1L, 10L, by=NULL), error=conditionMessage)
  )
  
  expect_identical(
    # for ubuntu-latest requires removing of "argument "
    tryCatch(seq(as.integer64(1L), 10L, length.out=NULL), error=conditionMessage),
    gsub("^argument (.*)", "\\1", tryCatch(seq(1L, 10L, length.out=NULL), error=conditionMessage))
  )

  expect_identical(seq(as.integer64(1L), 10L, length.out=0L), as.integer64(seq(1L, 10L, length.out=0L)))
  
  expect_identical(seq(as.integer64(1L), 10L, length.out=10L), as.integer64(seq(1L, 10L, length.out=10L)))

  expect_warning(
    # by = as.integer(((10 - 1) / (3 - 1)))
    expect_identical(seq(as.integer64(1L), 10L, length.out=3L), as.integer64(seq(1L, by=4L, length.out=3L))),
    "the resulting 'by' is truncated to integer64", fixed=TRUE
  )

  expect_identical(seq(as.integer64(1L), along.with=1:5), as.integer64(seq(1L, along.with=1:5)))
  
  expect_identical(seq(as.integer64(1L), length.out=2L), as.integer64(seq(1L, length.out=2L)))

  expect_warning(
    # by = as.integer(((10 - 1) / (5 - 1)))
    expect_identical(seq(as.integer64(1L), 10L, along.with=1:5, length.out=2), as.integer64(seq(1L, by=2L, along.with=1:5, length.out=2))),
    "the resulting 'by' is truncated to integer64", fixed=TRUE
  )

  expect_identical(seq(as.integer64(1L), along.with=1:5, by=2L), as.integer64(seq(1L, along.with=1:5, by=2L)))

  expect_identical(seq(length.out=as.integer64(2L)), as.integer64(seq(length.out=2L)))
    
  expect_identical(seq(as.integer64(10L), 1L, along.with=1:10), as.integer64(seq(10L, 1L, along.with=1:10)))

  expect_identical(seq(as.integer64(10L), 10L, length.out=3L), as.integer64(seq(10L, 10L, length.out=3L)))
  
  expect_warning(
    # by = as.integer(((20 - 10) / (4 - 1)))
    expect_identical(seq(as.integer64(10L), 20L, length.out=4L), as.integer64(seq(10L, by=3L, length.out=4L))),
    "the resulting 'by' is truncated to integer64", fixed=TRUE
  )
  
  expect_identical(seq(as.integer64(10L), 20L, length.out=3L), as.integer64(seq(10L, 20L, length.out=3L)))
  
  expect_identical(seq(to=as.integer64(20L), length.out=4L), as.integer64(seq(to=20L, length.out=4L)))
  
  expect_identical(seq(from=as.integer64(20L), length.out=4L), as.integer64(seq(from=20L, length.out=4L)))

  expect_identical(seq(from=as.integer64(1L), to=-1L), as.integer64(seq(from=1L, to=-1L)))
})

test_that("seq error if result does not fit in integer64", {
  expect_identical(seq(as.integer64(2^30), by=as.integer(2^30), length.out=2L), as.integer64(seq(as.integer(2^30), by=2^30, length.out=2L)))
  expect_error(seq(as.integer64(2^62), by=as.integer64(2^62), length.out=2L), "resulting sequence does not fit in integer64")
})

test_that(":.integer64 works analogously to integer", {
  i64_1 = as.integer64(1L)
  i64_10 = as.integer64(10L)
  
  expect_identical(i64_1:i64_10, as.integer64(1:10))
  expect_identical(i64_1:10, as.integer64(1:10))
  expect_identical(i64_1:"10", as.integer64(1:10))
  expect_identical(i64_10:i64_1, as.integer64(10:1))
  expect_identical(i64_10:1, as.integer64(10:1))
  expect_identical(i64_10:"1", as.integer64(10:1))
  expect_identical(
    tryCatch(i64_1:NA, error=conditionMessage),
    tryCatch(1L:NA, error=conditionMessage)
  )
  expect_identical(
    tryCatch(i64_1:NA_integer64_, error=conditionMessage),
    tryCatch(1L:NA, error=conditionMessage)
  )
  expect_identical(
    tryCatch(i64_1:"a", error=conditionMessage),
    tryCatch(suppressWarnings(1L:"a"), error=conditionMessage)
  )
  expect_identical(
    tryCatch(NA_integer64_:1L, error=conditionMessage),
    tryCatch(NA:1L, error=conditionMessage)
  )
  expect_identical(
    tryCatch(integer64():1L, error=conditionMessage),
    tryCatch(integer():1L, error=conditionMessage)
  )
  expect_identical(
    tryCatch(i64_1:integer(), error=conditionMessage),
    tryCatch(1L:integer(), error=conditionMessage)
  )
})

test_that(":.integer64 some integer64 specific behavior", {
  
  expect_error(lim.integer64()[1]:lim.integer64()[2], "sequence generation would be too long")

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
