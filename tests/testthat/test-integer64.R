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

  expect_identical(seq(x[1L], x[3L], by=1L), x)
  expect_identical(seq(x[1L], x[3L], by=x[1L]), x)
  expect_identical(seq(x[1L], to=10L, by=1L), as.integer64(1:10))
  expect_identical(seq(x[1L], to=11L, by=2L), as.integer64(c(1L, 3L, 5L, 7L, 9L, 11L)))
  # TODO(#47): More tests when the behavior is corrected.
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
  y = integer64(0L)

  expect_identical(x+y, integer64())
  expect_identical(y+x, integer64())

  expect_identical(x-y, integer64())
  expect_identical(y-x, integer64())

  expect_identical(+y, integer64())
  expect_identical(-y, integer64())

  expect_identical(x*y, integer64())
  expect_identical(y*x, integer64())

  expect_identical(x/y, double())
  expect_identical(y/x, double())

  expect_identical(x^y, integer64())
  expect_identical(y^x, integer64())

  expect_identical(x %/% y, integer64())
  expect_identical(y %/% x, integer64())

  expect_identical(x%%y, integer64())
  expect_identical(y%%x, integer64())

  expect_identical(log(x, base=y), double())
  # TODO(#93): don't suppress this warning which is inconsistent with integer()
  expect_identical(suppressWarnings(log(y, base=x)), double())

  expect_identical(x==y, logical())
  expect_identical(y==x, logical())

  expect_identical(x!=y, logical())
  expect_identical(y!=x, logical())

  expect_identical(x>=y, logical())
  expect_identical(y>=x, logical())

  expect_identical(x<=y, logical())
  expect_identical(y<=x, logical())

  expect_identical(x>y, logical())
  expect_identical(y>x, logical())

  expect_identical(x<y, logical())
  expect_identical(y<x, logical())

  expect_identical(x&y, logical())
  expect_identical(y&x, logical())

  expect_identical(x|y, logical())
  expect_identical(y|x, logical())

  expect_identical(xor(x, y), logical())
  expect_identical(xor(y, x), logical())
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

  skip_if_not_installed("withr") # only really for testing without testthat
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

  skip_if_not_installed("withr") # only really for testing without testthat
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
  x32 = as.integer64(1:10)
  x64 = as.integer64(1:10)
  expect_identical(match(x64, integer()), match(x32, integer()))
  expect_identical(match(x64, integer(), nomatch=NULL), match(x32, integer(), nomatch=NULL))
  expect_identical(match(x64, integer(), nomatch=10L), match(x32, integer(), nomatch=10L))
  expect_identical(match(integer(), x64), match(integer(), x32))
})
