# These tests were previously kept as tests under \examples{\dontshow{...}}.
#   Converted to "proper" unit tests for clarity, after making them more
#   canonical within {testthat}, e.g. better capturing expected warnings,
#   changing stopifnot(identical(...)) to expect_identical(...).

test_that("identical.integer64", {
  i64 = NA_real_
  class(i64) = "integer64"
  expect_identical(unclass(i64 - 1.0), unclass(i64 + 1.0))
  expect_identical(i64 - 1.0, i64 + 1.0)
  expect_false(identical.integer64(i64 - 1.0, i64 + 1.0))
})

test_that("dispatch of 'c' method", {
  expect_true(identical.integer64(c(integer64(0L), NA), as.integer64(NA)))
})

test_that("Dispatch on the second argument fails and we want to be notified once that changes", {
  expect_false(identical.integer64(c(NA, integer64(0L)), as.integer64(NA)))
})

test_that("Minus and plus", {
  d64 = c(
    -.Machine$double.base^.Machine$double.digits,
    -.Machine$integer.max,
    -1.0, 0.0, 1.0,
    .Machine$integer.max,
    .Machine$double.base^.Machine$double.digits
  )
  i64 = as.integer64(d64)
  expect_true(identical.integer64(i64 - 1.0 + 1.0, i64))
  expect_true(identical.integer64(i64 + 1.0 - 1.0, i64))
})

test_that("Minus and plus edge cases and 'rev'", {
  # UBSAN signed integer overflow expected for type 'long long int'
  # This is a false UBSAN alarm because overflow is detected and NA returned
  expect_warning(
    expect_true(
        identical.integer64(lim.integer64() + 1.0 - 1.0,
        c(lim.integer64()[1L], NA))
    ),
    "NAs produced by integer64 overflow",
    fixed = TRUE
  )
  expect_warning(
    expect_true(
        identical.integer64(rev(lim.integer64()) - 1.0 + 1.0,
        c(lim.integer64()[2L], NA))
    ),
    "NAs produced by integer64 overflow",
    fixed = TRUE
  )
})

test_that("'range.integer64', multiplication, integer division, sqrt, power, and log", {
  i64 = integer64(63L)
  i64[1L] = 1.0
  for (i in 2:63)
    i64[i] = 2.0 * i64[i-1L]
  expect_true(identical.integer64(i64 * rev(i64), rep(i64[63L], 63L)))
  for (i in 63:2)
    i64[i-1L] = i64[i] %/% 2.0
  expect_true(identical.integer64(i64 * rev(i64), rep(i64[63L], 63L)))
  for (i in 63:2)
    i64[i-1L] = i64[i] / 2.0
  expect_true(identical.integer64(i64 * rev(i64), rep(i64[63L], 63L)))
  expect_true(identical.integer64(
    c(
      -i64[63L] - (i64[63L] - 1.0),
      i64[63L] + (i64[63L] - 1.0)
    ),
    lim.integer64()
  ))

  expect_true(identical.integer64(i64[-1L] %/%2.0 * as.integer64(2L), i64[-1L]))
  expect_true(identical.integer64(i64[-1L] %/%2L * as.integer64(2L), i64[-1L]))
  expect_true(identical.integer64(i64[-1L] / 2.0 * as.integer64(2L), i64[-1L]))
  expect_true(identical.integer64(i64[-1L] / 2.0 * as.integer64(2L), i64[-1L]))

  expect_true(identical.integer64(i64[-63L] * 2.0 %/% 2.0, i64[-63L]))
  expect_true(identical.integer64(i64[-63L] * 2L %/% 2L, i64[-63L]))
  expect_true(identical.integer64(as.integer64(i64[-63L] * 2.0 / 2.0), i64[-63L]))
  expect_true(identical.integer64(as.integer64(i64[-63L] * 2L / 2L), i64[-63L]))

  expect_true(identical.integer64(
    as.integer64(sqrt(
      i64[-1L][c(FALSE, TRUE)]) * sqrt(i64[-1L][c(FALSE, TRUE)]
    )),
    i64[-1L][c(FALSE, TRUE)]
  ))

  expect_true(identical.integer64(as.integer64(2L) ^ (0:62), i64))
  expect_true(identical.integer64(as.integer64(0:62), as.integer64(round(log2(i64)))))
  expect_true(identical.integer64(
    as.integer64(round(log(as.integer64(2L)^(0:62), 2.0))),
    as.integer64(0:62)
  ))
  expect_true(identical.integer64(
    as.integer64(round(log(as.integer64(3L)^(0:39), 3.0))),
    as.integer64(0:39)
  ))
  expect_true(identical.integer64(
    as.integer64(round(log(as.integer64(10L)^(0:18), 10.0))),
    as.integer64(0:18)
  ))
  expect_true(identical.integer64(
    as.integer64(round(log10(as.integer64(10L)^(0:18)))),
    as.integer64(0:18)
  ))

  expect_true(identical.integer64(
    (as.integer64(2L)^(1:62))^(1.0/1:62),
    as.integer64(rep(2.0, 62L))
  ))
  expect_true(identical.integer64(
    (as.integer64(3L)^(1:39))^(1.0/1:39),
    as.integer64(rep(3.0, 39L))
  ))
  expect_true(identical.integer64(
    (as.integer64(10L)^(1:18))^(1.0/1:18),
    as.integer64(rep(10.0, 18L))
  ))
})

test_that("c and rep", {
  expect_true(identical.integer64(
    as.integer64(rep(1:3, 1:3)),
    rep(as.integer64(1:3), 1:3)
  ))
  expect_true(identical.integer64(
    as.integer64(rep(1:3, 3L)),
    rep(as.integer64(1:3), 3L)
  ))

  x = rep(NA_real_, 3L)
  class(x) = "integer64"
  x = x + -1:1
  expect_true(identical.integer64(rep(x, 3L), c(x, x, x)))
  expect_true(identical.integer64(
    c.integer64(list(x, x, x), recursive=TRUE),
    c(x, x, x)
  ))
})

test_that("seq", {
  expect_true(identical.integer64(
    seq(as.integer64(1L), 10.0, 2.0),
    as.integer64(seq(1.0, 10.0, 2.0))
  ))
  expect_true(identical.integer64(
    seq(as.integer64(1L), by=2.0, length.out=5.0),
    as.integer64(seq(1.0, by=2.0, length.out=5.0))
  ))
  expect_true(identical.integer64(
    seq(as.integer64(1L), by=2.0, length.out=6.0),
    as.integer64(seq(1.0, by=2.0, length.out=6.0))
  ))
  expect_true(identical.integer64(
    seq.integer64(along.with=3:5),
    as.integer64(seq(along.with=3:5))
  ))
  expect_true(identical.integer64(
    seq(as.integer64(1L), to=-9.0),
    as.integer64(seq(1.0, to=-9.0))
  ))
})

test_that("cbind and rbind", {
  x = rep(as.integer64(1:3), 2L)
  dim(x) = c(3L, 2L)
  expect_true(identical.integer64(cbind(as.integer64(1:3), 1:3), x))
  expect_true(identical.integer64(rbind(as.integer64(1:3), 1:3), t(x)))
})

test_that("Coercion", {
  expect_identical(
    as.double(as.integer64(c(NA, seq(0.0, 9.0, 0.25)))),
    as.double(as.integer(c(NA, seq(0.0, 9.0, 0.25))))
  )
  expect_identical(
    as.character(as.integer64(c(NA, seq(0.0, 9.0, 0.25)))),
    as.character(as.integer(c(NA, seq(0.0, 9.0, 0.25))))
  )
  expect_identical(
    as.integer(as.integer64(c(NA, seq(0.0, 9.0, 0.25)))),
    as.integer(c(NA, seq(0.0, 9.0, 0.25)))
  )
  expect_identical(
    as.logical(as.integer64(c(NA, seq(0.0, 9.0, 0.25)))),
    as.logical(as.integer(c(NA, seq(0.0, 9.0, 0.25))))
  )
  expect_identical(
    as.integer(as.integer64(c(NA, FALSE, TRUE))),
    as.integer(c(NA, FALSE, TRUE))
  )
  expect_identical(
    as.integer64(as.integer(as.integer64(-9:9))),
    as.integer64(-9:9)
  )
  expect_identical(
    as.integer64(as.double(as.integer64(-9:9))),
    as.integer64(-9:9)
  )
  expect_identical(
    as.integer64(as.character(as.integer64(-9:9))),
    as.integer64(-9:9)
  )
  expect_identical(
    as.integer64(as.character(lim.integer64())),
    lim.integer64()
  )
})

test_that("Logical operators", {
  expect_true(identical.integer64(
    !c(NA, -1:1),
    !c(as.integer64(NA), -1:1)
  ))

  xi = rep(c(NA, -1:1), 4L)
  xi64 = as.integer64(xi)
  yi = rep(c(NA, -1:1), each=4L)
  yi64 = as.integer64(yi)

  expect_true(identical.integer64(xi64 & yi64, xi & yi))
  expect_true(identical.integer64(xi64 | yi64, xi | yi))
  expect_true(identical.integer64(xor(xi64, yi64), xor(xi, yi)))
})

test_that("Comparison operators", {
  xi = rep(c(NA, -1:1), 4L)
  xi64 = as.integer64(xi)
  yi = rep(c(NA, -1:1), each=4L)
  yi64 = as.integer64(yi)

  expect_true(identical.integer64(xi64 == yi64, xi == yi))
  expect_true(identical.integer64(xi64 != yi64, xi != yi))
  expect_true(identical.integer64(xi64 > yi64, xi > yi))
  expect_true(identical.integer64(xi64 >= yi64, xi >= yi))
  expect_true(identical.integer64(xi64 < yi64, xi < yi))
  expect_true(identical.integer64(xi64 <= yi64, xi <= yi))
})

test_that("Vector functions", {
  xi = c(NA, -1:1)
  xi64 = as.integer64(xi)

  expect_true(identical.integer64(is.na(xi64), is.na(xi)))
  expect_true(identical.integer64(format(xi64), format(xi)))
  expect_true(identical.integer64(abs(xi64), as.integer64(abs(xi))))
  expect_true(identical.integer64(sign(xi64), as.integer64(sign(xi))))
  expect_true(identical.integer64(ceiling(xi64), as.integer64(ceiling(xi))))
  expect_true(identical.integer64(floor(xi64), as.integer64(floor(xi))))
  expect_true(identical.integer64(trunc(xi64), as.integer64(trunc(xi))))
  expect_true(identical.integer64(signif(xi64), xi64))
})

test_that("Summary functions", {
  expect_identical(all(as.integer64(1L)), all(1L))
  expect_identical(all(as.integer64(0L)), all(0L))
  expect_identical(all(NA_integer64_), all(NA_integer_))
  expect_identical(all(NA_integer64_, na.rm=TRUE), all(NA_integer_, na.rm=TRUE))
  expect_identical(all(as.integer64(1L), NA), all(1L, NA))
  expect_identical(all(as.integer64(0L), NA), all(0L, NA))
  expect_identical(all(as.integer64(1L), NA, na.rm=TRUE), all(1L, NA, na.rm=TRUE))
  expect_identical(all(as.integer64(0L), NA, na.rm=TRUE), all(0L, NA, na.rm=TRUE))
  expect_identical(all(as.integer64(c(1L, NA))), all(c(1L, NA_integer_)))
  expect_identical(all(as.integer64(c(0L, NA))), all(c(0L, NA_integer_)))
  expect_identical(all(as.integer64(c(1L, NA)), na.rm=TRUE), all(c(1L, NA_integer_), na.rm=TRUE))
  expect_identical(all(as.integer64(c(0L, NA)), na.rm=TRUE), all(c(0L, NA_integer_), na.rm=TRUE))

  expect_identical(any(as.integer64(1L)), any(1L))
  expect_identical(any(as.integer64(0L)), any(0L))
  expect_identical(any(NA_integer64_), any(NA_integer_))
  expect_identical(any(NA_integer64_, na.rm=TRUE), any(NA_integer_, na.rm=TRUE))
  expect_identical(any(as.integer64(1L), NA), any(1L, NA))
  expect_identical(any(as.integer64(0L), NA), any(0L, NA))
  expect_identical(any(as.integer64(1L), NA, na.rm=TRUE), any(1L, NA, na.rm=TRUE))
  expect_identical(any(as.integer64(0L), NA, na.rm=TRUE), any(0L, NA, na.rm=TRUE))
  expect_identical(any(as.integer64(c(1L, NA))), any(c(1L, NA_integer_)))
  expect_identical(any(as.integer64(c(0L, NA))), any(c(0L, NA_integer_)))
  expect_identical(any(as.integer64(c(1L, NA)), na.rm=TRUE), any(c(1L, NA_integer_), na.rm=TRUE))
  expect_identical(any(as.integer64(c(0L, NA)), na.rm=TRUE), any(c(0L, NA_integer_), na.rm=TRUE))

  xd = c(2.0, 3.0, NA)
  xi64 = as.integer64(xd)

  expect_true(identical.integer64(
    as.integer64(sum(xd)),
    sum(xi64)
  ))
  expect_true(identical.integer64(
    as.integer64(sum(xd, na.rm=TRUE)),
    sum(xi64, na.rm=TRUE)
  ))
  expect_true(identical.integer64(
    as.integer64(sum(xd)),
    sum(xi64)
  ))
  expect_true(identical.integer64(
    as.integer64(sum(xd, na.rm=TRUE)),
    sum(xi64, na.rm=TRUE)
  ))
  expect_true(identical.integer64(
    as.integer64(sum(2.0, 3.0, NA)),
    sum(as.integer64(2L), 3.0, NA)
  ))
  expect_true(identical.integer64(
    as.integer64(sum(2.0, 3.0, NA, na.rm=TRUE)),
    sum(as.integer64(2L), 3.0, NA, na.rm=TRUE)
  ))
  expect_true(identical.integer64(
    as.integer64(sum(2.0, 3.0, NA)),
    sum(as.integer64(2L), 3.0, NA)
  ))
  expect_true(identical.integer64(
    as.integer64(sum(2.0, 3.0, NA, na.rm=TRUE)),
    sum(as.integer64(2L), 3.0, NA, na.rm=TRUE)
  ))

  expect_true(identical.integer64(
    as.integer64(prod(xd)),
    prod(xi64)
  ))
  expect_true(identical.integer64(
    as.integer64(prod(xd, na.rm=TRUE)),
    prod(xi64, na.rm=TRUE)
  ))
  expect_true(identical.integer64(
    as.integer64(prod(xd)),
    prod(xi64)
  ))
  expect_true(identical.integer64(
    as.integer64(prod(xd, na.rm=TRUE)),
    prod(xi64, na.rm=TRUE)
  ))
  expect_true(identical.integer64(
    as.integer64(prod(2.0, 3.0, NA)),
    prod(as.integer64(2L), 3.0, NA)
  ))
  expect_true(identical.integer64(
    as.integer64(prod(2.0, 3.0, NA, na.rm=TRUE)),
    prod(as.integer64(2L), 3.0, NA, na.rm=TRUE)
  ))
  expect_true(identical.integer64(
    as.integer64(prod(2.0, 3.0, NA)),
    prod(as.integer64(2L), 3.0, NA)
  ))
  expect_true(identical.integer64(
    as.integer64(prod(2.0, 3.0, NA, na.rm=TRUE)),
    prod(as.integer64(2L), 3.0, NA, na.rm=TRUE)
  ))

  expect_true(identical.integer64(
    as.integer64(min(xd)),
    min(xi64)
  ))
  expect_true(identical.integer64(
    as.integer64(min(xd, na.rm=TRUE)),
    min(xi64, na.rm=TRUE)
  ))
  expect_true(identical.integer64(
    as.integer64(min(xd)),
    min(xi64)
  ))
  expect_true(identical.integer64(
    as.integer64(min(xd, na.rm=TRUE)),
    min(xi64, na.rm=TRUE)
  ))
  expect_true(identical.integer64(
    as.integer64(min(2.0, 3.0, NA)),
    min(as.integer64(2L), 3.0, NA)
  ))
  expect_true(identical.integer64(
    as.integer64(min(2.0, 3.0, NA, na.rm=TRUE)),
    min(as.integer64(2L), 3.0, NA, na.rm=TRUE)
  ))
  expect_true(identical.integer64(
    as.integer64(min(2.0, 3.0, NA)),
    min(as.integer64(2L), 3.0, NA)
  ))
  expect_true(identical.integer64(
    as.integer64(min(2.0, 3.0, NA, na.rm=TRUE)),
    min(as.integer64(2L), 3.0, NA, na.rm=TRUE)
  ))

  expect_true(identical.integer64(
    as.integer64(max(xd)),
    max(xi64)
  ))
  expect_true(identical.integer64(
    as.integer64(max(xd, na.rm=TRUE)),
    max(xi64, na.rm=TRUE)
  ))
  expect_true(identical.integer64(
    as.integer64(max(xd)),
    max(xi64)
  ))
  expect_true(identical.integer64(
    as.integer64(max(xd, na.rm=TRUE)),
    max(xi64, na.rm=TRUE)
  ))
  expect_true(identical.integer64(
    as.integer64(max(2.0, 3.0, NA)),
    max(as.integer64(2L), 3.0, NA)
  ))
  expect_true(identical.integer64(
    as.integer64(max(2.0, 3.0, NA, na.rm=TRUE)),
    max(as.integer64(2L), 3.0, NA, na.rm=TRUE)
  ))
  expect_true(identical.integer64(
    as.integer64(max(2.0, 3.0, NA)),
    max(as.integer64(2L), 3.0, NA)
  ))
  expect_true(identical.integer64(
    as.integer64(max(2.0, 3.0, NA, na.rm=TRUE)),
    max(as.integer64(2L), 3.0, NA, na.rm=TRUE)
  ))

  expect_true(identical.integer64(
    as.integer64(range(xd)),
    range(xi64)
  ))
  expect_true(identical.integer64(
    as.integer64(range(xd, na.rm=TRUE)),
    range(xi64, na.rm=TRUE)
  ))
  expect_true(identical.integer64(
    as.integer64(range(xd)),
    range(xi64)
  ))
  expect_true(identical.integer64(
    as.integer64(range(xd, na.rm=TRUE)),
    range(xi64, na.rm=TRUE)
  ))
  expect_true(identical.integer64(
    as.integer64(range(2.0, 3.0, NA)),
    range(as.integer64(2L), 3.0, NA)
  ))
  expect_true(identical.integer64(
    as.integer64(range(2.0, 3.0, NA, na.rm=TRUE)),
    range(as.integer64(2L), 3.0, NA, na.rm=TRUE)
  ))
})

test_that("Cumulative functions", {
  xd = c(2.0, 3.0, NA, 1.0, 4.0)
  xi64 = as.integer64(xd)

  expect_true(identical.integer64(cumsum(xi64), as.integer64(cumsum(xd))))
  expect_true(identical.integer64(cumprod(xi64), as.integer64(cumprod(xd))))
  expect_true(identical.integer64(cummin(xi64), as.integer64(cummin(xd))))
  expect_true(identical.integer64(cummax(xi64), as.integer64(cummax(xd))))
})

test_that("diff", {
  d64 = diffinv(rep(.Machine$integer.max, 100L), lag=2.0, differences=2L)
  i64 = as.integer64(d64)
  expect_identical(
    diff(d64, lag=2L, differences=2L),
    as.double(diff(i64, lag=2L, differences=2L))
  )
})
