test_that("basic math ops works", {
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

  # regression snuck through, caught by #149
  expect_identical(as.integer64(1L) * 1:5, as.integer64(1:5))
  expect_identical(1:5 * as.integer64(1L), as.integer64(1:5))
})

test_that("empty inputs give empty outputs for ops", {
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

with_parameters_test_that("ops with different classes in combination with integer64 (returning integer64):", local({

  # TODO(#248): uncomment when fixed: as.integer64(-10L)%%7L = -3L vs. as.integer(-10L)%%7L = 4L
  # x32 = c(-10:-1, 1:10)
  x32 = 1:10
  x64 = as.integer64(x32)
  set.seed(42)
  y = sample(x32)
  eval(str2lang(paste0("y = as.", class, "(y)")))

  eval(str2lang(paste0("test_e = tryCatch(`", operator, "`", "(x32, y), error=conditionMessage)")))
  eval(str2lang(paste0("test_a = tryCatch(`", operator, "`", "(x64, y), error=conditionMessage)")))
  if (operator %in% c("/", "<", "<=", "==", ">=", ">", "!=", "&", "|", "xor"))
    expect_identical(test_a, test_e)
  else 
    expect_identical(test_a, as.integer64(test_e))
  
  eval(str2lang(paste0("test_e = tryCatch(`", operator, "`", "(y, x32), error=conditionMessage)")))
  eval(str2lang(paste0("test_a = tryCatch(`", operator, "`", "(y, x64), error=conditionMessage)")))
  if (operator %in% c("/", "<", "<=", "==", ">=", ">", "!=", "&", "|", "xor"))
    expect_identical(test_a, test_e)
  else 
    expect_identical(test_a, as.integer64(test_e))

  }), 
  .cases=expand.grid(operator=c("+", "-", "*", "/", "^", "%%", "%/%", "<", "<=", "==", ">=", ">", "!=", "&", "|", "xor"), class=c("integer", "double", "logical"))
)

with_parameters_test_that("ops with different classes in combination with integer64 (not returning integer64):", local({

  if (getRversion() >= "4.3.0") {
    # TODO(#248): uncomment when fixed: as.integer64(-10L)%%7L = -3L vs. as.integer(-10L)%%7L = 4L
    # x32 = c(-10:-1, 1:10)
    x32 = 1:10
    x64 = as.integer64(x32)
    set.seed(42)
    y = sample(x32)
    eval(str2lang(paste0("y = as.", class, "(as.double(y)", if (class == "difftime") ", units = \"secs\"", ")")))
    
    eval(str2lang(paste0("test_e = tryCatch(`", operator, "`", "(x32, y), error=conditionMessage)")))
    eval(str2lang(paste0("test_a = tryCatch(`", operator, "`", "(x64, y), error=conditionMessage)")))
    expect_identical(test_a, test_e)
  
    eval(str2lang(paste0("test_e = tryCatch(`", operator, "`", "(y, x32), error=conditionMessage)")))
    eval(str2lang(paste0("test_a = tryCatch(`", operator, "`", "(y, x64), error=conditionMessage)")))
    expect_identical(test_a, test_e)
  }

  }), 
  .cases = expand.grid(operator = c("+", "-", "*", "/", "^", "%%", "%/%", "<", "<=", "==", ">=", ">", "!=", "&", "|", "xor"), class = c("complex", "Date", "POSIXct", "POSIXlt", "difftime"))
)

test_that("!.integer64", {
  x = c(-1:1, NA)
  expect_identical(!as.integer64(x), !x)
})
