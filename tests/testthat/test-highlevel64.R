test_that("match & %in% basics work", {
  x = as.integer64(2:5)
  y = as.integer64(3:6)
  expect_identical(match(x, y), c(NA, 1:3))
  expect_identical(match(y, x), c(2:4, NA))

  expect_identical(match(2:5, y), c(NA, 1:3))
  expect_identical(match(as.numeric(2:5), y), c(NA, 1:3))
  expect_identical(match(y, 2:5), c(2:4, NA))
  expect_identical(match(y, as.numeric(2:5)), c(2:4, NA))

  expect_identical(match(x, y, nomatch=0L), 0:3)

  expect_identical(x %in% y, c(FALSE, TRUE, TRUE, TRUE))
  expect_identical(y %in% x, c(TRUE, TRUE, TRUE, FALSE))
  expect_identical(x %in% 3:6, c(FALSE, TRUE, TRUE, TRUE))
  expect_identical(x %in% c(3.0, 4.0, 5.0, 6.0), c(FALSE, TRUE, TRUE, TRUE))
})

test_that("Different method= for match() and %in% work", {
  x = as.integer64(2:5)
  y = as.integer64(3:6)
  expected = c(NA_integer_, 1:3)

  expect_identical(match(x, y, method="hashpos"), expected)
  expect_identical(match(x, y, method="hashrev"), expected)
  expect_identical(match(x, y, method="sortorderpos"), expected)
  expect_error(match(x, y, method="_unknown_"), "'arg' should be one of", fixed=TRUE)
  # TODO(#58): Fix this, currently fails.
  # expect_identical(match(x, y, method="orderpos"), expected)

  # NB: %in% is quite a bit different; while there's a public API to
  #   `%in%.integer64`, likely, there shouldn't be (it's strange to export
  #   an S3 method like is currently done). The tests are designed to tickle
  #   the different methods through the public API only; this makes them
  #   prone to winding up testing something totally different later. I think
  #   that's fine; now that we have coverage tests up, any refactor that bumps
  #   around what exactly the following tests are covering, will show up in the PR.

  # method="hashrin" used when x is "short" but table is "long"
  x = as.integer64(seq_len(10L))
  table = as.integer64(seq_len(2.0**16.0 * 2.0/3.0 + 10.0)) # invert condition for bx>=16, 10.0 arbitrary buffer
  expect_identical(x %in% table, rep(TRUE, 10L))
})

# TODO(#59): Don't call table.integer64() directly.
test_that("duplicated, unique, table methods work", {
  x = as.integer64(1:3)
  expect_identical(duplicated(x), rep(FALSE, 3L))
  expect_identical(unique(x), x)
  expect_identical(table.integer64(x), table(x = 1:3))

  x = as.integer64(rep(1L, 3L))
  expect_identical(duplicated(x), c(FALSE, TRUE, TRUE))
  expect_identical(unique(x), x[1L])
  expect_identical(table.integer64(x), table(x = rep(1L, 3L)))

  x = as.integer64(c(1L, 2L, 1L))
  expect_identical(duplicated(x), c(FALSE, FALSE, TRUE))
  expect_identical(unique(x), x[1:2])
  expect_identical(table.integer64(x), table(x = c(1L, 2L, 1L)))

  x = as.integer64(c(1L, 1L, 2L))
  expect_identical(duplicated(x), c(FALSE, TRUE, FALSE))
  expect_identical(unique(x), x[c(1L, 3L)])
  expect_identical(table.integer64(x), table(x = c(1L, 1L, 2L)))

  expect_error(duplicated(x, method="_unknown_"), "'arg' should be one of", fixed=TRUE)
  expect_error(unique(x, method="_unknown_"), "'arg' should be one of", fixed=TRUE)
})

test_that("different method= for duplicated, unique work", {
  x = as.integer64(c(1L, 2L, 1L))
  exp_dup = c(FALSE, FALSE, TRUE)
  exp_unq = x[1:2]

  expect_identical(duplicated(x, method="hashdup"), exp_dup)
  expect_identical(unique(x, method="hashmapuni"), exp_unq)
  expect_identical(unique(x, method="hashuni"), exp_unq)

  expect_identical(duplicated(x, method="sortorderdup"), exp_dup)
  expect_identical(unique(x, method="sortorderuni"), exp_unq)
  expect_identical(unique(x, method="sortuni"), exp_unq)

  # TODO(#58): Fix this, currently fails.
  # expect_identical(duplicated(x, method="orderdup"), exp_dup)
  expect_identical(unique(x, method="orderuni"), exp_unq)
})

test_that("more coercion works", {
  expect_identical(as.factor(as.integer64(2:4)), factor(2:4))
  expect_identical(as.ordered(as.integer64(2:4)), as.ordered(2:4))
  expect_identical(as.integer64(factor(2:11)), as.integer64(1:10)) # NB: _not_ 2:11!
})

test_that("sorting methods work", {
  x = as.integer64(c(10L, 4L, 8L))
  x_rank = c(3.0, 1.0, 2.0)
  expect_identical(rank(x), x_rank)
  expect_identical(rank(x, method="orderrnk"), x_rank)

  x = as.integer64(1:100)
  q = as.integer64(c(1L, 26L, 50L, 75L, 100L))
  expect_identical(quantile(x, names=FALSE), q)
  expect_identical(median(x), q[3L])
  names(q) = c('0%', '25%', '50%', '75%', '100%')
  expect_identical(quantile(x), q)
  expect_identical(quantile(x, 0.2, names=FALSE), as.integer64(21L))
  expect_identical(median(NA_integer64_, na.rm=FALSE), NA_integer64_)
  expect_identical(median(integer64()), NA_integer64_)

  expect_error(quantile(x, type=7L), "only.*qtile.*supported")
  expect_error(quantile(NA_integer64_), "missing values not allowed")

  x = as.integer64(1:100)
  q = as.integer64(c(1L, 26L, 50L, 75L, 100L))
  names(q) = c('0%', '25%', '50%', '75%', '100%')
  expect_identical(qtile(x, method="sortqtl"), q)
  expect_identical(qtile(x, method="orderqtl"), q)

  x = as.integer64(c(1L, 1L, 2L, 3L, 2L, 4L))
  x_tiepos = c(1L, 2L, 3L, 5L)
  expect_identical(tiepos(x), x_tiepos)
  expect_identical(tiepos(x, method="ordertie"), x_tiepos)

  expect_error(rank(x, method="_unknown_"), "'arg' should be one of", fixed=TRUE)
  expect_error(qtile(x, method="_unknown_"), "'arg' should be one of", fixed=TRUE)
  expect_error(tiepos(x, method="_unknown_"), "'arg' should be one of", fixed=TRUE)
})

# These tests were previously kept as tests under \examples{\dontshow{...}}.
#   Converted to "proper" unit tests for clarity, after making them more
#   canonical within {testthat}, e.g. better capturing expected warnings,
#   changing stopifnot(identical(...)) to expect_identical(...).
test_that("Old \\dontshow{} tests continue working", {
  xi = c(1L, 1L, 2L)
  xi64 = as.integer64(xi)
  yi = c(3L, 4L, 4L)
  yi64 = as.integer64(yi)

  t_xi = table(x=xi)
  t_xi_yi = table(x=xi, y=yi)

  expect_identical(table.integer64(x=xi64), t_xi)
  expect_identical(table.integer64(x=xi64, y=yi64), t_xi_yi)

  expect_warning(
    expect_identical(table.integer64(x=xi), t_xi),
    "coercing first argument to integer64",
    fixed = TRUE
  )
  expect_warning(
    expect_identical(table.integer64(x=xi64, y=yi), t_xi_yi),
    "coercing argument 2 to integer64",
    fixed = TRUE
  )
  expect_warning(
    expect_identical(table.integer64(x=xi, y=yi64), t_xi_yi),
    "coercing argument 1 to integer64",
    fixed = TRUE
  )

  expect_identical(table(x=xi64), t_xi)
  expect_identical(table(x=xi64, y=yi64), t_xi_yi)
  expect_identical(table(x=xi64, y=yi), t_xi_yi)
  expect_identical(table(x=xi, y=yi64), t_xi_yi)
})

test_that("unipos() works as intended", {
  x = as.integer64(c(1L, 2L, 1L, 3L, 2L, 4L))
  x_unipos = c(1L, 2L, 4L, 6L)
  expect_identical(unipos(x), x_unipos)
  expect_identical(unipos(x, method="hashupo"), x_unipos)
  expect_identical(unipos(x, method="sortorderupo"), x_unipos)
  expect_identical(unipos(x, method="orderupo"), x_unipos)
  expect_error(unipos(x, method="_unknown_"), "'arg' should be one of", fixed=TRUE)
})

test_that("keypos() works as intended", {
  x = as.integer64(c(5L, 2L, 5L, 3L, 2L, 4L))
  x_keypos = c(4L, 1L, 4L, 2L, 1L, 3L)
  expect_identical(keypos(x), x_keypos)
  expect_identical(keypos(x, method="orderkey"), x_keypos)
  expect_error(keypos(x, method="_unknown_"), "'arg' should be one of", fixed=TRUE)
})

test_that("summary() works as intended", {
  x = as.integer64(c(1L, 2L, 10L, 20L, NA, 30L))
  # NB: as.integer64() strips names, so as.integer64(c(Min. = ...)) won't work
  x_summary = as.integer64(c(1L, 2L, 10L, 12L, 20L, 30L, 1L))
  names(x_summary) = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "NA's")
  expect_identical(summary(x), x_summary)
  expect_identical(summary(x[-5L]), x_summary[-7L])
})

test_that("prank() works as intended", {
  x = as.integer64(1:100)
  expect_identical(prank(x), (x-1.0)/99.0)
  expect_identical(prank(x[1L]), NA_integer64_)
})