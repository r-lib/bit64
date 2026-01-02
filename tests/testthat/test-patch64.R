test_that("base generic overwrites work", {
  x = c(2L, 4L, 3L)
  expect_identical(rank(x), c(1.0, 3.0, 2.0))
  expect_identical(order(x), c(1L, 3L, 2L))
})

# These tests were previously kept as tests under \examples{\dontshow{...}}.
#   Converted to "proper" unit tests for clarity, after making them more
#   canonical within {testthat}, e.g. better capturing expected warnings,
#   changing stopifnot(identical(...)) to expect_identical(...).
test_that("Old \\dontshow{} tests continue working", {
  expect_identical(match(as.integer64(2L), as.integer64(0:3)), match(2L, 0:3))
  expect_identical(as.integer64(2L) %in% as.integer64(0:3), 2L %in% 0:3)

  xi = c(1L, 1L, 2L)
  xi64 = as.integer64(xi)
  yi = c(3L, 4L, 4L)
  yi64 = as.integer64(yi)
  zi = c(1L, NA_integer_, 2L)
  zi64 = as.integer64(zi)

  expect_identical(unique(xi64), as.integer64(unique(xi)))
  expect_identical(rank(xi64), rank(xi))

  expect_identical(table(x=xi64), table(x=xi))
  expect_identical(table(x=xi64, y=yi64), table(x=xi, y=yi))
  expect_warning(
    expect_identical(table(x=xi64, y=yi), table(x=xi, y=yi)),
    "coercing argument 2 to integer64",
    fixed = TRUE
  )
  expect_warning(
    expect_identical(table(x=xi, y=yi64), table(x=xi, y=yi)),
    "coercing argument 1 to integer64",
    fixed = TRUE
  )

  expect_identical(order(zi64), order(zi))
  expect_identical(order(zi64, decreasing=TRUE), order(zi, decreasing=TRUE))
})
