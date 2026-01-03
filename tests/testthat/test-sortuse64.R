with_parameters_test_that("sortfin works", method=1:3, {
  x = as.integer64(1:10)
  r = sample(x)
  expect_identical(sortfin(integer64(), 1:10, method=method), rep(FALSE, 10L))
  expect_identical(sortfin(x, integer64(), method=method), logical())

  expect_true(all(sortfin(x, r, method=method)))

  expect_true(sortfin(x, 1L, method=method))
  expect_true(all(sortfin(x, c(1.0, 4.0), method=method)))
  expect_identical(sortfin(x, as.integer64(0:1), method=method), c(FALSE, TRUE))
  # only 'x' need be sorted
  expect_identical(sortfin(x, as.integer64(1:0), method=method), c(TRUE, FALSE))
})

with_parameters_test_that("orderfin and orderpos work", method=1:3, {
  x = as.integer64(1:10)
  idx = seq_along(x)

  expect_identical(orderfin(x, idx, 0:1, method=method), c(FALSE, TRUE))
  expect_identical(orderfin(x, idx, as.integer64(0:1), method=method), c(FALSE, TRUE))

  expect_identical(orderpos(x, idx, 0:1, method=method), c(NA_integer_, 1L))
  expect_identical(orderpos(x, idx, as.integer64(0:1), method=method), c(NA_integer_, 1L))

  # These were segfaulting due to length(order) < length(table)
  table = as.integer64(c(10L, 20L, 30L, 5L, 15L, 25L))
  order = c(4L, 1L, 5L, 2L, 6L, 3L) # order of table is 5, 10, 15, 20, 25, 30

  partial_order = c(4L, 1L, 5L, 2L) # order of subset of table is 5, 10, 15, 20

  x_search = as.integer64(c(5L, 10L, 15L, 20L, 25L, 30L, 99L))

  # reference with full order
  full_fin = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
  full_pos = c(4L, 1L, 5L, 2L, 6L, 3L, NA_integer_)
  expect_identical(orderfin(table, order, x_search, method=method), full_fin)
  expect_identical(orderpos(table, order, x_search, method=method), full_pos)

  # with partial order, we search in a subset
  partial_fin = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  partial_pos = c(4L, 1L, 5L, 2L, NA_integer_, NA_integer_, NA_integer_)
  expect_identical(orderfin(table, partial_order, x_search, method=method), partial_fin)
  expect_identical(orderpos(table, partial_order, x_search, method=method), partial_pos)

  expect_error(
    orderfin(as.integer64(10:1), 1:3, 8:11, method=method),
    "'table' is not sorted by 'order'",
    fixed = TRUE
  )
  expect_error(
    orderpos(as.integer64(10:1), 1:3, 8:11, method=method),
    "'table' is not sorted by 'order'",
    fixed = TRUE
  )
})

test_that("ordertab and orderdup work", {
  x = as.integer64(1:10)
  x = c(x, x[1:8], x[1:6])
  idx = order(x)

  expect_identical(ordertab(x, idx, 10L), rep(3:1, c(6L, 2L, 2L)))
  expect_identical(orderdup(x, idx), rep(c(FALSE, TRUE), c(10L, 14L)))
  expect_identical(orderdup(x, idx, method=2L), rep(c(FALSE, TRUE), c(10L, 14L)))
})

