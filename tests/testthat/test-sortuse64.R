with_parameters_test_that("sortfin works", method=1:3, {
  x = as.integer64(1:10)
  r = sample(x) # NB: default method assumes 'sorted' is, well, sorted, so this requires method!=1
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
  expect_identical(orderpos(x, idx, 0:1, method=method), c(NA_integer_, 1L))

  # These were segfaulting due to length(order) < length(table)
  table = as.integer64(c(10, 20, 30, 5, 15, 25))
  order = c(4, 1, 5, 2, 6, 3) # order of table is 5, 10, 15, 20, 25, 30

  partial_order = c(4, 1, 5, 2) # order of subset of table is 5, 10, 15, 20

  x_search = as.integer64(c(5, 10, 15, 20, 25, 30, 99))

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

  # user's example, where `table[order]` is not sorted ascending.
  # The C functions expect ascending order, so they will fail to find the values.
  # The test confirms the functions no longer segfault and return a consistent result.
  expect_identical(orderfin(as.integer64(10:1), 1:3, 8:11, method=method), rep(FALSE, 4L))
  expect_identical(orderpos(as.integer64(10:1), 1:3, 8:11, method=method), rep(NA_integer_, 4L))
})

test_that("ordertab and orderdup work", {
  x = as.integer64(1:10)
  x = c(x, x[1:8], x[1:6])
  idx = order(x)

  expect_identical(ordertab(x, idx, 10L), rep(3:1, c(6L, 2L, 2L)))
  expect_identical(orderdup(x, idx), rep(c(FALSE, TRUE), c(10L, 14L)))
  expect_identical(orderdup(x, idx, method=2L), rep(c(FALSE, TRUE), c(10L, 14L)))
})
