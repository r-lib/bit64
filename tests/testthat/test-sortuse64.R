test_that("orderfin and orderpos work with partial order", {
  # These were segfaulting due to length(order) < length(table)
  table = as.integer64(c(10, 20, 30, 5, 15, 25))
  order = c(4, 1, 5, 2, 6, 3) # order of table is 5, 10, 15, 20, 25, 30

  partial_order = c(4, 1, 5, 2) # order of subset of table is 5, 10, 15, 20

  x = as.integer64(c(5, 10, 15, 20, 25, 30, 99))

  # reference with full order
  full_fin = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
  full_pos = c(4L, 1L, 5L, 2L, 6L, 3L, NA_integer_)
  expect_identical(orderfin(table, order, x), full_fin)
  expect_identical(orderpos(table, order, x), full_pos)

  # with partial order, we search in a subset
  partial_fin = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  partial_pos = c(4L, 1L, 5L, 2L, NA_integer_, NA_integer_, NA_integer_)
  expect_identical(orderfin(table, partial_order, x), partial_fin)
  expect_identical(orderpos(table, partial_order, x), partial_pos)

  # user's example, table is not sorted ascending with order
  # just check for no error
  expect_identical(orderfin(as.integer64(10:1), 1:3, 8:11, method=1L), c(TRUE, TRUE, TRUE, FALSE))
  expect_identical(orderpos(as.integer64(10:1), 1:3, 8:11, method=1L), c(1L, 2L, 3L, NA_integer_))
})
