test_that("sortfin works", {
  x = as.integer64(1:10)
  r = sample(x) # NB: default method assumes 'sorted' is, well, sorted, so this requires method!=1
  for (method in 1:3) {
    # TODO(#164): restore this test; currently segfaults
    # expect_false(any(sortfin(integer64(), x, method=method)))
    expect_identical(sortfin(x, integer64(), method=method), logical())
  
    expect_true(all(sortfin(x, r)))
  
    expect_true(sortfin(x, 1L, method=method))
    expect_true(all(sortfin(x, c(1.0, 4.0), method=method)))
    expect_identical(sortfin(x, as.integer64(0:1), method=method), c(FALSE, TRUE))
    # only 'x' need be sorted
    expect_identical(sortfin(x, as.integer64(1:0), method=method), c(TRUE, FALSE))
  }
})

test_that("orderfin and orderpos work", {
  x = as.integer64(1:10)
  idx = seq_along(x)

  expect_identical(orderfin(x, idx, 0:1), c(FALSE, TRUE))
  expect_identical(orderfin(x, idx, as.integer64(0:1), method=2L), c(FALSE, TRUE))
  expect_identical(orderfin(x, idx, as.integer64(0:1), method=3L), c(FALSE, TRUE))

  expect_identical(orderpos(x, idx, 0:1), c(NA_integer_, 1L))
  expect_identical(orderpos(x, idx, as.integer64(0:1), method=2L), c(NA_integer_, 1L))
  expect_identical(orderpos(x, idx, as.integer64(0:1), method=3L), c(NA_integer_, 1L))
})

test_that("ordertab and orderdup work", {
  x = as.integer64(1:10)
  x = c(x, x[1:8], x[1:6])
  idx = order(x)

  expect_identical(ordertab(x, idx, 10L), rep(3:1, c(6L, 2L, 2L)))
  expect_identical(orderdup(x, idx), rep(c(FALSE, TRUE), c(10L, 14L)))
  expect_identical(orderdup(x, idx, method=2L), rep(c(FALSE, TRUE), c(10L, 14L)))
})
