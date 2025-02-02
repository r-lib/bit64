with_parameters_test_that("sortfin works", method=1:3, {
  x = as.integer64(1:10)
  r = sample(x) # NB: default method assumes 'sorted' is, well, sorted, so this requires method!=1
  expect_false(any(sortfin(integer64(), x, method=method)))
  expect_identical(sortfin(x, integer64(), method=method), logical())

  expect_true(all(sortfin(x, r)))

  expect_true(sortfin(x, 1L, method=method))
  expect_true(all(sortfin(x, c(1.0, 4.0), method=method)))
  expect_identical(sortfin(x, as.integer64(0:1), method=method), c(FALSE, TRUE))
  # only 'x' need be sorted
  expect_identical(sortfin(x, as.integer64(1:0), method=method), c(TRUE, FALSE))
})
