test_that("match basics work", {
  x = as.integer64(2:5)
  y = as.integer64(3:6)
  expect_identical(match(x, y), c(NA, 1:3))
  expect_identical(match(y, x), c(2:4, NA))

  expect_identical(match(2:5, y), c(NA, 1:3))
  expect_identical(match(as.numeric(2:5), y), c(NA, 1:3))
  expect_identical(match(y, 2:5), c(2:4, NA))
  expect_identical(match(y, as.numeric(2:5)), c(2:4, NA))

  expect_identical(match(x, y, nomatch=0L), 0:3)
})
