test_that("is.sorted works", {
  expect_true(is.sorted(integer64()))
  expect_true(is.sorted(as.integer64(1:10)))
  expect_false(is.sorted(as.integer64(10:1)))
})
