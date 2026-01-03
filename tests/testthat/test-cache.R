test_that("is.sorted works", {
  expect_true(bit::is.sorted(integer64()))
  expect_true(bit::is.sorted(as.integer64(1:10)))
  expect_false(bit::is.sorted(as.integer64(10:1)))
})
