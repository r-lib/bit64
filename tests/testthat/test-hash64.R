test_that("runif64 behaves as expected", {
  withr::local_seed(3478)

  expect_identical(
    runif64(10L),
    as.integer64(c(
      "6312937654860439830", "5047107428523623805", "7829389831893364707",
      "-3641910282010306573", "4600438248413496767", "4871064969903669683",
      "2693636032523872093", "4503042760826424596", "-8860474785465525016", "-4614238549190155011"
    ))
  )

  expect_identical(
    runif64(5L, 10L, 20L),
    as.integer64(c(16L, 19L, 16L, 15L, 20L))
  )

  # large enough number to be confident the test isn't "randomly" succeeding,
  #   but not so large as to noticeably slow down the suite.
  x = runif64(100000L, -5L, 5L)
  expect_true(all(x >= -5L & x <= 5L))
})
