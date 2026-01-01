test_that("substitute() invocation of generic doesn't warn during transition", {
  x = as.integer64(1:10)
  expect_no_warning(
    expect_identical(
      eval(substitute(f(x, y), list(f = `:`, x = x[1L], y = x[10L]))),
      x
    )
  )
})
