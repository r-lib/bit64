test_that("colSums and rowSums work on simple integer64 input", {
  A = array(seq_len(120L), dim = 2:5)
  A64 = array64(A, dim=dim(A))

  expect_int_32_64_equivalent(rowSums(A))
  expect_int_32_64_equivalent(rowSums(A, dims=2L))
  expect_int_32_64_equivalent(rowSums(A, dims=3L))

  expect_int_32_64_equivalent(colSums(A))
  expect_int_32_64_equivalent(colSums(A, dims=2L))
  expect_int_32_64_equivalent(colSums(A, dims=3L))

  expect_error(
    rowSums(A64, dims=4L),
    "dims= should be a length-1 integer",
    fixed = TRUE
  )
  expect_error(
    colSums(A64, dims=4L),
    "dims= should be a length-1 integer",
    fixed = TRUE
  )
})

test_that("colSums and rowSums work in presence of missing", {
  A = array(seq_len(120L), dim = 2:5)
  A[1L, 1L, 1L, 1L] = NA_integer_

  expect_int_32_64_equivalent(rowSums(A))
  expect_int_32_64_equivalent(rowSums(A, dims=2L))
  expect_int_32_64_equivalent(rowSums(A, dims=3L))

  expect_int_32_64_equivalent(colSums(A))
  expect_int_32_64_equivalent(colSums(A, dims=2L))
  expect_int_32_64_equivalent(colSums(A, dims=3L))

  expect_int_32_64_equivalent(rowSums(A, na.rm=TRUE))
  expect_int_32_64_equivalent(rowSums(A, na.rm=TRUE, dims=2L))
  expect_int_32_64_equivalent(rowSums(A, na.rm=TRUE, dims=3L))

  expect_int_32_64_equivalent(colSums(A, na.rm=TRUE))
  expect_int_32_64_equivalent(colSums(A, na.rm=TRUE, dims=2L))
  expect_int_32_64_equivalent(colSums(A, na.rm=TRUE, dims=3L))
})
