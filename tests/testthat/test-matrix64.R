test_that("colSums and rowSums work on simple integer64 input", {
  A = array(seq_len(120L), dim = 2:5)
  A64 = array64(A, dim=dim(A))

  # matches the behavior of sum.integer64 to not become numeric
  expect_s3_class(rowSums(A64), "integer64")
  expect_s3_class(colSums(A64), "integer64")

  expect_int_32_64_equivalent(rowSums(A))
  expect_int_32_64_equivalent(rowSums(A, dims=2L))
  expect_int_32_64_equivalent(rowSums(A, dims=3L))

  expect_int_32_64_equivalent(colSums(A))
  expect_int_32_64_equivalent(colSums(A, dims=2L))
  expect_int_32_64_equivalent(colSums(A, dims=3L))

  skip_if_not_r_version("4.0.0") # named args in stopifnot() unsupported -> different error
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

test_that("All-missing inputs are handled correctly by colSums and rowSums", {
  A64 = matrix64(rep(NA_integer64_, 6L), nrow=3L, ncol=2L)

  expect_identical(rowSums(A64), rep(NA_integer64_, 3L))
  expect_identical(colSums(A64), rep(NA_integer64_, 2L))
})

test_that("out-of-integer-range inputs are handled correctly", {
  A64 = matrix64(2.0^(30:35), nrow=3L, ncol=2L)

  expect_identical(rowSums(A64), as.integer64(2L^30L*c(1L+8L, 2L+16L, 4L+32L)))
  expect_identical(colSums(A64), as.integer64(2L^30L*c(1L+2L+4L, 8L+16L+32L)))
})

test_that("aperm works in simple cases", {
  # example from ?aperm
  A = array64(1:24, 2:4)
  B = aperm(A, c(2L, 1L, 3L))
  # ignore class: t() gives 'array', not easy to delete it/add it to A[...]
  expect_identical(t(B[, , 2L]), A[, , 2L], ignore_attr="class")
  expect_identical(t(B[, , 3L]), A[, , 3L], ignore_attr="class")
  expect_identical(t(B[, , 4L]), A[, , 4L], ignore_attr="class")
})
