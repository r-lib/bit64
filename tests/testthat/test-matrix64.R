test_that("matrix works still on simple integer input", {
  x = as.integer(1:10)

  expect_identical(class(matrix(x))[1L], "matrix")
  expect_no_warning(expect_identical(matrix(x)[seq_along(x)], x))
  expect_no_warning(expect_identical(dim(matrix(x)), c(10L, 1L)))
  expect_no_warning(expect_identical(matrix(x, byrow=TRUE)[seq_along(x)], x))
  expect_no_warning(expect_identical(dim(matrix(x, byrow=TRUE)), c(10L, 1L)))

  expect_no_warning(expect_identical(matrix(x, nrow=2)[seq_along(x)], x))
  expect_no_warning(expect_identical(dim(matrix(x, nrow=2)), c(2L, 5L)))
  expect_no_warning(expect_identical(matrix(x, nrow=2, byrow=TRUE)[seq_along(x)], x[c(1,6,2,7,3,8,4,9,5,10)]))
  expect_no_warning(expect_identical(dim(matrix(x, nrow=2, byrow=TRUE)), c(2L, 5L)))
  expect_no_warning(expect_identical(matrix(NA_integer_, nrow=2, ncol=1)[1:2], c(NA_integer_, NA_integer_)))
  expect_no_warning(expect_identical(dim(matrix(NA_integer_, nrow=2, ncol=1)), c(2L, 1L)))
  expect_no_warning(expect_identical(matrix(integer(), nrow=2, ncol=1)[1:2], c(NA_integer_, NA_integer_)))
  expect_no_warning(expect_identical(dim(matrix(integer(), nrow=2, ncol=1)), c(2L, 1L)))
  expect_no_warning(expect_identical(dimnames(matrix(x, 2, dimnames=list(NULL, letters[1:5]))), list(NULL, letters[1:5])))
  expect_no_warning(expect_identical(dimnames(matrix(x, 2, dimnames=list(LETTERS[1:2]))), list(LETTERS[1:2], NULL)))
  expect_no_warning(expect_identical(dimnames(matrix(x, 2, dimnames=list(LETTERS[1:2], letters[1:5]))), list(LETTERS[1:2], letters[1:5])))
  expect_warning(expect_identical(matrix(x, nrow=2, ncol=6)[seq_len(2*6)], c(x, x[1:2])), "data length [[]10] is not a sub-multiple or multiple of the number of columns [[]6]")
  expect_warning(expect_identical(dim(matrix(x, nrow=2, ncol=6)), c(2L, 6L)), "data length [[]10] is not a sub-multiple or multiple of the number of columns [[]6]")
  expect_warning(expect_identical(matrix(x, nrow=2, ncol=3)[seq_len(2*3)], x[seq_len(2*3)]), "data length [[]10] is not a sub-multiple or multiple of the number of columns [[]3]")
  expect_warning(expect_identical(dim(matrix(x, nrow=2, ncol=3)), c(2L, 3L)), "data length [[]10] is not a sub-multiple or multiple of the number of columns [[]3]")
  expect_warning(expect_identical(matrix(x, nrow=3, ncol=2)[seq_len(3*2)], x[seq_len(3*2)]), "data length [[]10] is not a sub-multiple or multiple of the number of rows [[]3]")
  expect_error(matrix(x, nrow=-1), "invalid 'nrow' value")
  expect_error(matrix(x, ncol=-1), "invalid 'ncol' value")

})

test_that("matrix works on simple integer64 input", {
  x = as.integer64(1:10)

  expect_s3_class(matrix(x), "integer64")
  expect_no_warning(expect_identical(matrix(x)[seq_along(x)], x))
  expect_no_warning(expect_identical(dim(matrix(x)), c(10L, 1L)))
  expect_no_warning(expect_identical(matrix(x, byrow=TRUE)[seq_along(x)], x))
  expect_no_warning(expect_identical(dim(matrix(x, byrow=TRUE)), c(10L, 1L)))

  expect_no_warning(expect_identical(matrix(x, nrow=2)[seq_along(x)], x))
  expect_no_warning(expect_identical(dim(matrix(x, nrow=2)), c(2L, 5L)))
  expect_no_warning(expect_identical(matrix(x, nrow=2, byrow=TRUE)[seq_along(x)], x[c(1,6,2,7,3,8,4,9,5,10)]))
  expect_no_warning(expect_identical(dim(matrix(x, nrow=2, byrow=TRUE)), c(2L, 5L)))
  expect_no_warning(expect_identical(matrix(NA_integer64_, nrow=2, ncol=1)[1:2], c(NA_integer64_, NA_integer64_)))
  expect_no_warning(expect_identical(dim(matrix(NA_integer64_, nrow=2, ncol=1)), c(2L, 1L)))
  expect_no_warning(expect_identical(matrix(integer64(), nrow=2, ncol=1)[1:2], c(NA_integer64_, NA_integer64_)))
  expect_no_warning(expect_identical(dim(matrix(integer64(), nrow=2, ncol=1)), c(2L, 1L)))
  expect_no_warning(expect_identical(dimnames(matrix(x, 2, dimnames=list(NULL, letters[1:5]))), list(NULL, letters[1:5])))
  expect_no_warning(expect_identical(dimnames(matrix(x, 2, dimnames=list(LETTERS[1:2]))), list(LETTERS[1:2], NULL)))
  expect_no_warning(expect_identical(dimnames(matrix(x, 2, dimnames=list(LETTERS[1:2], letters[1:5]))), list(LETTERS[1:2], letters[1:5])))
  expect_warning(expect_identical(matrix(x, nrow=2, ncol=6)[seq_len(2*6)], c(x, x[1:2])), "data length [[]10] is not a sub-multiple or multiple of the number of columns [[]6]")
  expect_warning(expect_identical(dim(matrix(x, nrow=2, ncol=6)), c(2L, 6L)), "data length [[]10] is not a sub-multiple or multiple of the number of columns [[]6]")
  expect_warning(expect_identical(matrix(x, nrow=2, ncol=3)[seq_len(2*3)], x[seq_len(2*3)]), "data length [[]10] is not a sub-multiple or multiple of the number of columns [[]3]")
  expect_warning(expect_identical(dim(matrix(x, nrow=2, ncol=3)), c(2L, 3L)), "data length [[]10] is not a sub-multiple or multiple of the number of columns [[]3]")
  expect_warning(expect_identical(matrix(x, nrow=3, ncol=2)[seq_len(3*2)], x[seq_len(3*2)]), "data length [[]10] is not a sub-multiple or multiple of the number of rows [[]3]")
  expect_error(matrix(x, nrow=-1), "invalid 'nrow' value")
  expect_error(matrix(x, ncol=-1), "invalid 'ncol' value")

})


test_that("array works still on simple integer input", {
  x = as.integer(1:10)

  expect_identical(class(array(x)), "array")
  expect_no_warning(expect_identical(array(x)[seq_along(x)], structure(x, dim = length(x))))
  expect_no_warning(expect_identical(dim(array(x)), c(10L)))
  expect_no_warning(expect_identical(array(x, c(2,5))[seq_along(x)], x))
  expect_no_warning(expect_identical(dim(array(x, c(2,5))), c(2L,5L)))
  expect_no_warning(expect_identical(array(x, c(1,2,3))[seq_len(1*2*3)], rep_len(x, 1*2*3)))
  expect_no_warning(expect_identical(dim(array(x, c(1,2,3))), c(1L,2L,3L)))
  expect_no_warning(expect_identical(array(x, c(3,2,3))[seq_len(3*2*3)], rep_len(x, 3*2*3)))
  expect_no_warning(expect_identical(array(NA_integer_, c(2,1))[1:2], c(NA_integer_, NA_integer_)))
  expect_no_warning(expect_identical(array(integer(), c(2,1))[1:2], c(NA_integer_, NA_integer_)))
  expect_no_warning(expect_identical(dimnames(array(x, c(2,5), dimnames=list(NULL, letters[1:5]))), list(NULL, letters[1:5])))
  expect_no_warning(expect_identical(dimnames(array(x, c(2,5), dimnames=list(LETTERS[1:2]))), list(LETTERS[1:2], NULL)))
  expect_no_warning(expect_identical(dimnames(array(x, c(2,5), dimnames=list(LETTERS[1:2], letters[1:5]))), list(LETTERS[1:2], letters[1:5])))
  expect_error(array(x, dim=NULL), "'dims?' cannot be of length 0")
  expect_error(array(x, dim=-1), "negative length vectors are not allowed")
  expect_no_warning(expect_identical(array(x, dim=0),  structure(integer(), dim = 0L)))

})

test_that("array works on simple integer64 input", {
  x = as.integer64(1:10)

  expect_s3_class(array(x), "integer64")
  expect_no_warning(expect_identical(array(x)[seq_along(x)], structure(x, dim=length(x))))
  expect_no_warning(expect_identical(dim(array(x)), c(10L)))
  expect_no_warning(expect_identical(array(x, c(2,5))[seq_along(x)], x))
  expect_no_warning(expect_identical(dim(array(x, c(2,5))), c(2L,5L)))
  expect_no_warning(expect_identical(array(x, c(1,2,3))[seq_len(1*2*3)], x[1:6]))
  expect_no_warning(expect_identical(dim(array(x, c(1,2,3))), c(1L,2L,3L)))
  expect_no_warning(expect_identical(array(x, c(3,2,3))[seq_len(3*2*3)], c(x, x[1:8])))
  expect_no_warning(expect_identical(array(NA_integer64_, c(2,1))[1:2], c(NA_integer64_, NA_integer64_)))
  expect_no_warning(expect_identical(array(integer64(), c(2,1))[1:2], c(NA_integer64_, NA_integer64_)))
  expect_no_warning(expect_identical(dimnames(array(x, c(2,5), dimnames=list(NULL, letters[1:5]))), list(NULL, letters[1:5])))
  expect_no_warning(expect_identical(dimnames(array(x, c(2,5), dimnames=list(LETTERS[1:2]))), list(LETTERS[1:2], NULL)))
  expect_no_warning(expect_identical(dimnames(array(x, c(2,5), dimnames=list(LETTERS[1:2], letters[1:5]))), list(LETTERS[1:2], letters[1:5])))
  expect_error(array(x, dim=NULL), "'dims?' cannot be of length 0")
  expect_error(array(x, dim=-1), "negative length vectors are not allowed")
  expect_no_warning(expect_identical(array(x, dim=0),  structure(integer64(), dim = 0L)))

})

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

  skip_unless_r(">= 4.0.0") # named args in stopifnot() unsupported -> different error
  expect_error(
    rowSums(A64, dims=4L),
    "invalid 'dims'",
    fixed = TRUE
  )
  expect_error(
    colSums(A64, dims=4L),
    "invalid 'dims'",
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

test_that("dimnames with colSums and rowSums", {
  M32 = matrix(as.integer(1:(3*2)), nrow=3L, ncol=2L, dimnames=list(LETTERS[1:3], letters[1:2]))
  A32 = array(as.integer(1:(2*5*3)), dim=c(2, 5, 3), dimnames=list(LETTERS[1:2], letters[1:5], rev(LETTERS)[1:3]))
  M64 = matrix(as.integer64(1:(3*2)), nrow=3L, ncol=2L, dimnames=list(LETTERS[1:3], letters[1:2]))
  A64 = array(as.integer64(1:(2*5*3)), dim=c(2, 5, 3), dimnames=list(LETTERS[1:2], letters[1:5], rev(LETTERS)[1:3]))

  expect_no_warning(expect_identical(names(colSums(M32)), names(colSums(M64))))
  expect_no_warning(expect_identical(dimnames(colSums(M32)), dimnames(colSums(M64))))
  expect_no_warning(expect_identical(names(colSums(A32)), names(colSums(A64))))
  expect_no_warning(expect_identical(dimnames(colSums(A32)), dimnames(colSums(A64))))
  expect_no_warning(expect_identical(names(colSums(A32, dims=2L)), names(colSums(A64, dims=2L))))
  expect_no_warning(expect_identical(dimnames(colSums(A32, dims=2L)), dimnames(colSums(A64, dims=2L))))
  expect_no_warning(expect_identical(names(rowSums(M32)), names(rowSums(M64))))
  expect_no_warning(expect_identical(dimnames(rowSums(M32)), dimnames(rowSums(M64))))
  expect_no_warning(expect_identical(names(rowSums(A32)), names(rowSums(A64))))
  expect_no_warning(expect_identical(dimnames(rowSums(A32)), dimnames(rowSums(A64))))
  expect_no_warning(expect_identical(names(rowSums(A32, dims=2L)), names(rowSums(A64, dims=2L))))
  expect_no_warning(expect_identical(dimnames(rowSums(A32, dims=2L)), dimnames(rowSums(A64, dims=2L))))
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

test_that("matrix multiplication", {
  skip_unless_r(">= 4.0.0") # it does not work with ubuntu-latest (3.6), because a double vector is returned
  m32 = matrix(1:10, 2)
  m64 = matrix(as.integer64(m32), nrow(m32))
  mDo = matrix(as.numeric(m32), nrow(m32))
  mCo = matrix(as.complex(m32), nrow(m32))
  expect_error(m32%*%m32, "non-conformable arguments")
  expect_error(m64%*%m64, "non-conformable arguments")
  expect_identical(m64%*%t(m64), matrix(as.integer64(m32%*%t(m32)), nrow=nrow(m32)))
  expect_identical(t(m64)%*%m64, matrix(as.integer64(t(m32)%*%m32), ncol=ncol(m32)))
  expect_identical((1:2)%*%m64, matrix(as.integer64((1:2)%*%m32), ncol=ncol(m32)))
  expect_identical(m64%*%(1:5), matrix(as.integer64(m32%*%(1:5)), nrow=nrow(m32)))
  expect_error((1:2)%*%3L, "non-conformable arguments")
  expect_error(as.integer64(1:2)%*%as.integer64(3L), "non-conformable arguments")
  expect_identical(as.integer64(1L)%*%(3:4), matrix(as.integer64(3:4), nrow=1L))
  expect_identical(as.integer64(1:2)%*%(3:4), matrix(as.integer64(11L), nrow=1L))

  expect_identical(m64%*%t(m32), matrix(as.integer64(m32%*%t(m32)), nrow=nrow(m32)))
  expect_identical(m32%*%t(m64), matrix(as.integer64(m32%*%t(m32)), nrow=nrow(m32)))
  expect_identical(m64%*%t(mDo), matrix(as.integer64(m32%*%t(m32)), nrow=nrow(m32)))
  expect_identical(mDo%*%t(m64), matrix(as.integer64(m32%*%t(m32)), nrow=nrow(m32)))
  expect_identical(m64%*%t(mCo), matrix(as.complex(m32%*%t(m32)), nrow=nrow(m32)))
  expect_identical(mCo%*%t(m64), matrix(as.complex(m32%*%t(m32)), nrow=nrow(m32)))

  expect_error(m64%*%LETTERS[1:5], "non-numeric argument to binary operator", fixed=TRUE)
  expect_error(m64%*%as.raw(1:5), "non-numeric argument to binary operator", fixed=TRUE)
  
  # warning in multiplication part
  x = as.integer64("4000000000")
  expect_warning(expect_identical(matrix(x, 1)%*%matrix(x, ncol=1), matrix(NA_integer64_, 1, 1)), "NAs produced by integer64 overflow")
  
  # warning in summation part
  x = rep_len(as.integer64("3000000000"), 2)
  expect_warning(expect_identical(matrix(x, 1)%*%matrix(x, ncol=1), matrix(NA_integer64_, 1, 1)), "NAs produced by integer64 overflow")
  
})


test_that("coercion to matrix and array", {
  
  i32 = 1:10
  i64 = as.integer64(i32)
  m64 = matrix(as.integer64(i32), 2L)

  expect_identical(as.matrix(i64), structure(i64, dim = c(length(i64), 1L)), ignore_attr=if (getRversion() < "4.0.0") "class" else FALSE)
  expect_identical(as.matrix(m64), m64)

  expect_identical(as.array(i64), structure(i64, dim = c(length(i64))), ignore_attr=if (getRversion() < "4.0.0") "class" else FALSE)
  expect_identical(as.array(m64), m64)
})
