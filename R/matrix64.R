#' Working with integer64 arrays and matrices
#'
#' These functions and methods facilitate working with integer64
#'   objects stored in matrices. As ever, the primary motivation
#'   for having tailor-made functions here is that R's methods
#'   often receive input from bit64 and treat the vectors as doubles,
#'   leading to unexpected and/or incorrect results.
#'
#' As of now, the `colSums()` and `rowSums()` methods are implemented
#'   as wrappers around equivalent `apply()` approaches, because
#'   re-using the default routine (and then applying integer64 to the
#'   result) does not work for objects with missing elements. Ideally
#'   this would eventually get its own dedicated C routine mimicking
#'   that of `colSums()` for integers; feature requests and PRs welcome.
#'
#' `aperm()` is required for `apply()` to work, in general, otherwise
#'    `FUN` gets applied to a class-stripped version of the input.
#'
#' @param x An array of integer64 numbers.
#' @param na.rm,dims Same interpretation as in [colSums()].
#' @examples
#' A = as.integer64(1:6)
#' dim(A) = 3:2
#'
#' colSums(A)
#' rowSums(A)
#' aperm(A, 2:1)
#' @name matrix64
NULL

#' @export
colSums <- function(x, na.rm=FALSE, dims=1L) UseMethod("colSums")
#' @rdname matrix64
#' @export
colSums.default <- function(x, na.rm=FALSE, dims=1L)
  base::colSums(x, na.rm, dims)

#' @rdname matrix64
#' @export
colSums.integer64 <- function(x, na.rm=FALSE, dims=1L) {
  n_dim <- length(dim(x))
  stopifnot(
    `dims= should be a length-1 integer between 1 and length(dim(x))-1L` =
      length(dims) == 1L && dims > 0L && dims < n_dim
  )
  MARGIN = tail(seq_len(n_dim), -dims)
  ret = apply(x, MARGIN, sum, na.rm = na.rm)
  class(ret) = "integer64"
  ret
}

#' @export
rowSums <- function(x, na.rm=FALSE, dims=1L) UseMethod("rowSums")
#' @rdname matrix64
#' @export
rowSums.default <- function(x, na.rm=FALSE, dims=1L)
  base::rowSums(x, na.rm, dims)

#' @rdname matrix64
#' @export
rowSums.integer64 <- function(x, na.rm=FALSE, dims=1L) {
  n_dim <- length(dim(x))
  stopifnot(
    `dims= should be a length-1 integer between 1 and length(dim(x))-1L` =
      length(dims) == 1L && dims > 0L && dims < n_dim
  )
  MARGIN = seq_len(dims)
  ret = apply(x, MARGIN, sum, na.rm = na.rm)
  class(ret) = "integer64"
  ret
}

#' @rdname matrix64
#' @export
aperm.integer64 <- function(a, perm, ...) {
  class(a) = minusclass(class(a), "integer64")
  ret <- aperm(a, perm, ...)
  class(ret) = plusclass(class(a), "integer64")
  ret
}
