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
#' @param ... Passed on to subsequent methods.
#' @param data,nrow,ncol,byrow,dimnames,dim Arguments for `matrix()` and `array()`.
#' @examples
#' A = matrix(as.integer64(1:6), 3)
#'
#' colSums(A)
#' rowSums(A)
#' aperm(A, 2:1)
#' @name matrix64
NULL


#' @rdname matrix64
#' @export matrix
matrix = function(data=NA, nrow=1L, ncol=1L, byrow=FALSE, dimnames=NULL) UseMethod("matrix")
#' @exportS3Method matrix default
matrix.default = function(...) {
  withCallingHandlers({
    base::matrix(...)
  }, warning = function(w) {
    sc = sys.call(sys.nframe() - 8L)
    if (!is.symbol(sc[[1L]]) || sc[[1L]] != as.symbol("matrix"))
      sc = sys.call(sys.nframe() - 7L)
    warning(warningCondition(w$message, call=sc))
    invokeRestart("muffleWarning")
  }, error = function(e) {
    sc = sys.call(sys.nframe() - 5L)
    if (!is.symbol(sc[[1L]]) || sc[[1L]] != as.symbol("matrix"))
      sc = sys.call(sys.nframe() - 4L)
    stop(errorCondition(e$message, call=sc))
  })
}

#' @exportS3Method matrix integer64
matrix.integer64 = function(data=NA_integer64_, ...) {
  if (!length(data)) data = NA_integer64_
  ret = withCallingHandlers({
    base::matrix(data, ...)
  }, warning = function(w) {
    sc = sys.call(sys.nframe() - 8L)
    if (!is.symbol(sc[[1L]]) || sc[[1L]] != as.symbol("matrix"))
      sc = sys.call(sys.nframe() - 7L)
    warning(warningCondition(w$message, call=sc))
    invokeRestart("muffleWarning")
  }, error = function(e) {
    sc = sys.call(sys.nframe() - 5L)
    if (!is.symbol(sc[[1L]]) || sc[[1L]] != as.symbol("matrix"))
      sc = sys.call(sys.nframe() - 4L)
    stop(errorCondition(e$message, call=sc))
  })
  class(ret) = class(data)
  ret
}


#' @rdname matrix64
#' @export array
array = function(data=NA, dim=length(data), dimnames=NULL) UseMethod("array")
#' @exportS3Method array default
array.default = function(...) {
  withCallingHandlers({
    base::array(...)
  }, warning = function(w) {
    sc = sys.call(sys.nframe() - 8L)
    if (!is.symbol(sc[[1L]]) || sc[[1L]] != as.symbol("matrix"))
      sc = sys.call(sys.nframe() - 7L)
    warning(warningCondition(w$message, call=sc))
    invokeRestart("muffleWarning")
  }, error = function(e) {
    sc = sys.call(sys.nframe() - 5L)
    if (!is.symbol(sc[[1L]]) || sc[[1L]] != as.symbol("matrix"))
      sc = sys.call(sys.nframe() - 4L)
    stop(errorCondition(e$message, call=sc))
  })
}

#' @exportS3Method array integer64
array.integer64 = function(data=NA_integer64_, ...) {
  if (!length(data)) data = NA_integer64_
  ret = withCallingHandlers({
    base::array(data, ...)
  }, warning = function(w) {
    sc = sys.call(sys.nframe() - 8L)
    if (!is.symbol(sc[[1L]]) || sc[[1L]] != as.symbol("matrix"))
      sc = sys.call(sys.nframe() - 7L)
    warning(warningCondition(w$message, call=sc))
    invokeRestart("muffleWarning")
  }, error = function(e) {
    sc = sys.call(sys.nframe() - 5L)
    if (!is.symbol(sc[[1L]]) || sc[[1L]] != as.symbol("matrix"))
      sc = sys.call(sys.nframe() - 4L)
    stop(errorCondition(e$message, call=sc))
  })
  class(ret) = class(data)
  ret
}

#' @rdname matrix64
#' @export
colSums = function(x, na.rm=FALSE, dims=1L) UseMethod("colSums")
#' @rdname matrix64
#' @export
colSums.default = function(x, na.rm=FALSE, dims=1L) base::colSums(x, na.rm, dims)

#' @rdname matrix64
#' @export
colSums.integer64 = function(x, na.rm=FALSE, dims=1L) {
  dn = dim(x)
  if (!is.array(x) || length(dn) < 2L) 
    stop("'x' must be an array of at least two dimensions", domain="R-base")
  if (length(dims) != 1L || dims < 1L || dims > length(dn) - 1L) 
    stop("invalid 'dims'", domain="R-base")
  
  ret = apply(x, seq_along(dn)[-seq_len(dims)], sum, na.rm=na.rm)
  class(ret) = class(x)
  ret
}


#' @rdname matrix64
#' @export
rowSums = function(x, na.rm=FALSE, dims=1L) UseMethod("rowSums")
#' @rdname matrix64
#' @export
rowSums.default = function(...) base::rowSums(...)

#' @rdname matrix64
#' @export
rowSums.integer64 = function(x, na.rm=FALSE, dims=1L) {
  dn = dim(x)
  if (!is.array(x) || length(dn) < 2L) 
    stop("'x' must be an array of at least two dimensions", domain="R-base")
  if (length(dims) != 1L || dims < 1L || dims > length(dn) - 1L) 
    stop("invalid 'dims'", domain="R-base")
  
  ret = apply(x, seq_len(dims), sum, na.rm=na.rm)
  class(ret) = class(x)
  ret
}


#' @rdname matrix64
#' @param a,perm Passed on to [aperm()].
#' @exportS3Method base::aperm integer64
aperm.integer64 = function(a, perm, ...) {
  ret = NextMethod()
  class(ret) = class(a)
  ret
}


#' @exportS3Method base::`%*%` integer64
`%*%.integer64` = function(x, y) {
  if (!is.integer64(x) && !is.integer64(y)) 
    return(x%*%y)

  target_class = target_class_for_Ops(x, y)
  if (target_class != "integer64") {
    if (is.integer64(x)) {
      for (cc in class(y)) {
        f = getS3method("%*%", cc, optional=TRUE)
        if (!is.null(f))
          return(f(.as_double_integer64(x, keep.attributes=TRUE), y))
      }
      x = .as_double_integer64(x, keep.attributes=TRUE)
    } else {
      y = .as_double_integer64(y, keep.attributes=TRUE)
    }
    return(x%*%y)
  }

  dx = dim(x)
  dy = dim(y)
  if (length(dx) > 2L || length(dy) > 2L)
    stop("non-conformable arguments", domain="R")
  if (length(dx) <= 1L && length(dy) <= 1L) {
    dx = c(1L, length(x))
    if (length(x) == length(y)) {
      dy = c(length(y), 1L)
    } else {
      dy = c(1L, length(y))
    }
  }
  if (length(dx) <= 1L)
    dx = c(1L, dy[1L])
  if (length(dy) <= 1L)
    dy = c(dx[2L], 1L)
  if (dx[2L] != dy[1L])
    stop("non-conformable arguments", domain="R")
  dim(x) = dx
  dim(y) = dy

  if (is.double(x)) {
    ret = .Call(C_matmult_double_integer64, x, structure(as.integer64(y), dim=dy), double(dx[1L]*dy[2L]))
  } else if (is.double(y)) {
    ret = .Call(C_matmult_integer64_double, structure(as.integer64(x), dim=dx), y, double(dx[1L]*dy[2L]))
  } else {
    ret = .Call(C_matmult_integer64_integer64, structure(as.integer64(x), dim=dx), structure(as.integer64(y), dim=dy), double(dx[1L]*dy[2L]))
  }
  dim(ret) = c(dx[1L], dy[2L])
  oldClass(ret) = "integer64"
  ret
}


#' @exportS3Method base::as.matrix integer64
as.matrix.integer64 = function(x, ...) {
  if (is.matrix(x)) {
    x
  } else {
    array(x, c(length(x), 1L), {if (!is.null(names(x))) list(names(x), NULL) else NULL})
  } 
}
