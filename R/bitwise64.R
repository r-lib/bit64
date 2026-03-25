# /*
# R-Code
# S3 atomic 64bit integers for R
# (c) 2026 Michael Chirico
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2026-03-24
#*/

#' @title Bitwise Logical Operations
#' @description Logical operations on integer vectors with elements viewed as sets of bits. As soon as an integer64 vector is involved, the operations are performed using integer64 semantics. Otherwise the \code{base} package functions are called.
#' @inheritParams base::bitwAnd
#' @inheritParams base::bitShiftL
#' @return 
#' An integer64 vector of length the longer of the arguments, or zero length if one is zero-length.
#' 
#' @seealso [bitwAnd][base::bitwAnd], [bitwOr][base::bitwOr], [bitwXor][base::bitwXor], [bitwNot][base::bitwNot], [bitwShiftL][base::bitwShiftL], [bitwShiftR][base::bitwShiftR]
#' @examples
#' x <- as.integer64(1:5)
#' y <- c(1L, 3L, 5L, 7L)
#' bitwAnd(x, y)
#' bitwOr(x, y)
#' bitwXor(x, y)
#' bitwNot(x)
#' bitwShiftL(x, 1L)
#' bitwShiftR(x, 1L)
#' 
#' @export
#' @name bitwise
#' @rdname bitwise
bitwNot = function(a) {
  if (!is.integer64(a))
    return(base::bitwNot(a))
  
  ret = .Call(C_bitwNot_integer64, a, double(length(a)))
  oldClass(ret) = "integer64"
  ret
}

#' @export
#' @rdname bitwise
bitwAnd = function(a, b) {
  if (!(is.integer64(a) || is.integer64(b)))
    return(base::bitwAnd(a, b))
  
  if (is.factor(a) || inherits(a, "POSIXct") || inherits(a, "Date"))
    a = as.integer(a)
  if (is.factor(b) || inherits(b, "POSIXct") || inherits(b, "Date"))
    b = as.integer(b)
  
  if (!is.numeric(a) || !is.numeric(b))
    stop("'a' and 'b' must have the same type", domain="R")
  
  l1 = length(a)
  l2 = length(b)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  if (is.integer(a)) {
    ret = .Call(C_bitwAnd_integer64_integer, b, a, double(l))
  } else if (is.integer(b)) {
    ret = .Call(C_bitwAnd_integer64_integer, a, b, double(l))
  } else if (is.integer64(a) && is.integer64(b)) {
    ret = .Call(C_bitwAnd_integer64_integer64, a, b, double(l))
  } else {
    ret = .Call(C_bitwAnd_integer64_integer64, as.integer64(a), as.integer64(b), double(l))
  }
  oldClass(ret) = "integer64"
  ret
}

#' @export
#' @rdname bitwise
bitwOr = function(a, b) {
  if (!(is.integer64(a) || is.integer64(b)))
    return(base::bitwOr(a, b))
  
  if (is.factor(a) || inherits(a, "POSIXct") || inherits(a, "Date"))
    a = as.integer(a)
  if (is.factor(b) || inherits(b, "POSIXct") || inherits(b, "Date"))
    b = as.integer(b)
  
  if (!is.numeric(a) || !is.numeric(b))
    stop("'a' and 'b' must have the same type", domain="R")
  
  l1 = length(a)
  l2 = length(b)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  if (is.integer(a)) {
    ret = .Call(C_bitwOr_integer64_integer, b, a, double(l))
  } else if (is.integer(b)) {
    ret = .Call(C_bitwOr_integer64_integer, a, b, double(l))
  } else if (is.integer64(a) && is.integer64(b)) {
    ret = .Call(C_bitwOr_integer64_integer64, a, b, double(l))
  } else {
    ret = .Call(C_bitwOr_integer64_integer64, as.integer64(a), as.integer64(b), double(l))
  }
  oldClass(ret) = "integer64"
  ret
}

#' @export
#' @rdname bitwise
bitwXor = function(a, b) {
  if (!(is.integer64(a) || is.integer64(b)))
    return(base::bitwXor(a, b))
  
  if (is.factor(a) || inherits(a, "POSIXct") || inherits(a, "Date"))
    a = as.integer(a)
  if (is.factor(b) || inherits(b, "POSIXct") || inherits(b, "Date"))
    b = as.integer(b)
  
  if (!is.numeric(a) || !is.numeric(b))
    stop("'a' and 'b' must have the same type", domain="R")
  
  l1 = length(a)
  l2 = length(b)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  if (is.integer(a)) {
    ret = .Call(C_bitwXor_integer64_integer, b, a, double(l))
  } else if (is.integer(b)) {
    ret = .Call(C_bitwXor_integer64_integer, a, b, double(l))
  } else if (is.integer64(a) && is.integer64(b)) {
    ret = .Call(C_bitwXor_integer64_integer64, a, b, double(l))
  } else {
    ret = .Call(C_bitwXor_integer64_integer64, as.integer64(a), as.integer64(b), double(l))
  }
  oldClass(ret) = "integer64"
  ret
}

#' @export
#' @rdname bitwise
bitwShiftL = function(a, n) {
  if (!(is.integer64(a) || is.integer64(n)))
    return(base::bitwShiftL(a, n))
  if (!is.integer64(a))
    return(base::bitwShiftL(a, as.integer(n)))
  
  # if (!is.numeric(n) || is.factor(n) || inherits(n, "POSIXct") || inherits(n, "Date"))
  #   n = as.integer(n)
  # if (!is.numeric(a))
  #   stop("'a' and 'b' must have the same type", domain="R")
  
  l1 = length(a)
  l2 = length(n)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  if (is.integer64(n)) {
    ret = .Call(C_bitwShiftL_integer64_integer64, a, n, double(l))
  } else {
    ret = .Call(C_bitwShiftL_integer64_integer, a, as.integer(n), double(l))
  }
  oldClass(ret) = "integer64"
  ret
}

#' @export
#' @rdname bitwise
bitwShiftR = function(a, n) {
  if (!(is.integer64(a) || is.integer64(n)))
    return(base::bitwShiftR(a, n))
  if (!is.integer64(a))
    return(base::bitwShiftR(a, as.integer(n)))
  
  # if (!is.numeric(n) || is.factor(n) || inherits(n, "POSIXct") || inherits(n, "Date"))
  #   n = as.integer(n)
  # if (!is.numeric(a))
  #   stop("'a' and 'b' must have the same type", domain="R")
  
  l1 = length(a)
  l2 = length(n)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  if (is.integer64(n)) {
    ret = .Call(C_bitwShiftR_integer64_integer64, a, n, double(l))
  } else {
    ret = .Call(C_bitwShiftR_integer64_integer, a, as.integer(n), double(l))
  }
  oldClass(ret) = "integer64"
  ret
}
