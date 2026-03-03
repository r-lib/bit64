# /*
# R-Code
# S3 atomic 64bit integers for R
# (c) 2011-2024 Jens Oehlschägel
# (c) 2025-2026 Michael Chirico
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2026-01-16
#*/

#' Binary operators for integer64 vectors
#'
#' Binary operators for integer64 vectors.
#'
#' @param e1 an atomic vector of class 'integer64'
#' @param e2 an atomic vector of class 'integer64'
#'
#' @returns
#'   [`&`], [`|`], [xor()], [`!=`], [`==`],
#'   [`<`], [`<=`], [`>`], [`>=`] return a logical vector
#'
#'   [`^`] and [`/`] return a double vector
#'
#'   [`+`], [`-`], [`*`], \code{\link[=Arithmetic]{\%/\%}}, \code{\link[=Arithmetic]{\%\%}}
#'    return a vector of class 'integer64'
#'
#' @keywords classes manip
#' @seealso [format.integer64()] [integer64()]
#' @examples
#'   as.integer64(1:12) - 1
#'   options(integer64_semantics="new")
#'   d <- 2.5
#'   i <- as.integer64(5)
#'   d/i  # new 0.5
#'   d*i  # new 13
#'   i*d  # new 13
#'   options(integer64_semantics="old")
#'   d/i  # old: 0.4
#'   d*i  # old: 10
#'   i*d  # old: 13
#' @name ops64
NULL

# Version of Leonardo Silvestri
#' @rdname ops64
#' @export
binattr = function(e1, e2) {
  d1 = dim(e1)
  d2 = dim(e2)
  n1 = length(e1)
  n2 = length(e2)

  ## this first part takes care of erroring out when the dimensions
  ## are not compatible or warning if needed:
  if (length(d1)) {
    if (length(d2)) {
      if (!identical(dim(e1), dim(e2)))
        stop(gettext("non-conformable arrays", domain="R"))
    } else {
      if (n2 > n1 && n1)
        stop("length(e2) does not match dim(e1)")
      if (n2 && n1 %% n2)
        warning("length(e1) not a multiple length(e2)")
    }
  } else if (length(d2)) {
    if (n1 > n2 && n2)
      stop("length(e1) does not match dim(n2)")
    if (n1 && n2 %% n1)
      warning("length(e2) not a multiple length(e1)")
  } else {
    # nolint next: unnecessary_nesting_linter. Good parallelism.
    if (n1 < n2 && n1) {
      if (n1 && n2 %% n1)
        warning("length(e2) not a multiple length(e1)")
    } else {
      # nolint next: unnecessary_nesting_linter. Good parallelism.
      if (n2 && n1 %% n2)
        warning("length(e1) not a multiple length(e2)")
    }
  }

  ## in this part we mimic R's algo for selecting attributes:
  if (n1 == n2) {
    ## if same size take attribute from e1 if it exists, else from e2
    if (n1 == 0L) {
      ae1 = attributes(e1)[c("class", "dim", "dimnames")]
      ae2 = attributes(e2)[c("class", "dim", "dimnames")]
    }
    ae1 = attributes(e1)
    ae2 = attributes(e2)
    nae1 = names(attributes(e1))
    nae2 = names(attributes(e2))
    if (n1==0L) {
      ae1 = ae1[nae1 %in% c("class", "dim", "dimnames")]
      ae2 = ae1[nae1 %in% c("class", "dim", "dimnames")]
    }
    allattr = list()
    for (a in union(nae1, nae2))
      if (a %in% nae1)
        allattr[[a]] = ae1[[a]]
    else
      allattr[[a]] = ae2[[a]]
    allattr
  } else if (n1 == 0L || n1 > n2) {
    attributes(e1)
  } else {
    attributes(e2)
  }
}

# helper for determining the target class for Ops methods
target_class_for_Ops = function(e1, e2) {
  if(missing(e2)) {
    if (!is.numeric(unclass(e1)) && !is.logical(e1) && !is.complex(e1))
      stop(errorCondition(gettext("non-numeric argument to mathematical function", domain="R"), call=sys.call(sys.nframe() - 1L)))

    if (is.complex(e1)) {
      "complex"
    } else {
      "integer64"
    }
  } else {
    if (!is.numeric(unclass(e1)) && !is.logical(e1) && !is.complex(e1))
      stop(errorCondition(gettext("non-numeric argument to binary operator", domain="R"), call=sys.call(sys.nframe() - 1L)))
    if (!is.numeric(unclass(e2)) && !is.logical(e2) && !is.complex(e2))
      stop(errorCondition(gettext("non-numeric argument to binary operator", domain="R"), call=sys.call(sys.nframe() - 1L)))

    if (is.complex(e1) || is.complex(e2)) {
      "complex"
    } else {
      "integer64"
    }
  }
}

#' @rdname ops64
#' @export
`+.integer64` = function(e1, e2) {
  if (missing(e2))
    return(e1)
  a = binattr(e1, e2)
  e1 = as.integer64(e1)
  e2 = as.integer64(e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret = double(l)
  ret = .Call(C_plus_integer64, e1, e2, ret)
  a$class = plusclass(a$class, "integer64")
  attributes(ret) = a
  ret
}

#' @rdname ops64
#' @export
`-.integer64` = function(e1, e2) {
  if (missing(e2)) {
    e2 = e1
    e1 = 0L
  }
  a = binattr(e1, e2)
  e1 = as.integer64(e1)
  e2 = as.integer64(e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret = double(l)
  .Call(C_minus_integer64, e1, e2, ret)
  a$class = plusclass(a$class, "integer64")
  attributes(ret) = a
  ret
}

#' @rdname ops64
#' @export
`%/%.integer64` = function(e1, e2) {
  a = binattr(e1, e2)
  e1 = as.integer64(e1)
  e2 = as.integer64(e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret = double(l)
  .Call(C_intdiv_integer64, e1, e2, ret)
  a$class = plusclass(a$class, "integer64")
  attributes(ret) = a
  ret
}

#' @rdname ops64
#' @export
`%%.integer64` = function(e1, e2) {
  a = binattr(e1, e2)
  e1 = as.integer64(e1)
  e2 = as.integer64(e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret = double(l)
  .Call(C_mod_integer64, e1, e2, ret)
  a$class = plusclass(a$class, "integer64")
  attributes(ret) = a
  ret
}

#' @rdname ops64
#' @export
`*.integer64` = function(e1, e2) {
  a = binattr(e1, e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret = double(l)
  if (getOption("integer64_semantics", "old") == "old") {
    if (is.double(e2))  # implies !is.integer64(e2)
      ret = .Call(C_times_integer64_double, as.integer64(e1), e2, ret)
    else
      ret = .Call(C_times_integer64_integer64, as.integer64(e1), as.integer64(e2), ret)
  } else {
    # nolint next: unnecessary_nesting_linter. Good parallelism, and on a to-be-deprecated code path.
    if (is.double(e2))  # implies !is.integer64(e2)
      ret = .Call(C_times_integer64_double, as.integer64(e1), e2, ret)
    else if (is.double(e1))
      ret = .Call(C_times_integer64_double, as.integer64(e2), e1, ret)
    else
      ret = .Call(C_times_integer64_integer64, as.integer64(e1), as.integer64(e2), ret)
  }
  a$class = plusclass(a$class, "integer64")
  attributes(ret) = a
  ret
}

#' @rdname ops64
#' @export
`^.integer64` = function(e1, e2) {
  a = binattr(e1, e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret = double(l)
  if (is.double(e2))  # implies !is.integer64(e2)
    ret = .Call(C_power_integer64_double, as.integer64(e1), e2, ret)
  else
    ret = .Call(C_power_integer64_integer64, as.integer64(e1), as.integer64(e2), ret)
  a$class = plusclass(a$class, "integer64")
  attributes(ret) = a
  ret
}

#' @rdname ops64
#' @export
`/.integer64` = function(e1, e2) {
  a = binattr(e1, e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret = double(l)
  if (getOption("integer64_semantics", "old") == "old") {
    if (is.double(e2))  # implies !is.integer64(e2)
      ret = .Call(C_divide_integer64_double, as.integer64(e1), e2, ret)
    else
      ret = .Call(C_divide_integer64_integer64, as.integer64(e1), as.integer64(e2), ret)
  } else {
    # nolint next: unnecessary_nesting_linter. Good parallelism, and on a to-be-deprecated code path.
    if (is.double(e2))  # implies !is.integer64(e2)
      ret = .Call(C_divide_integer64_double, as.integer64(e1), e2, ret)
    else if (is.double(e1))
      ret = .Call(C_divide_double_integer64, e1, e2, ret)
    else
      ret = .Call(C_divide_integer64_integer64, as.integer64(e1), as.integer64(e2), ret)
  }
  a$class = minusclass(a$class, "integer64")
  attributes(ret) = a
  ret
}

#' @rdname ops64
#' @export
`==.integer64` = function(e1, e2) {
  a = binattr(e1, e2)
  e1 = as.integer64(e1)
  e2 = as.integer64(e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret = logical(l)
  .Call(C_EQ_integer64, e1, e2, ret)
  a$class = minusclass(a$class, "integer64")
  attributes(ret) = a
  ret
}

#' @rdname ops64
#' @export
`!=.integer64` = function(e1, e2) {
  a = binattr(e1, e2)
  e1 = as.integer64(e1)
  e2 = as.integer64(e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret = logical(l)
  .Call(C_NE_integer64, e1, e2, ret)
  a$class = minusclass(a$class, "integer64")
  attributes(ret) = a
  ret
}

#' @rdname ops64
#' @export
`<.integer64` = function(e1, e2) {
  a = binattr(e1, e2)
  e1 = as.integer64(e1)
  e2 = as.integer64(e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret = logical(l)
  .Call(C_LT_integer64, e1, e2, ret)
  a$class = minusclass(a$class, "integer64")
  attributes(ret) = a
  ret
}

#' @rdname ops64
#' @export
`<=.integer64` = function(e1, e2) {
  a = binattr(e1, e2)
  e1 = as.integer64(e1)
  e2 = as.integer64(e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret = logical(l)
  .Call(C_LE_integer64, e1, e2, ret)
  a$class = minusclass(a$class, "integer64")
  attributes(ret) = a
  ret
}

#' @rdname ops64
#' @export
`>.integer64` = function(e1, e2) {
  a = binattr(e1, e2)
  e1 = as.integer64(e1)
  e2 = as.integer64(e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret = logical(l)
  .Call(C_GT_integer64, e1, e2, ret)
  a$class = minusclass(a$class, "integer64")
  attributes(ret) = a
  ret
}

#' @rdname ops64
#' @export
`>=.integer64` = function(e1, e2) {
  a = binattr(e1, e2)
  e1 = as.integer64(e1)
  e2 = as.integer64(e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret = logical(l)
  .Call(C_GE_integer64, e1, e2, ret)
  a$class = minusclass(a$class, "integer64")
  attributes(ret) = a
  ret
}

#' @rdname ops64
#' @export
`&.integer64` = function(e1, e2) {
  a = binattr(e1, e2)
  ret = as.logical(e1) & as.logical(e2)
  a$class = minusclass(a$class, "integer64")
  attributes(ret) = a
  ret
}

#' @rdname ops64
#' @export
`|.integer64` = function(e1, e2) {
  a = binattr(e1, e2)
ret = as.logical(e1) | as.logical(e2)
  a$class = minusclass(a$class, "integer64")
  attributes(ret) = a
  ret
}

#' @rdname format.integer64
#' @export
`!.integer64` = function(x) {
  a = attributes(x)
  ret = !as.logical(x)
  a$class = minusclass(a$class, "integer64")
  attributes(ret) = a
  ret
}

