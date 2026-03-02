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
#' @param e1,e2,x numeric or complex vectors or objects which can be coerced to such, or other objects for which methods have been written for - especially 'integer64' vectors.
#'
#' @returns
#'   [`&`], [`|`], [`!`], [`!=`], [`==`], [`<`], [`<=`], [`>`], [`>=`] return a logical vector
#'
#'   [`/`] returns a double vector
#'
#'   [`+`], [`-`], [`*`], [`%/%`][Arithmetic], [`%%`][Arithmetic], [`^`] return a vector of class 'integer64' or different class depending on the operands
#'
#' @keywords classes manip
#' @seealso [integer64()]
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
    if (n1 == 0L) {
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
  convert_to_integer64 = function(el) is.integer64(el) || ((is.integer(el) || is.double(el)) && !inherits(el, c("difftime", "Date", "POSIXt"))) || is.logical(el)
  if(missing(e2)) {
    if (!is.numeric(unclass(e1)) && !is.logical(e1) && !is.complex(e1) && !inherits(e1, "POSIXt"))
      stop(errorCondition(gettext("non-numeric argument to mathematical function", domain="R"), call=sys.call(sys.nframe() - 1L)))

    if (convert_to_integer64(e1)) {
      "integer64"
    } else {
      class(e1)[1L]
    }
  } else {
    if (!is.numeric(unclass(e1)) && !is.logical(e1) && !is.complex(e1) && !inherits(e1, "POSIXt"))
      stop(errorCondition(gettext("non-numeric argument to binary operator", domain="R"), call=sys.call(sys.nframe() - 1L)))
    if (!is.numeric(unclass(e2)) && !is.logical(e2) && !is.complex(e2) && !inherits(e2, "POSIXt"))
      stop(errorCondition(gettext("non-numeric argument to binary operator", domain="R"), call=sys.call(sys.nframe() - 1L)))

    conv_to_int1 = convert_to_integer64(e1)
    if (conv_to_int1 && convert_to_integer64(e2)) {
      "integer64"
    } else if (conv_to_int1) {
      class(e2)[1L]
    } else {
      class(e1)[1L]
    }
  }
}

#' @rawNamespace if (getRversion() >= "4.3.0") S3method(chooseOpsMethod,integer64)
chooseOpsMethod.integer64 = function(x, y, mx, my, cl, reverse) {
  TRUE
}

#' @rdname ops64
#' @exportS3Method `+` integer64
`+.integer64` = function(e1, e2) {
  if (missing(e2))
    return(e1)
  
  target_class = target_class_for_Ops(e1, e2)
  if (target_class != "integer64") {
    if (is.integer64(e1))
      e1 = .as_double_integer64(e1, keep.attributes=TRUE)
    else
      e2 = .as_double_integer64(e2, keep.attributes=TRUE)
    return(e1 + e2)
  }

  a = binattr(e1, e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret = .Call(C_plus_integer64, as.integer64(e1), as.integer64(e2), double(l))
  a$class = plusclass(a$class, "integer64")
  attributes(ret) = a
  ret
}

#' @rdname ops64
#' @exportS3Method `-` integer64
`-.integer64` = function(e1, e2) {
  if (missing(e2)) {
    if (!is.integer64(e1)) 
      return(-e1)
    e2 = e1
    e1 = as.integer64(0L)
  } else {
    target_class = target_class_for_Ops(e1, e2)
    if (target_class != "integer64") {
      if (is.integer64(e1))
        e1 = .as_double_integer64(e1, keep.attributes=TRUE)
      else
        e2 = .as_double_integer64(e2, keep.attributes=TRUE)
      return(e1 - e2)
    }
  }

  a = binattr(e1, e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret = .Call(C_minus_integer64, as.integer64(e1), as.integer64(e2), double(l))
  a$class = plusclass(a$class, "integer64")
  attributes(ret) = a
  ret
}

#' @rdname ops64
#' @exportS3Method `%/%` integer64
`%/%.integer64` = function(e1, e2) {
  target_class = target_class_for_Ops(e1, e2)
  if (target_class != "integer64") {
    if (is.integer64(e1))
      e1 = .as_double_integer64(e1, keep.attributes=TRUE)
    else
      e2 = .as_double_integer64(e2, keep.attributes=TRUE)
    return(e1 %/% e2)
  }
  
  a = binattr(e1, e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret = .Call(C_intdiv_integer64, as.integer64(e1), as.integer64(e2), double(l))
  a$class = plusclass(a$class, "integer64")
  attributes(ret) = a
  ret
}

#' @rdname ops64
#' @exportS3Method `%%` integer64
`%%.integer64` = function(e1, e2) {
  target_class = target_class_for_Ops(e1, e2)
  if (target_class != "integer64") {
    if (is.integer64(e1))
      e1 = .as_double_integer64(e1, keep.attributes=TRUE)
    else
      e2 = .as_double_integer64(e2, keep.attributes=TRUE)
    return(e1 %% e2)
  }
  
  a = binattr(e1, e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret = .Call(C_mod_integer64, as.integer64(e1), as.integer64(e2), double(l))
  a$class = plusclass(a$class, "integer64")
  attributes(ret) = a
  ret
}

#' @rdname ops64
#' @exportS3Method `*` integer64
`*.integer64` = function(e1, e2) {
  target_class = target_class_for_Ops(e1, e2)
  if (target_class != "integer64") {
    if (is.integer64(e1))
      e1 = .as_double_integer64(e1, keep.attributes=TRUE)
    else
      e2 = .as_double_integer64(e2, keep.attributes=TRUE)
    return(e1 * e2)
  }

  a = binattr(e1, e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  if (getOption("integer64_semantics", "old") == "old") {
    if (is.double(e2))  # implies !is.integer64(e2)
      ret = .Call(C_times_integer64_double, as.integer64(e1), e2, double(l))
    else
      ret = .Call(C_times_integer64_integer64, as.integer64(e1), as.integer64(e2), double(l))
  } else {
    # nolint next: unnecessary_nesting_linter. Good parallelism, and on a to-be-deprecated code path.
    if (is.double(e2))  # implies !is.integer64(e2)
      ret = .Call(C_times_integer64_double, as.integer64(e1), e2, double(l))
    else if (is.double(e1))
      ret = .Call(C_times_integer64_double, as.integer64(e2), e1, double(l))
    else
      ret = .Call(C_times_integer64_integer64, as.integer64(e1), as.integer64(e2), double(l))
  }
  a$class = plusclass(a$class, "integer64")
  attributes(ret) = a
  ret
}

#' @rdname ops64
#' @exportS3Method `^` integer64
`^.integer64` = function(e1, e2) {
  target_class = target_class_for_Ops(e1, e2)
  if (target_class != "integer64") {
    if (is.integer64(e1))
      e1 = .as_double_integer64(e1, keep.attributes=TRUE)
    else
      e2 = .as_double_integer64(e2, keep.attributes=TRUE)
    return(e1 ^ e2)
  }
  
  a = binattr(e1, e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  if (is.double(e2)) {
    ret = .Call(C_power_integer64_double, e1, e2, double(l))
  } else {
    ret = .Call(C_power_integer64_integer64, as.integer64(e1), as.integer64(e2), double(l))
  }
  a$class = plusclass(a$class, "integer64")
  attributes(ret) = a
  ret
}

#' @rdname ops64
#' @exportS3Method `/` integer64
`/.integer64` = function(e1, e2) {
  target_class = target_class_for_Ops(e1, e2)
  if (target_class != "integer64") {
    if (is.integer64(e1))
      e1 = .as_double_integer64(e1, keep.attributes=TRUE)
    else
      e2 = .as_double_integer64(e2, keep.attributes=TRUE)
    return(e1 / e2)
  }
  
  a = binattr(e1, e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  if (getOption("integer64_semantics", "old") == "old") {
    if (is.double(e2))  # implies !is.integer64(e2)
      ret = .Call(C_divide_integer64_double, as.integer64(e1), e2, double(l))
    else
      ret = .Call(C_divide_integer64_integer64, as.integer64(e1), as.integer64(e2), double(l))
  } else {
    # nolint next: unnecessary_nesting_linter. Good parallelism, and on a to-be-deprecated code path.
    if (is.double(e2))  # implies !is.integer64(e2)
      ret = .Call(C_divide_integer64_double, as.integer64(e1), e2, double(l))
    else if (is.double(e1))
      ret = .Call(C_divide_double_integer64, e1, e2, double(l))
    else
      ret = .Call(C_divide_integer64_integer64, as.integer64(e1), as.integer64(e2), double(l))
  }
  a$class = minusclass(a$class, "integer64")
  attributes(ret) = a
  ret
}

#' @rdname ops64
#' @exportS3Method `==` integer64
`==.integer64` = function(e1, e2) {
  target_class = target_class_for_Ops(e1, e2)
  if (target_class != "integer64") {
    if (is.integer64(e1))
      e1 = .as_double_integer64(e1, keep.attributes=TRUE)
    else
      e2 = .as_double_integer64(e2, keep.attributes=TRUE)
    return(e1 == e2)
  }

  a = binattr(e1, e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret = .Call(C_EQ_integer64, as.integer64(e1), as.integer64(e2), logical(l))
  names(ret) = a$names
  ret
}

#' @rdname ops64
#' @exportS3Method `!=` integer64
`!=.integer64` = function(e1, e2) {
  target_class = target_class_for_Ops(e1, e2)
  if (target_class != "integer64") {
    if (is.integer64(e1))
      e1 = .as_double_integer64(e1, keep.attributes=TRUE)
    else
      e2 = .as_double_integer64(e2, keep.attributes=TRUE)
    return(e1 != e2)
  }
  
  a = binattr(e1, e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret = .Call(C_NE_integer64, as.integer64(e1), as.integer64(e2), logical(l))
  names(ret) = a$names
  ret
}

#' @rdname ops64
#' @exportS3Method `<` integer64
`<.integer64` = function(e1, e2) {
  target_class = target_class_for_Ops(e1, e2)
  if (target_class != "integer64") {
    if (is.integer64(e1))
      e1 = .as_double_integer64(e1, keep.attributes=TRUE)
    else
      e2 = .as_double_integer64(e2, keep.attributes=TRUE)
    return(e1 < e2)
  }

  a = binattr(e1, e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret = .Call(C_LT_integer64, as.integer64(e1), as.integer64(e2), logical(l))
  names(ret) = a$names
  ret
}

#' @rdname ops64
#' @exportS3Method `<=` integer64
`<=.integer64` = function(e1, e2) {
  target_class = target_class_for_Ops(e1, e2)
  if (target_class != "integer64") {
    if (is.integer64(e1))
      e1 = .as_double_integer64(e1, keep.attributes=TRUE)
    else
      e2 = .as_double_integer64(e2, keep.attributes=TRUE)
    return(e1 <= e2)
  }

  a = binattr(e1, e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret = .Call(C_LE_integer64, as.integer64(e1), as.integer64(e2), logical(l))
  names(ret) = a$names
  ret
}

#' @rdname ops64
#' @exportS3Method `>` integer64
`>.integer64` = function(e1, e2) {
  target_class = target_class_for_Ops(e1, e2)
  if (target_class != "integer64") {
    if (is.integer64(e1))
      e1 = .as_double_integer64(e1, keep.attributes=TRUE)
    else
      e2 = .as_double_integer64(e2, keep.attributes=TRUE)
    return(e1 > e2)
  }

  a = binattr(e1, e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret = .Call(C_GT_integer64, as.integer64(e1), as.integer64(e2), logical(l))
  names(ret) = a$names
  ret
}

#' @rdname ops64
#' @exportS3Method `>=` integer64
`>=.integer64` = function(e1, e2) {
  target_class = target_class_for_Ops(e1, e2)
  if (target_class != "integer64") {
    if (is.integer64(e1))
      e1 = .as_double_integer64(e1, keep.attributes=TRUE)
    else
      e2 = .as_double_integer64(e2, keep.attributes=TRUE)
    return(e1 >= e2)
  }
  
  a = binattr(e1, e2)
  l1 = length(e1)
  l2 = length(e2)
  l = if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret = .Call(C_GE_integer64, as.integer64(e1), as.integer64(e2), logical(l))
  names(ret) = a$names
  ret
}

#' @rdname ops64
#' @exportS3Method `&` integer64
`&.integer64` = function(e1, e2) {
  target_class = target_class_for_Ops(e1, e2)
  if (target_class != "integer64") {
    if (is.integer64(e1))
      e1 = .as_double_integer64(e1, keep.attributes=TRUE)
    else
      e2 = .as_double_integer64(e2, keep.attributes=TRUE)
    return(e1 & e2)
  }
  
  a = binattr(e1, e2)
  ret = as.logical(e1) & as.logical(e2)
  names(ret) = a$names
  ret
}

#' @rdname ops64
#' @exportS3Method `|` integer64
`|.integer64` = function(e1, e2) {
  target_class = target_class_for_Ops(e1, e2)
  if (target_class != "integer64") {
    if (is.integer64(e1))
      e1 = .as_double_integer64(e1, keep.attributes=TRUE)
    else
      e2 = .as_double_integer64(e2, keep.attributes=TRUE)
    return(e1 | e2)
  }
  
  a = binattr(e1, e2)
  ret = as.logical(e1) | as.logical(e2)
  names(ret) = a$names
  ret
}

#' @rdname ops64
#' @exportS3Method `!` integer64
`!.integer64` = function(x) {
  ret = !as.logical(x)
  names(ret) = names(x)
  ret
}
