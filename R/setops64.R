# /*
# R-Code
# S3 atomic 64bit integers for R
# (c) 2026 Michael Chirico
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2026-01-14
#*/

#' @title Set Operations
#' @description Performs set union, intersection, (asymmetric!) difference, equality and membership on two vectors. As soon as an integer64 vector is involved, the operations are performed using integer64 semantics. Otherwise the \code{base} package functions are called.
#' @param x,y,el,set vectors (of the same mode) containing a sequence of items (conceptually) with no duplicated values.
#' @return 
#' For union, a vector of a common mode or class.
#' 
#' For intersect, a vector of a common mode or class, or NULL if x or y is NULL.
#' 
#' For setdiff, a vector of the same mode or class as x.
#' 
#' A logical scalar for setequal and a logical of the same length as x for is.element.
#' @seealso [base::union]
#' @examples
#' x <- as.integer64(1:5)
#' y <- c(1L, 3L, 5L, 7L)
#' union(x, y)
#' intersect(x, y)
#' setdiff(x, y)
#' setequal(x, y)
#' is.element(x, y)
#' 
#' @export
#' @rdname sets
union = function(x, y) {
  if (!(is.integer64(x) || is.integer64(y)))
    return(base::union(x, y))
  
  target_class = target_class_and_sample_value(list(x, y))
  # try using the benefit of integer64 caching, if possible. I.e. call unique() before as().
  x = unique(x)
  class_x = class(x)[1L]
  if (class_x != target_class) {
    # TODO(#44): remove this special coercion for factor and ordered
    if (target_class == "integer64" && class_x %in% c("factor", "ordered") && getOption("bit64.promoteInteger64ToCharacter", FALSE) == FALSE)
      x = as.character(x)
    x = as(x, target_class)
  }
  y = unique(y)
  class_y = class(y)[1L]
  if (class_y != target_class) {
    # TODO(#44): remove this special coercion for factor and ordered
    if (target_class == "integer64" && class_y %in% c("factor", "ordered") && getOption("bit64.promoteInteger64ToCharacter", FALSE) == FALSE)
      y = as.character(y)
    y = as(y, target_class)
  }

  unique(c(x, y))
}

#' @export
#' @rdname sets
intersect = function(x, y) {
  if (!(is.integer64(x) || is.integer64(y)))
    return(base::intersect(x, y))
  
  target_class = target_class_and_sample_value(list(x, y))
  x = unique(x)
  class_x = class(x)[1L]
  if (class_x != target_class) {
    # TODO(#44): remove this special coercion for factor and ordered
    if (target_class == "integer64" && class_x %in% c("factor", "ordered") && getOption("bit64.promoteInteger64ToCharacter", FALSE) == FALSE)
      x = as.character(x)
    x = as(x, target_class)
  }
  y = unique(y)
  class_y = class(y)[1L]
  if (class_y != target_class) {
    # TODO(#44): remove this special coercion for factor and ordered
    if (target_class == "integer64" && class_y %in% c("factor", "ordered") && getOption("bit64.promoteInteger64ToCharacter", FALSE) == FALSE)
      y = as.character(y)
    y = as(y, target_class)
  }
  
  x[match(x, y, 0L) > 0L]
}

#' @export
#' @rdname sets
setequal = function(x, y) {
  if (!(is.integer64(x) || is.integer64(y)))
    return(base::setequal(x, y))
  
  target_class = target_class_and_sample_value(list(x, y))
  x = unique(x)
  if (class(x)[1L] != target_class)
    x = as(x, target_class)
  y = unique(y)
  if (class(y)[1L] != target_class)
    y = as(y, target_class)
  
  length(x) == length(y) && !anyNA(match(x, y))
}

#' @export
#' @rdname sets
setdiff = function(x, y) {
  if (!(is.integer64(x) || is.integer64(y)))
    return(base::setdiff(x, y))
  
  class_x = class(x)[1L]
  if (class_x %in% c("POSIXct", "Date"))
    x = unclass(x)
  if (class_x %in% c("factor", "ordered"))
    x = as.character(x)
  target_class = target_class_and_sample_value(list(x, y))
  x = unique(x)
  if (class(x)[1L] != target_class)
    x_match = as(x, target_class)
    else
      x_match = x
  y = unique(y)
  class_y = class(y)[1L]
  if (class_y != target_class) {
    # TODO(#44): remove this special coercion for factor and ordered
    if (target_class == "integer64" && class_y %in% c("factor", "ordered") && getOption("bit64.promoteInteger64ToCharacter", FALSE) == FALSE)
      y = as.character(y)
    y = as(y, target_class)
  }

  x[match(x_match, y, 0L) == 0L]
}

#' @export
#' @rdname sets
is.element = function(el, set) {
  if (!(is.integer64(el) || is.integer64(set)))
    return(base::is.element(el, set))
  
  target_class = target_class_and_sample_value(list(el, set))
  class_el = class(el)[1L]
  if (class_el != target_class) {
    # TODO(#44): remove this special coercion for factor and ordered
    if (target_class == "integer64" && class_el %in% c("factor", "ordered") && getOption("bit64.promoteInteger64ToCharacter", FALSE) == FALSE)
      el = as.character(el)
    el = as(el, target_class)
  }
  set = unique(set)
  class_set = class(set)[1L]
  if (class_set != target_class) {
    # TODO(#44): remove this special coercion for factor and ordered
    if (target_class == "integer64" && class_set %in% c("factor", "ordered") && getOption("bit64.promoteInteger64ToCharacter", FALSE) == FALSE)
      set = as.character(set)
    set = as(set, target_class)
  }
  
  match(el, set, 0L) > 0L
}
