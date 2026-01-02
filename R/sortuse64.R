# /*
# R-Code for searching and merging
# S3 atomic 64bit integers for R
# (c) 2011 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2011-12-11
# Last changed:  2011-12-11
# */

#' Searching and other uses of sorting for 64bit integers
#'
#' This is roughly an implementation of hash functionality but based on sorting
#'   instead on a hashmap. Since sorting is more informative than hashing we
#'   can do some more interesting things.
#'
#' @param sorted a sorted [`integer64`] vector
#' @param ... further arguments, passed from generics, ignored in methods
#' @param method see Details
#'
#' @details
#'
# nolint start: line_length_linter.
#' | **sortfun** | **orderfun** | **sortorderfun** | **see also**       | **description** |
#' |------------:|-------------:|-----------------:|-------------------:|:----------------|
#' |   `sortnut` |   `ordernut` |                  |                    | return number of tied and of unique values |
#' |   `sortfin` |   `orderfin` |                  | [`%in%.integer64`] | return logical whether `x` is in `table` |
#' |             |   `orderpos` |   `sortorderpos` | [`match()`][match.integer64] | return positions of `x` in `table` |
#' |             |   `orderdup` |   `sortorderdup` | [`duplicated()`][duplicated.integer64] | return logical whether values are duplicated |
#' |   `sortuni` |   `orderuni` |   `sortorderuni` | [`unique()`][unique.integer64] | return unique values (=dimensiontable) |
#' |             |   `orderupo` |   `sortorderupo` | [`unique()`][unique.integer64] | return positions of unique values |
#' |             |   `ordertie` |   `sortordertie` |                    | return positions of tied values |
#' |             |   `orderkey` |   `sortorderkey` |                    | positions of values in vector of unique values (match in dimensiontable) |
#' |   `sorttab` |   `ordertab` |   `sortordertab` | [`table()`][table] | tabulate frequency of values  |
#' |             |   `orderrnk` |   `sortorderrnk` |                    | rank averaging ties |
#' |   `sortqtl` |   `orderqtl` |                  |                    | return quantiles given probabilities |
# nolint end: line_length_linter.
#'
#' The functions `sortfin`, `orderfin`, `orderpos` and `sortorderpos` each
#'   offer three algorithms for finding `x` in `table`.
#'
#' With `method=1L` each value of `x` is searched independently using
#'   _binary search_, this is fastest for small `table`s.
#'
#' With `method=2L` the values of `x` are first sorted and then searched using
#'   _doubly exponential search_, this is the best all-around method.
#'
#' With `method=3L` the values of `x` are first sorted and then searched using
#'   simple merging, this is the fastest method if `table` is huge and `x` has
#'   similar size and distribution of values.
#'
#' With `method=NULL` the functions use a heuristic to determine the fastest
#'   algorithm.
#'
#' The functions `orderdup` and `sortorderdup` each offer two algorithms for
#'   setting the truth values in the return vector.
#'
#' With `method=1L` the return values are set directly which causes random
#'   write access on a possibly large return vector.
#'
#' With `method=2L` the return values are first set in a smaller bit-vector --
#'   random access limited to a smaller memory region -- and finally written
#'   sequentially to the logical output  vector.
#'
#' With `method=NULL` the functions use a heuristic to determine the fastest
#'   algorithm.
#'
#' @return see details
#' @keywords programming manip
#' @seealso [`match()`][match.integer64]
#' @examples
#'  message("check the code of 'optimizer64' for examples:")
#'  print(optimizer64)
#' @export
sortnut <- function(sorted, ...) UseMethod("sortnut")

#' @rdname sortnut
#' @export
sortnut.integer64 <- function(sorted, ...) {
  ret <- .Call(C_r_ram_integer64_sortnut, x = sorted)
  names(ret) <- c("nunique", "nties")
  ret
}

#' @rdname sortnut
#' @param table the original data with original order under the sorted vector
#' @param order an [`integer`] order vector that turns 'table' into 'sorted'
#' @export
ordernut <- function(table, order, ...) UseMethod("ordernut")

#' @rdname sortnut
#' @export
ordernut.integer64 <- function(table, order, ...) {
  ret <- .Call(C_r_ram_integer64_ordernut, table = as.integer64(table), order = as.integer(order))
  names(ret) <- c("nunique", "nties")
  ret
}

#' @rdname sortnut
#' @param x an [`integer64`] vector
#' @export
sortfin <- function(sorted, x, ...) UseMethod("sortfin")

#' @rdname sortnut
#' @export
sortfin.integer64 <- function(sorted, x, method=NULL, ...) {
  n <- length(x)
  if (is.null(method)) {
    if (n<2048L) {
      method <- 1L
    } else if (n<length(sorted)/128L) {
      method <- 2L
    } else {
      method <- 3L
    }
  } else {
    method <- as.integer(method)
  }
  if (method==1L) {
      .Call(C_r_ram_integer64_sortfin_asc
      , x = as.integer64(x)
      , sorted = as.integer64(sorted)
      , method= method
      , ret = logical(n)
      )
  } else {
    sx <- clone(as.integer64(x)); o <- seq_along(x); ramsortorder(sx, o, na.last=FALSE, ...)
    ret <- logical(n)
    ret[o] <- .Call(C_r_ram_integer64_sortfin_asc
      , x = sx
      , sorted = as.integer64(sorted)
      , method= method
      , ret = ret
      )
    ret
  }
}

#' @rdname sortnut
#' @export
orderfin <- function(table, order, x, ...) UseMethod("orderfin")

#' @rdname sortnut
#' @export
orderfin.integer64 <- function(table, order, x, method=NULL, ...) {
  n <- length(x)
  if (is.null(method)) {
    if (n<4096L) {
      method <- 1L
    } else if (n<length(table)/8L) {
      method <- 2L
    } else {
      method <- 3L
    }
  } else {
    method <- as.integer(method)
  }
  if (method==1L) {
      .Call(C_r_ram_integer64_orderfin_asc
      , x = as.integer64(x)
      , table = as.integer64(table)
      , order = as.integer(order)
      , method= as.integer(method)
      , ret = logical(n)
      )
  } else {
    o <- seq_along(x); ramorder(x, o, na.last=FALSE, ...)
    ret <- logical(n)
    ret[o] <- .Call(C_r_ram_integer64_orderfin_asc
      , x = x[o]
      , table = as.integer64(table)
      , order = as.integer(order)
      , method= as.integer(method)
      , ret = ret
      )
      ret
  }
}

#' @rdname sortnut
#' @export
orderpos <- function(table, order, x, ...) UseMethod("orderpos")

#' @rdname sortnut
#' @param nomatch the value to be returned if an element is not found in the hashmap
#' @export
orderpos.integer64 <- function(table, order, x, nomatch=NA, method=NULL, ...) {
  n <- length(x)
  if (is.null(method)) {
    if (n<4096L) {
      method <- 1L
    } else if (n<length(table)/8L) {
      method <- 2L
    } else {
      method <- 3L
    }
  } else {
    method <- as.integer(method)
  }
  if (method==1L) {
      .Call(C_r_ram_integer64_orderpos_asc
      , x = as.integer64(x)
      , table = as.integer64(table)
      , order = as.integer(order)
      , nomatch = as.integer(nomatch)
      , method= as.integer(method)
      , ret = integer(n)
      )
  } else {
    o <- seq_along(x); ramorder(x, o, na.last=FALSE, ...)
    ret <- integer(n)
    ret[o] <- .Call(C_r_ram_integer64_orderpos_asc
      , x = x[o]
      , table = as.integer64(table)
      , order = as.integer(order)
      , nomatch = as.integer(nomatch)
      , method= as.integer(method)
      , ret = ret
      )
      ret
  }
}

#' @rdname sortnut
#' @export
sortorderpos <- function(sorted, order, x, ...) UseMethod("sortorderpos")

#' @rdname sortnut
#' @export
sortorderpos.integer64 <- function(sorted, order, x, nomatch=NA, method=NULL, ...) {
  n <- length(x)
  if (is.null(method)) {
    if (n<2048L) {
      method <- 1L
    } else if (n<length(sorted)/128L) {
      method <- 2L
    } else {
      method <- 3L
    }
  } else {
    method <- as.integer(method)
  }
  if (method==1L) {
      .Call(C_r_ram_integer64_sortorderpos_asc
      , x = as.integer64(x)
      , sorted = as.integer64(sorted)
      , order = as.integer(order)
      , nomatch = as.integer(nomatch)
      , method= as.integer(method)
      , ret = integer(n)
      )
  } else {
    sx <- clone(as.integer64(x)); o <- seq_along(x); ramsortorder(sx, o, na.last=FALSE, ...)
    ret <- integer(n)
    ret[o] <- .Call(C_r_ram_integer64_sortorderpos_asc
      , x = sx
      , sorted = as.integer64(sorted)
      , order = as.integer(order)
      , nomatch = as.integer(nomatch)
      , method= as.integer(method)
      , ret = ret
      )
      ret
  }
}

#' @rdname sortnut
#' @export
orderdup <- function(table, order, ...) UseMethod("orderdup")

#' @rdname sortnut
#' @export
orderdup.integer64 <- function(table, order, method=NULL, ...) {
  if (is.null(method)) {
    if (length(table)<4194304L)
        method <- 1L
      else
        method <- 2L
  } else {
    method <- as.integer(method)
  }
  .Call(C_r_ram_integer64_orderdup_asc
  , table = as.integer64(table)
  , order = as.integer(order)
  , method = method
  , ret = logical(length(table))
  )
}

#' @rdname sortnut
#' @export
sortorderdup <- function(sorted, order, ...) UseMethod("sortorderdup")

#' @rdname sortnut
#' @export
sortorderdup.integer64 <- function(sorted, order, method=NULL, ...) {
  if (is.null(method)) {
    if (length(sorted)<4194304L)
        method <- 1L
      else
        method <- 2L
  } else {
    method <- as.integer(method)
  }
  .Call(C_r_ram_integer64_sortorderdup_asc
  , sorted = as.integer64(sorted)
  , order = as.integer(order)
  , method = method
  , ret = logical(length(sorted))
  )
}

#' @rdname sortnut
#' @param nunique number of unique elements, usually we get this from cache
#'   or call `sortnut` or `ordernut`
#' @export
sortuni <- function(sorted, nunique, ...) UseMethod("sortuni")

#' @rdname sortnut
#' @export
sortuni.integer64 <- function(sorted, nunique, ...) {
  .Call(C_r_ram_integer64_sortuni_asc
  , sorted = as.integer64(sorted)
  , ret = integer64(nunique)
  )
}

#' @rdname sortnut
#' @export
orderuni <- function(table, order, nunique, ...) UseMethod("orderuni")

#' @rdname sortnut
#' @param keep.order determines order of results and speed: `FALSE` (the default)
#'   is faster and returns in sorted order, `TRUE` returns in the order of first
#'   appearance in the original data, but this requires extra work
#' @export
orderuni.integer64 <- function(table, order, nunique, keep.order=FALSE, ...) {
  .Call(C_r_ram_integer64_orderuni_asc
  , table = as.integer64(table)
  , order = as.integer(order)
  , keep.order = as.logical(keep.order)
  , ret = integer64(nunique)
  )
}

#' @rdname sortnut
#' @export
sortorderuni <- function(table, sorted, order, nunique, ...) UseMethod("sortorderuni")

#' @rdname sortnut
#' @export
sortorderuni.integer64 <- function(table, sorted, order, nunique, ...) {
  .Call(C_r_ram_integer64_sortorderuni_asc
  , table = as.integer64(table)
  , sorted = as.integer64(sorted)
  , order = as.integer(order)
  , ret = integer64(nunique)
  )
}

#' @rdname sortnut
#' @export
orderupo <- function(table, order, nunique, ...) UseMethod("orderupo")

#' @rdname sortnut
#' @export
orderupo.integer64 <- function(table, order, nunique, keep.order=FALSE, ...) {
    .Call(C_r_ram_integer64_orderupo_asc
    , table = as.integer64(table)
    , order = as.integer(order)
    , keep.order = as.logical(keep.order)
    , ret = integer(nunique)
    )
}

#' @rdname sortnut
#' @export
sortorderupo <- function(sorted, order, nunique, keep.order=FALSE, ...) UseMethod("sortorderupo")

#' @rdname sortnut
#' @export
sortorderupo.integer64 <- function(sorted, order, nunique, keep.order=FALSE, ...) {
  .Call(C_r_ram_integer64_sortorderupo_asc
    , sorted = as.integer64(sorted)
    , order = as.integer(order)
    , keep.order = as.logical(keep.order)
    , ret = integer(nunique)
    )
}

#' @rdname sortnut
#' @param nties number of tied values, usually we get this from cache or
#'   call `sortnut` or `ordernut`
#' @export
ordertie <- function(table, order, nties, ...) UseMethod("ordertie")

#' @rdname sortnut
#' @export
ordertie.integer64 <- function(table, order, nties, ...) {
  .Call(C_r_ram_integer64_ordertie_asc
  , table = as.integer64(table)
  , order = as.integer(order)
  , ret = integer(nties)
  )
}

#' @rdname sortnut
#' @export
sortordertie <- function(sorted, order, nties, ...) UseMethod("sortordertie")

#' @rdname sortnut
#' @export
sortordertie.integer64 <- function(sorted, order, nties, ...) {
  .Call(C_r_ram_integer64_sortordertie_asc
  , sorted = as.integer64(sorted)
  , order = as.integer(order)
  , ret = integer(nties)
  )
}

#' @rdname sortnut
#' @export
sorttab <- function(sorted, nunique, ...) UseMethod("sorttab")

#' @rdname sortnut
#' @export
sorttab.integer64 <- function(sorted, nunique, ...) {
  .Call(C_r_ram_integer64_sorttab_asc
  , sorted = as.integer64(sorted)
  , ret = integer(nunique)
  )
}

#' @rdname sortnut
#' @export
ordertab <- function(table, order, nunique, ...) UseMethod("ordertab")

#' @rdname sortnut
#' @param denormalize FALSE returns counts of unique values, TRUE returns each
#'   value with its counts
#' @export
ordertab.integer64 <- function(table, order, nunique, denormalize=FALSE, keep.order=FALSE, ...) {
  denormalize <- as.logical(denormalize)
  keep.order <- as.logical(keep.order)
  .Call(C_r_ram_integer64_ordertab_asc
  , table = as.integer64(table)
  , order = as.integer(order)
  , denormalize = denormalize
  , keep.order = keep.order
  , ret = integer(if (denormalize || keep.order) length(table) else nunique)
  )
}

#' @rdname sortnut
#' @export
sortordertab <- function(sorted, order, ...) UseMethod("sortordertab")

#' @rdname sortnut
#' @export
sortordertab.integer64 <- function(sorted, order, denormalize=FALSE, ...) {
  .Call(C_r_ram_integer64_sortordertab_asc
  , sorted = as.integer64(sorted)
  , order = as.integer(order)
  , denormalize = as.logical(denormalize)
  , ret = integer(length(sorted))
  )
}

#' @rdname sortnut
#' @param na.skip.num 0 or the number of `NA`s. With 0, `NA`s are coded with 1L,
#'   with the number of `NA`s, these are coded with `NA`
#' @export
orderkey <- function(table, order, na.skip.num=0L, ...) UseMethod("orderkey")

#' @rdname sortnut
#' @export
orderkey.integer64 <- function(table, order, na.skip.num=0L, ...) {
    .Call(C_r_ram_integer64_orderkey_asc
    , table = as.integer64(table)
    , order = as.integer(order)
    , na.skip.num=na.skip.num
    , ret = integer(length(table))
    )
}

#' @rdname sortnut
#' @export
sortorderkey <- function(sorted, order, na.skip.num=0L, ...) UseMethod("sortorderkey")

#' @rdname sortnut
#' @export
sortorderkey.integer64 <- function(sorted, order, na.skip.num=0L, ...) {
    .Call(C_r_ram_integer64_sortorderkey_asc
    , sorted = as.integer64(sorted)
    , order = as.integer(order)
    , na.skip.num=na.skip.num
    , ret = integer(length(sorted))
    )
}

#' @rdname sortnut
#' @param na.count the number of `NA`s, needed for this low-level function algorithm
#' @export
orderrnk <- function(table, order, na.count, ...) UseMethod("orderrnk")

#' @rdname sortnut
#' @export
orderrnk.integer64 <- function(table, order, na.count, ...) {
  .Call(C_r_ram_integer64_orderrnk_asc
  , table = as.integer64(table)
  , order = as.integer(order)
  , na.count=as.integer(na.count)
  , ret = double(length(table))
  )
}

#' @rdname sortnut
#' @export
sortorderrnk <- function(sorted, order, na.count, ...) UseMethod("sortorderrnk")
#' @rdname sortnut
#' @export
sortorderrnk.integer64 <- function(sorted, order, na.count, ...) {
  .Call(C_r_ram_integer64_sortorderrnk_asc
  , sorted = as.integer64(sorted)
  , order = as.integer(order)
  , na.count=as.integer(na.count)
  , ret = double(length(sorted))
  )
}

#' @rdname sortnut
#' @param probs vector of probabilities in `[0..1]` for which we seek quantiles
#' @export
sortqtl <- function(sorted, na.count, probs, ...) UseMethod("sortqtl")

#' @rdname sortnut
#' @export
sortqtl.integer64 <- function(sorted, na.count, probs, ...) {
    n <- length(sorted) - na.count  # nvalid
    ret <- sorted[na.count + round(1L + probs * (n-1L))]
    # TODO(#31): Remove this once `[` can return NA for integer64 directly
    ret[is.na(probs)] <- NA
    ret
}

#' @rdname sortnut
#' @export
orderqtl <- function(table, order, na.count, probs, ...) UseMethod("orderqtl")

#' @rdname sortnut
#' @export
orderqtl.integer64 <- function(table, order, na.count, probs, ...) {
  n = length(table) - na.count  # nvalid
  idx = na.count + round(1L + probs * (n-1L))
  ret = table[order[idx]]
  # TODO(#31): Remove this once `[` can return NA for integer64 directly
  ret[is.na(probs)] <- NA
  ret
}
