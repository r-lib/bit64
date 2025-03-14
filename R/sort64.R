# /*
# R-Code for sorting and ordering
# S3 atomic 64bit integers for R
# (c) 2011 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2011-12-11
# Last changed:  2011-12-11
# */

#' Low-level intger64 methods for in-RAM sorting and ordering
#'
#' Fast low-level methods for sorting and ordering. The `..sortorder`
#'   methods do sorting and ordering at once, which requires more RAM
#'   than ordering but is (almost) as fast as as sorting.
#'
#' @note
#' Note that these methods purposely violate the functional programming
#'   paradigm: they are called for the side-effect of changing some of
#'   their arguments. The `sort`-methods change `x`, the `order`-methods
#'   change `i`, and the `sortoder`-methods change both `x` and `i`
#'
#' @param x a vector to be sorted by [ramsort.integer64()] and
#'   [ramsortorder.integer64()], i.e. the output of  [sort.integer64()]
#' @param i integer positions to be modified by [ramorder.integer64()] and
#'   [ramsortorder.integer64()], default is 1:n, in this case the output is
#'   similar to [order.integer64()]
#' @param has.na boolean scalar defining whether the input vector might contain
#'   `NA`s. If we know we don't have NAs, this may speed-up. _Note_ that you
#'   risk a crash if there are unexpected `NA`s with `has.na=FALSE`
#' @param na.last boolean scalar telling ramsort whether to sort `NA`s last
#'   or first. _Note_ that 'boolean' means that there is no third option `NA`
#'   as in [sort()]
#' @param decreasing boolean scalar telling ramsort whether to sort increasing
#'   or decreasing
#' @param stable boolean scalar defining whether stable sorting is needed.
#'   Allowing non-stable may speed-up.
#' @param optimize by default ramsort optimizes for 'time' which requires more
#'   RAM, set to 'memory' to minimize RAM requirements and sacrifice speed
#' @param restlevel number of remaining recursionlevels before `quicksort`
#'   switches from recursing to `shellsort`
#' @param radixbits size of radix in bits
#' @param VERBOSE cat some info about chosen method
#' @param ... further arguments, passed from generics, ignored in methods
#'
#' @details See [bit::ramsort()]
#' @return These functions return the number of `NAs` found or assumed
#'   during sorting
#' @keywords programming manip
#' @seealso [bit::ramsort()] for the generic, `ramsort.default` for the methods
#'   provided by package ff, [sort.integer64()] for the sort interface and
#'   [sortcache()] for caching the work of sorting
#' @examples
#'   x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
#'   x
#'   message("ramsort example")
#'   s <- bit::clone(x)
#'   bit::ramsort(s)
#'   message("s has been changed in-place - whether or not ramsort uses an in-place algorithm")
#'   s
#'   message("ramorder example")
#'   s <- bit::clone(x)
#'   o <- seq_along(s)
#'   bit::ramorder(s, o)
#'   message("o has been changed in-place - s remains unchanged")
#'   s
#'   o
#'   s[o]
#'   message("ramsortorder example")
#'   o <- seq_along(s)
#'   bit::ramsortorder(s, o)
#'   message("s and o have both been changed in-place - this is much faster")
#'   s
#'   o
#' @name ramsort.integer64
NULL

#' @rdname ramsort.integer64
#' @export
shellsort.integer64 <- function(x, has.na=TRUE, na.last=FALSE, decreasing=FALSE, ...) {
  force(x)
  .Call(C_r_ram_integer64_shellsort
  , x = x
  , has_na     = as.logical(has.na)
  , na_last    = as.logical(na.last)
  , decreasing = as.logical(decreasing)
  )
}

#' @rdname ramsort.integer64
#' @export
shellsortorder.integer64 <- function(x, i, has.na=TRUE, na.last=FALSE, decreasing=FALSE, ...) {
  force(x)
  force(i)
  if (!is.integer(i))
    stop("i must be integer")
  if (length(i) != length(x))
    stop("lengths of x and i don't match")
  .Call(C_r_ram_integer64_shellsortorder
  , x = x
  , i = i
  , has_na     = as.logical(has.na)
  , na_last    = as.logical(na.last)
  , decreasing = as.logical(decreasing)
  )
}

#' @rdname ramsort.integer64
#' @export
shellorder.integer64 <- function(x, i, has.na=TRUE, na.last=FALSE, decreasing=FALSE, ...) {
  force(x)
  force(i)
  if (!is.integer(i))
    stop("i must be integer")
  if (length(i) != length(x))
    stop("lengths of x and i don't match")
  .Call(C_r_ram_integer64_shellorder
  , x = x
  , i = i
  , has_na     = as.logical(has.na)
  , na_last    = as.logical(na.last)
  , decreasing = as.logical(decreasing)
  )
}

#' @rdname ramsort.integer64
#' @export
mergesort.integer64 <- function(x, has.na=TRUE, na.last=FALSE, decreasing=FALSE, ...) {
  force(x)
  .Call(C_r_ram_integer64_mergesort
  , x = x
  , has_na     = as.logical(has.na)
  , na_last    = as.logical(na.last)
  , decreasing = as.logical(decreasing)
  )
}

#' @rdname ramsort.integer64
#' @export
mergeorder.integer64 <- function(x, i, has.na=TRUE, na.last=FALSE, decreasing=FALSE, ...) {
  force(x)
  force(i)
  if (!is.integer(i))
    stop("i must be integer")
  if (length(i) != length(x))
    stop("lengths of x and i don't match")
  .Call(C_r_ram_integer64_mergeorder
  , x = x
  , i = i
  , has_na     = as.logical(has.na)
  , na_last    = as.logical(na.last)
  , decreasing = as.logical(decreasing)
  )
}

#' @rdname ramsort.integer64
#' @export
mergesortorder.integer64 <- function(x, i, has.na=TRUE, na.last=FALSE, decreasing=FALSE, ...) {
  force(x)
  force(i)
  if (!is.integer(i))
    stop("i must be integer")
  if (length(i) != length(x))
    stop("lengths of x and i don't match")
  .Call(C_r_ram_integer64_mergesortorder
  , x = x
  , i = i
  , has_na     = as.logical(has.na)
  , na_last    = as.logical(na.last)
  , decreasing = as.logical(decreasing)
  )
}

#' @rdname ramsort.integer64
#' @export
quicksort.integer64 <- function(x,
                                has.na=TRUE,
                                na.last=FALSE,
                                decreasing=FALSE,
                                restlevel=floor(1.5 * log2(length(x))),
                                ...) {
  force(x)
  if (restlevel<0L)
    restlevel = 0L
  .Call(C_r_ram_integer64_quicksort
  , x = x
  , has_na     = as.logical(has.na)
  , na_last    = as.logical(na.last)
  , decreasing = as.logical(decreasing)
  , restlevel = as.integer(restlevel)
  )
}

#' @rdname ramsort.integer64
#' @export
quicksortorder.integer64 <- function(x, i,
                                     has.na=TRUE,
                                     na.last=FALSE,
                                     decreasing=FALSE,
                                     restlevel=floor(1.5 * log2(length(x))),
                                     ...) {
  force(x)
  force(i)
  if (!is.integer(i))
    stop("i must be integer")
  if (length(i) != length(x))
    stop("lengths of x and i don't match")
  if (restlevel<0L)
    restlevel = 0L
  .Call(C_r_ram_integer64_quicksortorder
  , x = x
  , i = i
  , has_na     = as.logical(has.na)
  , na_last    = as.logical(na.last)
  , decreasing = as.logical(decreasing)
  , restlevel = as.integer(restlevel)
  )
}

#' @rdname ramsort.integer64
#' @export
quickorder.integer64 <- function(x, i,
                                 has.na=TRUE,
                                 na.last=FALSE,
                                 decreasing=FALSE,
                                 restlevel=floor(1.5 * log2(length(x))),
                                 ...) {
  force(x)
  force(i)
  if (!is.integer(i))
    stop("i must be integer")
  if (length(i) != length(x))
    stop("lengths of x and i don't match")
  if (restlevel<0L)
    restlevel = 0L
  .Call(C_r_ram_integer64_quickorder
  , x = x
  , i = i
  , has_na     = as.logical(has.na)
  , na_last    = as.logical(na.last)
  , decreasing = as.logical(decreasing)
  , restlevel = as.integer(restlevel)
  )
}

#' @rdname ramsort.integer64
#' @export
radixsort.integer64 <- function(x, has.na=TRUE, na.last=FALSE, decreasing=FALSE, radixbits=8L, ...) {
  stopifnot(radixbits %in% c(1L, 2L, 4L, 8L, 16L))
  force(x)
  .Call(C_r_ram_integer64_radixsort
  , x = x
  , has_na     = as.logical(has.na)
  , na_last    = as.logical(na.last)
  , decreasing = as.logical(decreasing)
  , radixbits = as.integer(radixbits)
  )
}

#' @rdname ramsort.integer64
#' @export
radixsortorder.integer64 <- function(x, i, has.na=TRUE, na.last=FALSE, decreasing=FALSE, radixbits=8L, ...) {
  stopifnot(radixbits %in% c(1L, 2L, 4L, 8L, 16L))
  force(x)
  force(i)
  if (!is.integer(i))
    stop("i must be integer")
  if (length(i) != length(x))
    stop("lengths of x and i don't match")
  .Call(C_r_ram_integer64_radixsortorder
  , x = x
  , i = i
  , has_na     = as.logical(has.na)
  , na_last    = as.logical(na.last)
  , decreasing = as.logical(decreasing)
  , radixbits = as.integer(radixbits)
  )
}

#' @rdname ramsort.integer64
#' @export
radixorder.integer64 <- function(x, i, has.na=TRUE, na.last=FALSE, decreasing=FALSE, radixbits=8L, ...) {
  stopifnot(radixbits %in% c(1L, 2L, 4L, 8L, 16L))
  force(x)
  force(i)
  if (!is.integer(i))
    stop("i must be integer")
  if (length(i) != length(x))
    stop("lengths of x and i don't match")
  .Call(C_r_ram_integer64_radixorder
  , x = x
  , i = i
  , has_na     = as.logical(has.na)
  , na_last    = as.logical(na.last)
  , decreasing = as.logical(decreasing)
  , radixbits = as.integer(radixbits)
  )
}

#' @rdname ramsort.integer64
#' @export
ramsort.integer64 <- function(x,
                              has.na=TRUE,
                              na.last=FALSE,
                              decreasing=FALSE,
                              stable=TRUE,
                              optimize=c("time", "memory"),
                              VERBOSE=FALSE,
                              ...) {
  optimize <- match.arg(optimize)
  if (is.null(names(x))) {
    if (optimize == "time") {
      if (length(x) < 2048L) {
        if (VERBOSE) cat("ramsort selected mergesort\n")
        mergesort(x, has.na=has.na, na.last=na.last, decreasing=decreasing)
      } else if (length(x) < 16777216L) {
        if (VERBOSE) cat("ramsort selected radix8sort\n")
        radixsort(x, radixbits=8L, has.na=has.na, na.last=na.last, decreasing=decreasing)
      } else {
        if (VERBOSE) cat("ramsort selected radix4sort\n")
        radixsort(x, radixbits=4L, has.na=has.na, na.last=na.last, decreasing=decreasing)
      }
    } else {
      if (VERBOSE) cat("ramsort selected quicksort\n")
      quicksort(x, has.na=has.na, na.last=na.last, decreasing=decreasing)
    }
  } else {
    if (stable || optimize == "time") {
      i <- seq_along(x)
      if (length(x) < 2048L) {
        if (VERBOSE) cat("ramsortorder selected mergesortorder\n")
        ret <- mergesortorder(x, i, has.na=has.na, na.last=na.last, decreasing=decreasing)
      } else if (length(x) < 2097152L) {
        if (VERBOSE) cat("ramsortorder selected radix8sortorder\n")
        ret <- radixsortorder(x, i, radixbits=8L, has.na=has.na, na.last=na.last, decreasing=decreasing)
      } else {
        if (VERBOSE) cat("ramsortorder selected radix4sortorder\n")
        ret <- radixsortorder(x, i, radixbits=4L, has.na=has.na, na.last=na.last, decreasing=decreasing)
      }
    } else {
      if (VERBOSE) cat("ramsort selected quicksortorder\n")
      i <- seq_along(x)
      ret <- quicksortorder(x, i, has.na=has.na, na.last=na.last, decreasing=decreasing)
    }
    setattr(x, "names", names(x)[i])
    ret
  }
}

#' @rdname ramsort.integer64
#' @export
ramsortorder.integer64 <- function(x, i,
                                   has.na=TRUE,
                                   na.last=FALSE,
                                   decreasing=FALSE,
                                   stable=TRUE,
                                   optimize=c("time", "memory"),
                                   VERBOSE=FALSE,
                                   ...) {
  optimize <- match.arg(optimize)
  if (!is.null(names(x)) && !is.null(names(i))) stop("names not supported")
  if (stable || optimize == "time") {
    if (length(x) < 2048L) {
      if (VERBOSE) cat("ramsortorder selected mergesortorder\n")
      mergesortorder(x, i, has.na=has.na, na.last=na.last, decreasing=decreasing)
    } else if (length(x) < 16777216L) {
      if (VERBOSE) cat("ramsortorder selected radix8sortorder\n")
      radixsortorder(x, i, radixbits=8L, has.na=has.na, na.last=na.last, decreasing=decreasing)
    } else {
      if (VERBOSE) cat("ramsortorder selected radix4sortorder\n")
      radixsortorder(x, i, radixbits=4L, has.na=has.na, na.last=na.last, decreasing=decreasing)
    }
  } else {
    if (VERBOSE) cat("ramsortorder selected quicksortorder\n")
    quicksortorder(x, i, has.na=has.na, na.last=na.last, decreasing=decreasing)
  }
}

#' @rdname ramsort.integer64
#' @export
ramorder.integer64 <- function(x, i,
                               has.na=TRUE,
                               na.last=FALSE,
                               decreasing=FALSE,
                               stable=TRUE,
                               optimize=c("time", "memory"),
                               VERBOSE=FALSE,
                               ...) {
  optimize <- match.arg(optimize)
  if (!is.null(names(x)) || !is.null(names(i))) stop("names not supported")
  if (stable) {
    if (VERBOSE) cat("ramorder selected mergeorder\n")
    mergeorder(x, i, has.na=has.na, na.last=na.last, decreasing=decreasing)
  } else {
    if (VERBOSE) cat("ramorder selected quickorder\n")
    quickorder(x, i, has.na=has.na, na.last=na.last, decreasing=decreasing)
  }
}

#' High-level intger64 methods for sorting and ordering
#'
#' Fast high-level methods for sorting and ordering. These are wrappers to
#'   [ramsort.integer64()] and friends and do not modify their arguments.
#'
#' @param x a vector to be sorted by [ramsort.integer64()] and
#'   [ramsortorder.integer64()], i.e. the output of  [sort.integer64()]
#' @param has.na boolean scalar defining whether the input vector might
#'   contain `NA`s. If we know we don't have NAs, this may speed-up. _Note_
#'   that you risk a crash if there are unexpected `NA`s with `has.na=FALSE`
#' @param na.last boolean scalar telling ramsort whether to sort `NA`s last
#'   or first. _Note_ that 'boolean' means that there is no third option
#'   `NA` as in [sort()]
#' @param decreasing boolean scalar telling ramsort whether to sort
#'   increasing or decreasing
#' @param stable boolean scalar defining whether stable sorting is needed.
#'   Allowing non-stable may speed-up.
#' @param optimize by default ramsort optimizes for 'time' which requires
#'   more RAM, set to 'memory' to minimize RAM requirements and sacrifice speed
#' @param VERBOSE cat some info about chosen method
#' @param ... further arguments, passed from generics, ignored in methods
#'
#' @details see [sort()] and [order()]
#' @return `sort` returns the sorted vector and `vector` returns the order positions.
#' @keywords programming manip
#' @seealso [`sort()`][sort.integer64], [sortcache()]
#' @examples
#'   x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
#'   x
#'   sort(x)
#'   message("the following has default optimize='time' which is faster but requires more RAM
#' , this calls 'ramorder'")
#'   order.integer64(x)
#'   message("slower with less RAM, this calls 'ramsortorder'")
#'   order.integer64(x, optimize="memory")
#' @name sort.integer64
NULL

#' @rdname sort.integer64
#' @export
sort.integer64 <- function(x,
                           decreasing=FALSE,
                           has.na=TRUE,
                           na.last=TRUE,
                           stable=TRUE,
                           optimize=c("time", "memory"),
                           VERBOSE=FALSE,
                           ...) {
  do.na.last <- is.na(na.last) || na.last
  cache_env <- cache(x)
  if (!is.null(cache_env$sort)) {
    if (do.na.last || decreasing) {
      na.count <- cache_env$na.count
      s <- double(length(x))
      .Call(C_r_ram_integer64_sortsrt,
        x = cache_env$sort,
        na_count = as.integer(na.count),
        na_last = as.logical(do.na.last),
        decreasing = as.logical(decreasing),
        s = s
      )
      setattr(s, "class", "integer64")
    } else {
      s <- cache_env$sort  # here we save copying at all
    }
  } else if (!is.null(cache_env$order)) {
    if (do.na.last || decreasing) {
      na.count <- cache_env$na.count
      s <- double(length(x))
      .Call(C_r_ram_integer64_sortsrt,
        x = x[cache_env$order],
        na_count = as.integer(na.count),
        na_last = as.logical(do.na.last),
        decreasing = as.logical(decreasing),
        s = s
      )
      setattr(s, "class", "integer64")
    } else {
      s <- x[cache_env$order]
    }
  } else {
    if (identical(cache_env$na.count, 0L))
      has.na <- FALSE
    s <- clone(x)
    na.count <- ramsort(
        s
    , has.na=has.na
    , na.last=do.na.last
    , decreasing=decreasing
    , stable=stable
    , optimize = optimize
    , VERBOSE = FALSE
    )
  }
  if (is.na(na.last) && na.count)
    length(s) <- length(s) - na.count
  s
}

#' @rdname sort.integer64
#' @export
order.integer64 <- function(...,
                            na.last=TRUE,
                            decreasing=FALSE,
                            has.na=TRUE,
                            stable=TRUE,
                            optimize=c("time", "memory"),
                            VERBOSE=FALSE) {
  do.na.last <- is.na(na.last) || na.last
    # COPY ON MODIFY is broken for reading from list(...)
    # because list(...) creates a copy of all ... and this invalidates our caches
    # therefore we go this sick workaround
    argsymbols <- as.list(substitute(list(...)))[-1L]
    argframe <- parent.frame()
    A <- function(i) eval(argsymbols[[i]], argframe)
    N <- length(argsymbols)
  if (N!=1L)
    stop("can only order one vector at the moment")
  x <- A(1L)
  cache_env <- cache(x)
  if (!is.null(cache_env$order)) {
    if (do.na.last || decreasing) {
        o <- integer(length(x))
        if (is.null(cache_env$sort)) {
          na.count <- cache_env$na.count
          .Call(C_r_ram_integer64_orderord,
            x = x,
            i = cache_env$order,
            na_count = as.integer(na.count),
            na_last = as.logical(do.na.last),
            decreasing = as.logical(decreasing),
            o = o
          )
        } else {
          na.count <- cache_env$na.count
          .Call(C_r_ram_integer64_sortorderord,
            x = cache_env$sort,
            i = cache_env$order,
            na_count = as.integer(na.count),
            na_last = as.logical(do.na.last),
            decreasing = as.logical(decreasing),
            o = o
          )
        }
      } else {
        o <- cache_env$order  # here we save copying at all
      }
  } else {
    if (identical(cache_env$na.count, 0L))
      has.na <- FALSE
    optimize <- match.arg(optimize)
    o <- seq_along(x)
    if (optimize == "time") {
        s <- clone(x)
        na.count <- ramsortorder(s, o
        , has.na=has.na
        , na.last=do.na.last
        , decreasing=decreasing
        , stable=stable
        , optimize = optimize
        , VERBOSE = FALSE
        )
    } else {
        na.count <- ramorder(x, o
        , has.na=has.na
        , na.last=do.na.last
        , decreasing=decreasing
        , stable=stable
        , optimize = optimize
        , VERBOSE = FALSE
        )
    }
  }
  if (is.na(na.last) && na.count)
    length(o) <- length(o) - na.count
  o
}
