# /*
# R-Code for hashing
# S3 atomic 64bit integers for R
# (c) 2011 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2011-12-11
# Last changed:  2011-12-11
# */


#' Hashing for 64bit integers
#'
#' This is an explicit implementation of hash functionality that underlies
#' matching and other functions in R. Explicit means that you can create,
#' store and use hash functionality directly. One advantage is that you can
#' re-use hashmaps, which avoid re-building hashmaps again and again.
#'
#' @param x an integer64 vector
#' @param minfac minimum factor by which the hasmap has more elements compared to the data `x`,
#'   ignored if `hashbits` is given directly
#' @param hashbits length of hashmap is `2^hashbits`
#' @param cache an optional [cache()] object into which to put the hashmap (by default a new cache is created
#' @param nunique giving _correct_ number of unique elements can help reducing the size of the hashmap
#' @param nomatch the value to be returned if an element is not found in the hashmap
#' @param keep.order determines order of results and speed: `FALSE` (the default) is faster and returns in the
#'   (pseudo)random order of the hash function, `TRUE` returns in the order of first appearance in the original
#'   data, but this requires extra work
#' @param ... further arguments, passed from generics, ignored in methods
#'
#' @details
# nolint start: line_length_linter.
#' | **function** | **see also**                            | **description** |
#' |-------------:|----------------------------------------:|:----------------|
#' |    `hashfun` |                                `digest` | export of the hash function used in `hashmap` |
#' |    `hashmap` |            [`match()`][match.integer64] | return hashmap  |
#' |    `hashpos` |            [`match()`][match.integer64] | return positions of `x` in `hashmap` |
#' |    `hashrev` |            [`match()`][match.integer64] | return positions of `hashmap` in `x` |
#' |    `hashfin` |                      [`%in%.integer64`] | return logical whether `x` is in `hashmap` |
#' |    `hashrin` |                      [`%in%.integer64`] | return logical whether `hashmap` is in `x` |
#' |    `hashdup` |  [`duplicated()`][duplicated.integer64] | return logical whether hashdat is duplicated using hashmap |
#' |    `hashuni` |          [`unique()`][unique.integer64] | return unique values of hashmap |
#' | `hashmapuni` |          [`unique()`][unique.integer64] | return unique values of `x` |
#' |    `hashupo` |          [`unique()`][unique.integer64] | return positions of unique values in hashdat |
#' | `hashmapupo` |          [`unique()`][unique.integer64] | return positions of unique values in `x` |
#' |    `hashtab` |            [`table()`][table] | tabulate values of hashdat using hashmap in `keep.order=FALSE` |
#' | `hashmaptab` |            [`table()`][table] | tabulate values of `x` building hasmap on the fly in `keep.order=FALSE` |
# nolint end: line_length_linter.
#'
#' @return See Details
#' @keywords programming manip
#' @seealso [`match()`][match.integer64], [runif64()]
#' @examples
#' x <- as.integer64(sample(c(NA, 0:9)))
#' y <- as.integer64(sample(c(NA, 1:9), 10, TRUE))
#' hashfun(y)
#' hx <- hashmap(x)
#' hy <- hashmap(y)
#' ls(hy)
#' hashpos(hy, x)
#' hashrev(hx, y)
#' hashfin(hy, x)
#' hashrin(hx, y)
#' hashdup(hy)
#' hashuni(hy)
#' hashuni(hy, keep.order=TRUE)
#' hashmapuni(y)
#' hashupo(hy)
#' hashupo(hy, keep.order=TRUE)
#' hashmapupo(y)
#' hashtab(hy)
#' hashmaptab(y)
#'
#' stopifnot(identical(match(as.integer(x), as.integer(y)), hashpos(hy, x)))
#' stopifnot(identical(match(as.integer(x), as.integer(y)), hashrev(hx, y)))
#' stopifnot(identical(as.integer(x) %in% as.integer(y), hashfin(hy, x)))
#' stopifnot(identical(as.integer(x) %in% as.integer(y), hashrin(hx, y)))
#' stopifnot(identical(duplicated(as.integer(y)), hashdup(hy)))
#' stopifnot(identical(as.integer64(unique(as.integer(y))), hashuni(hy, keep.order=TRUE)))
#' stopifnot(identical(sort(hashuni(hy, keep.order=FALSE)), sort(hashuni(hy, keep.order=TRUE))))
#' stopifnot(identical(y[hashupo(hy, keep.order=FALSE)], hashuni(hy, keep.order=FALSE)))
#' stopifnot(identical(y[hashupo(hy, keep.order=TRUE)], hashuni(hy, keep.order=TRUE)))
#' stopifnot(identical(hashpos(hy, hashuni(hy, keep.order=TRUE)), hashupo(hy, keep.order=TRUE)))
#' stopifnot(identical(hashpos(hy, hashuni(hy, keep.order=FALSE)), hashupo(hy, keep.order=FALSE)))
#' stopifnot(identical(hashuni(hy, keep.order=FALSE), hashtab(hy)$values))
#' stopifnot(identical(as.vector(table(as.integer(y), useNA="ifany"))
#' , hashtab(hy)$counts[order.integer64(hashtab(hy)$values)]))
#' stopifnot(identical(hashuni(hy, keep.order=TRUE), hashmapuni(y)))
#' stopifnot(identical(hashupo(hy, keep.order=TRUE), hashmapupo(y)))
#' stopifnot(identical(hashtab(hy), hashmaptab(y)))
#'
#'     \dontrun{
#'     message("explore speed given size of the hasmap in 2^hashbits and size of the data")
#'     message("more hashbits means more random access and less collisions")
#'     message("i.e. more data means less random access and more collisions")
#'     bits <- 24
#'     b <- seq(-1, 0, 0.1)
#'     tim <- matrix(NA, length(b), 2, dimnames=list(b, c("bits", "bits+1")))
#'     for (i in 1:length(b)) {
#'       n <- as.integer(2^(bits+b[i]))
#'       x <- as.integer64(sample(n))
#'       tim[i, 1] <- repeat.time(hashmap(x, hashbits=bits))[3]
#'       tim[i, 2] <- repeat.time(hashmap(x, hashbits=bits+1))[3]
#'       print(tim)
#'       matplot(b, tim)
#'     }
#'     message("we conclude that n*sqrt(2) is enough to avoid collisions")
#'     }
#' @name hashmap
NULL

#' @rdname hashmap
#' @export
hashfun <- function(x, ...) UseMethod("hashfun")
#' @rdname hashmap
#' @export
hashfun.integer64 <- function(x, minfac=1.41, hashbits=NULL, ...) {
  n <- length(x)
  if (is.null(hashbits)) {
    minlen <- ceiling(n*minfac)
    if (minlen > 0L)
      hashbits <- as.integer(ceiling(log2(minlen)))
    else
      hashbits <- 0L
  } else {
    hashbits <- as.integer(hashbits)
  }
  .Call(C_hashfun_integer64, x, hashbits, integer(n))
}

#' @rdname hashmap
#' @export
hashmap <- function(x, ...) UseMethod("hashmap")

#' @rdname hashmap
#' @export
hashmap.integer64 <- function(x, nunique=NULL, minfac=1.41, hashbits=NULL, cache=NULL, ...) {
  if (is.null(nunique)) {
    nunique <- integer(1L)
    n <- length(x)
  } else {
    nunique <- as.integer(nunique)
    n <- nunique
  }
  if (is.null(hashbits)) {
    minlen <- ceiling(n*minfac)
    if (minlen > 0L)
      hashbits <- as.integer(ceiling(log2(minlen)))
    else
      hashbits <- 0L
  } else {
    hashbits <- as.integer(hashbits)
  }
  nhash <- as.integer(2L^hashbits)
  hashmap <- integer(nhash)
  .Call(C_hashmap_integer64, x, hashbits, hashmap, nunique)

  if (is.null(cache))
      cache <- newcache(x)
  else
    if (!still.identical(x, get("x", envir=cache, inherits=FALSE)))
          stop("vector 'x' dissociated from cache")
  assign("hashmap", hashmap, envir=cache)
  assign("hashbits", hashbits, envir=cache)
  assign("nhash", nhash, envir=cache)
  assign("nunique", nunique, envir=cache)
  cache
}

#' @rdname hashmap
#' @export
hashpos <- function(cache, ...) UseMethod("hashpos")
#' @rdname hashmap
#' @export
hashpos.cache_integer64 <- function(cache, x, nomatch = NA_integer_, ...) {
  hashbits <- get("hashbits", envir=cache, inherits=FALSE)
  hashmap <- get("hashmap", envir=cache, inherits=FALSE)
  hashdat <- get("x", envir=cache, inherits=FALSE)
  .Call(C_hashpos_integer64, as.integer64(x), hashdat, hashbits, hashmap, as.integer(nomatch), integer(length(x)))
}

#' @rdname hashmap
#' @export
hashrev <- function(cache, ...) UseMethod("hashrev")
#' @rdname hashmap
#' @export
hashrev.cache_integer64 <- function(cache, x, nomatch = NA_integer_, ...) {
  hashbits <- get("hashbits", envir=cache, inherits=FALSE)
  hashmap <- get("hashmap", envir=cache, inherits=FALSE)
  hashdat <- get("x", envir=cache, inherits=FALSE)
  nunique <- get("nunique", envir=cache, inherits=FALSE)
  .Call(C_hashrev_integer64,
    as.integer64(x),
    hashdat, hashbits, hashmap, nunique,
    as.integer(nomatch),
    integer(length(hashdat))
  )
}

#' @rdname hashmap
#' @export
hashfin <- function(cache, ...) UseMethod("hashfin")
#' @rdname hashmap
#' @export
hashfin.cache_integer64 <- function(cache, x, ...) {
  hashbits <- get("hashbits", envir=cache, inherits=FALSE)
  hashmap <- get("hashmap", envir=cache, inherits=FALSE)
  hashdat <- get("x", envir=cache, inherits=FALSE)
  .Call(C_hashfin_integer64, as.integer64(x), hashdat, hashbits, hashmap, logical(length(x)))
}

#' @rdname hashmap
#' @export
hashrin <- function(cache, ...) UseMethod("hashrin")
#' @rdname hashmap
#' @export
hashrin.cache_integer64 <- function(cache, x, ...) {
  hashbits <- get("hashbits", envir=cache, inherits=FALSE)
  hashmap <- get("hashmap", envir=cache, inherits=FALSE)
  hashdat <- get("x", envir=cache, inherits=FALSE)
  .Call(C_hashrin_integer64, as.integer64(x), hashdat, hashbits, hashmap, nunique, logical(length(hashdat)))
}

#' @rdname hashmap
#' @export
hashdup <- function(cache, ...) UseMethod("hashdup")
#' @rdname hashmap
#' @export
hashdup.cache_integer64 <- function(cache, ...) {
  hashbits <- get("hashbits", envir=cache, inherits=FALSE)
  hashmap <- get("hashmap", envir=cache, inherits=FALSE)
  hashdat <- get("x", envir=cache, inherits=FALSE)
  nunique <- get("nunique", envir=cache, inherits=FALSE)
  .Call(C_hashdup_integer64, hashdat, hashbits, hashmap, nunique, logical(length(hashdat)))
}

#' @rdname hashmap
#' @export
hashuni <- function(cache, ...) UseMethod("hashuni")
#' @rdname hashmap
#' @export
hashuni.cache_integer64 <- function(cache, keep.order=FALSE, ...) {
  hashbits <- get("hashbits", envir=cache, inherits=FALSE)
  hashmap <- get("hashmap", envir=cache, inherits=FALSE)
  hashdat <- get("x", envir=cache, inherits=FALSE)
  nunique <- get("nunique", envir=cache, inherits=FALSE)
  ret <- .Call(C_hashuni_integer64, hashdat, hashbits, hashmap, as.logical(keep.order), double(nunique))
  oldClass(ret) <- "integer64"
  ret
}

#' @rdname hashmap
#' @export
hashupo <- function(cache, ...) UseMethod("hashupo")
#' @rdname hashmap
#' @export
hashupo.cache_integer64 <- function(cache, keep.order=FALSE, ...) {
  hashbits <- get("hashbits", envir=cache, inherits=FALSE)
  hashmap <- get("hashmap", envir=cache, inherits=FALSE)
  hashdat <- get("x", envir=cache, inherits=FALSE)
  nunique <- get("nunique", envir=cache, inherits=FALSE)
  .Call(C_hashupo_integer64, hashdat, hashbits, hashmap, as.logical(keep.order), integer(nunique))
}

# just returns a vector of length nunique of counts of the values
# at positions hashupo(, keep.order=FALSE) which are those of hashuni(, keep.order=FALSE)
#' @rdname hashmap
#' @export
hashtab <- function(cache, ...) UseMethod("hashtab")
#' @rdname hashmap
#' @export
hashtab.cache_integer64 <- function(cache, ...) {
  hashbits <- get("hashbits", envir=cache, inherits=FALSE)
  hashmap <- get("hashmap", envir=cache, inherits=FALSE)
  hashdat <- get("x", envir=cache, inherits=FALSE)
  nunique <- get("nunique", envir=cache, inherits=FALSE)
  ret <- .Call(C_hashtab_integer64, hashdat, hashbits, hashmap, nunique)
  attr(ret, "names") <- c("values", "counts")
  ret
}

#' @rdname hashmap
#' @export
hashmaptab <- function(x, ...) UseMethod("hashmaptab")
#' @rdname hashmap
#' @export
hashmaptab.integer64 <- function(x, nunique=NULL, minfac=1.5, hashbits=NULL, ...) {
  if (is.null(nunique)) {
    nunique <- integer(1L)
    n <- length(x)
  } else {
    nunique <- as.integer(nunique)
    n <- nunique
  }
  if (is.null(hashbits))
    hashbits <- as.integer(ceiling(log2(n*minfac)))
  else
    hashbits <- as.integer(hashbits)
  nhash <- as.integer(2L^hashbits)
  hashmap <- integer(nhash)
  ret <- .Call(C_hashmaptab_integer64, x, hashbits, hashmap, nunique)
  # theoretically we could use {hashmap, nunique} at this point the same way like after calling hashmap_integer64
  attr(ret, "names") <- c("values", "counts")
  ret
}

#' @rdname hashmap
#' @export
hashmapuni <- function(x, ...) UseMethod("hashmapuni")
#' @rdname hashmap
#' @export
hashmapuni.integer64 <- function(x, nunique=NULL, minfac=1.5, hashbits=NULL, ...) {
  if (is.null(nunique)) {
    nunique <- integer(1L)
    n <- length(x)
  } else {
    nunique <- as.integer(nunique)
    n <- nunique
  }
  if (is.null(hashbits)) {
    minlen <- ceiling(n*minfac)
    if (minlen > 0L)
      hashbits <- as.integer(ceiling(log2(minlen)))
    else
      hashbits <- 0L
  } else {
    hashbits <- as.integer(hashbits)
  }
  nhash <- as.integer(2L^hashbits)
  hashmap <- integer(nhash)
  ret <- .Call(C_hashmapuni_integer64, x, hashbits, hashmap, nunique)
  # theoretically we could use {hashmap, nunique} at this point the same way like after calling hashmap_integer64
  oldClass(ret) <- "integer64"
  ret
}

#' @rdname hashmap
#' @export
hashmapupo <- function(x, ...) UseMethod("hashmapupo")
#' @rdname hashmap
#' @export
hashmapupo.integer64 <- function(x, nunique=NULL, minfac=1.5, hashbits=NULL, ...) {
  if (is.null(nunique)) {
    nunique <- integer(1L)
    n <- length(x)
  } else {
    nunique <- as.integer(nunique)
    n <- nunique
  }
  if (is.null(hashbits)) {
    minlen <- ceiling(n*minfac)
    if (minlen > 0L)
      hashbits <- as.integer(ceiling(log2(minlen)))
    else
      hashbits <- 0L
  } else {
    hashbits <- as.integer(hashbits)
  }
  nhash <- as.integer(2L^hashbits)
  hashmap <- integer(nhash)
  # theoretically we could use {hashmap, nunique} at this point the same way like after calling hashmap_integer64
  .Call(C_hashmapupo_integer64, x, hashbits, hashmap, nunique)
}


#' integer64: random numbers
#'
#' Create uniform random 64-bit integers within defined range
#'
#' @param n length of return vector
#' @param min lower inclusive bound for random numbers
#' @param max upper inclusive bound for random numbers
#' @param replace set to FALSE for sampleing from a finite pool, see [sample()]
#'
#' @return a integer64 vector
#'
#' @details
#' For each random integer we call R's internal C interface `unif_rand()` twice.
#'   Each call is mapped to 2^32 unsigned integers. The two 32-bit patterns are
#'   concatenated to form the new integer64. This process is repeated until the
#'   result is not a `NA_INTEGER64_`.
#' @keywords classes distribution sysdata
#' @seealso [runif()], [hashfun()]
#'
#' @examples
#'   runif64(12)
#'   runif64(12, -16, 16)
#'   runif64(12, 0, as.integer64(2^60)-1)  # not 2^60-1 !
#'   var(runif(1e4))
#'   var(as.double(runif64(1e4, 0, 2^40))/2^40)  # ~ = 1/12 = .08333
#'
#'   table(sample(16, replace=FALSE))
#'   table(runif64(16, 1, 16, replace=FALSE))
#'   table(sample(16, replace=TRUE))
#'   table(runif64(16, 1, 16, replace=TRUE))
#'
#' @export
runif64 <- function(n, min=lim.integer64()[1L], max=lim.integer64()[2L], replace = TRUE) {
  n <- as.integer(n)
  min <- as.integer64(min)
  max <- as.integer64(max)
  if (replace) {
    ret <- .Call(C_runif_integer64, n, min, max)
    oldClass(ret) <- "integer64"
  } else {
    N <- n
    d <- max - min + 1L
    if (!is.na(d) && N > d)
      stop("cannot take a sample larger than the population when 'replace = FALSE'")
    if (!is.na(d) && n > d  / (2.0*log(n, 64.0))) {
      ret <- .Call(C_runif_integer64, as.integer(d), as.integer64(min), as.integer64(max))
      oldClass(ret) <- "integer64"
      ret <- sample(ret, n, FALSE)
    } else {
      ret <- integer64()
      while (N > 0L) {
        ret <- unique(c(ret, Recall(
          if (N*1.05 < .Machine$integer.max) N*1.05 else N
        , min
        , max
        , replace=TRUE
        )))
        N <- n - length(ret)
      }
      if (N != 0L)
        ret <- ret[1:n]
    }
  }
  ret
}
