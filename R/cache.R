# /*
# R-Code for caching
# S3 atomic 64bit integers for R
# (c) 2011 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2011-12-11
# Last changed:  2011-12-11
# */

#' Atomic Caching
#'
#' Functions for caching results attached to atomic objects
#'
#' @param x an integer64 vector (or a cache object in case of `print.cache`)
#'
#' @details
#' A `cache` is an [environment] attached to an atomic object with the
#'   [attribute] name 'cache'. It contains at least a reference to the
#'   atomic object that carries the cache. This is used when accessing
#'   the cache to detect whether the object carrying the cache has been
#'   modified meanwhile.
#'
#' @return See details
#' @seealso
#'   [bit::still.identical()] for testing whether to symbols point to the same RAM.
#'
#' Functions that get and set small cache-content automatically when a cache is present:
#'   [bit::na.count()], [bit::nvalid()], [bit::is.sorted()], [bit::nunique()] and
#'   [bit::nties()]
#'
#' Setting big caches with a relevant memory footprint requires a conscious decision
#'   of the user: [hashcache], [sortcache], [ordercache], [sortordercache]
#'
#' Functions that use big caches: [match.integer64()], [%in%.integer64],
#'   [duplicated.integer64()], [unique.integer64()], [unipos()], [table.integer64()],
#'   [as.factor.integer64()], [as.ordered.integer64()], [keypos()], [tiepos()],
#'   [rank.integer64()], [prank()], [qtile()], [quantile.integer64()],
#'   [median.integer64()], and [summary.integer64()]
#'
#' @examples
#'   x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
#'   y <- x
#'   still.identical(x,y)
#'   y[1] <- NA
#'   still.identical(x,y)
#'   mycache <- newcache(x)
#'   ls(mycache)
#'   mycache
#'   rm(mycache)
#'   jamcache(x)
#'   cache(x)
#'   x[1] <- NA
#'   cache(x)
#'   getcache(x, "abc")
#'   setcache(x, "abc", 1)
#'   getcache(x, "abc")
#'   remcache(x)
#'   cache(x)
#' @keywords environment
#' @name cache
NULL

#still.identical <- function(x, y){
#  .Call(C_r_ram_truly_identical, x = x, y = y, PACKAGE = "bit64")
#}

#' @describeIn cache creates a new cache referencing  `x`
#' @export
newcache <- function(x) {
    env <- new.env()
    vmode <- typeof(x)
    if (vmode=="double" && is.integer64(x))
      vmode <- "integer64"
    setattr(env, "class", c(paste("cache", vmode, sep="_"),"cache","environment"))
    assign("x", x, envir=env)
    env
}

#' @describeIn cache forces `x` to have a cache
#' @export
jamcache <- function(x) {
    cache <- attr(x, "cache")
    if (is.null(cache)){
        cache <- newcache(x)
        setattr(x, "cache", cache)
    }else
        if (!bit::still.identical(x, get("x", envir=cache, inherits=FALSE))){
            cache <- newcache(x)
            setattr(x, "cache", cache)
            warning("replaced outdated cache with empty cache")
        }
    cache
}

#' @describeIn cache returns the cache attached to `x` if it is not
#'   found to be outdated
#' @export
cache <- function(x) {
    cache <- attr(x, "cache")
    if (is.null(cache) || bit::still.identical(x, get("x", envir=cache, inherits=FALSE)))
        cache
    else{
        remcache(x)
        warning("removed outdated cache")
        NULL
    }
}

#' @describeIn cache assigns a value into the cache of `x`
#' @param which A character naming the object to be retrieved from the cache or to be stored in the cache
#' @param value An object to be stored in the cache
#' @export
setcache <- function(x, which, value) {
      env <- jamcache(x)
      assign(which, value, envir=env)
      env
}

#' @describeIn cache gets cache value 'which' from `x`
#' @export
getcache <- function(x, which){
    cache <- attr(x, "cache")
    if (is.null(cache))
      return(NULL)
    if (bit::still.identical(x, get("x", envir=cache, inherits=FALSE))){
        if (exists(which, envir=cache, inherits=FALSE))
            get(which, envir=cache, inherits=FALSE)
        else
            NULL
    }else{
        remcache(x)
        warning("removed outdated cache")
        NULL
    }
}

#' @describeIn cache removes the cache from `x`
#' @export
remcache <- function(x) {
        setattr(x, "cache", NULL)
    invisible()
}

#' @rdname cache
#' @param all.names,pattern passed to [ls()] when listing the cache content
#' @param ... ignored
#' @export
print.cache<- function(x, all.names=FALSE, pattern, ...){
  l <- ls(x, all.names, pattern=pattern)
  cat(class(x)[1L], ": ", paste(l, collapse=" - "), "\n", sep="")
  invisible(l)
}

#' Big caching of hashing, sorting, ordering
#'
#' Functions to create cache that accelerates many operations
#'
#' @param x an atomic vector (note that currently only integer64 is supported)
#' @param nunique giving _correct_ number of unique elements can help reducing
#'   the size of the hashmap
#' @param ... passed to [hashmap()]
#'
#' @details
#' The result of relative expensive operations [hashmap()], [ramsort()],
#'   [ramsortorder()], and [ramorder()] can be stored in a cache in order to
#'   avoid multiple excutions. Unless in very specific situations, the
#'   recommended method is `hashsortorder` only.
#'
#' @note
#'   Note that we consider storing the big results from sorting and/or ordering
#'   as a relevant side-effect, and therefore storing them in the cache should
#'   require a conscious decision of the user.
#'
#' @return `x` with a [cache()] that contains the result of the expensive operations,
#'   possible together with small derived information (such as [nunique.integer64()])
#'   and previously cached results.
#'
#' @seealso
#'   [cache()] for caching functions and [nunique.integer64()] for methods benefiting
#'   from small caches
#'
#' @examples
#'   x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
#'   sortordercache(x)
#'
#' @keywords environment
#' @export
hashcache <-function(x, nunique=NULL, ...){
    env <- jamcache(x)
    if (is.null(nunique))
        nunique <- env$nunique
    env <- hashmap(x, nunique=nunique, cache=env, ...)
    if (is.null(nunique) && env$nunique<sqrt(length(x)))
        env <- hashmap(x, nunique=env$nunique, cache=env, ...)
    na.count(x) # since x has cache, na.count() will update the cache, unless its already there
    # different from sortcache, ordercache and sortordercache we do not set nties: hastab is too expensive
    invisible(env)
}

#' @rdname hashcache
#' @param has.na boolean scalar defining whether the input vector might contain
#'    `NA`s. If we know we don't have `NA`s, this may speed-up. _Note_ that you
#'    risk a crash if there are unexpected `NA`s with `has.na=FALSE`.
#' @export
sortcache <- function(x, has.na = NULL){
    if (is.null(has.na)){
        na.count <- getcache(x, "na.count")
        if (is.null(na.count))
            has.na <- TRUE
        else
            has.na <- na.count > 0L
    }
    s <- clone(x)
    na.count <- ramsort(s, has.na = has.na, na.last = FALSE, decreasing = FALSE, stable = FALSE, optimize = "time")
    nut <- .Call(C_r_ram_integer64_sortnut, x = s, PACKAGE = "bit64")
    setcache(x, "sort", s)
    setcache(x, "na.count", na.count)
    setcache(x, "nunique", nut[[1L]])
    setcache(x, "nties", nut[[2L]])
    invisible(x)
}

#' @rdname hashcache
#' @param stable boolean scalar defining whether stable sorting is needed. Allowing
#'   non-stable may speed-up.
#' @export
sortordercache <- function(x, has.na = NULL, stable = NULL){
    if (is.null(has.na)){
        na.count <- getcache(x, "na.count")
        if (is.null(na.count))
            has.na <- TRUE
        else
            has.na <- na.count > 0L
    }
    if (is.null(stable)){
        nunique <- getcache(x, "nunique")
        if (is.null(nunique))
          stable <- TRUE
        else
          stable <- nunique < length(x)
    }
    s <- clone(x)
    o <- seq_along(x)
    na.count <- ramsortorder(s, o, has.na = has.na, na.last = FALSE, decreasing = FALSE, stable = stable, optimize = "time")
    nut <- .Call(C_r_ram_integer64_sortnut, x = s, PACKAGE = "bit64")
    setcache(x, "sort", s)
    setcache(x, "order", o)
    setcache(x, "na.count", na.count)
    setcache(x, "nunique", nut[[1L]])
    setcache(x, "nties", nut[[2L]])
    invisible(x)
}

#' @rdname hashcache
#' @param optimize by default ramsort optimizes for 'time' which requires more RAM,
#'   set to 'memory' to minimize RAM requirements and sacrifice speed.
#' @export
ordercache <- function(x, has.na = NULL, stable = NULL, optimize = "time"){
    if (is.null(has.na)){
        na.count <- getcache(x, "na.count")
        if (is.null(na.count))
            has.na <- TRUE
        else
            has.na <- na.count > 0L
    }
    if (is.null(stable)){
        nunique <- getcache(x, "nunique")
        if (is.null(nunique))
          stable <- TRUE
        else
          stable <- nunique < length(x)
    }
    o <- seq_along(x)
    na.count <- ramorder(x, o, has.na = has.na, na.last = FALSE, decreasing = FALSE, stable = stable, optimize = optimize)
    nut <- .Call(C_r_ram_integer64_ordernut, table = x, order = o, PACKAGE = "bit64")
    setcache(x, "order", o)
    setcache(x, "na.count", na.count)
    setcache(x, "nunique", nut[[1L]])
    setcache(x, "nties", nut[[2L]])
    invisible(x)
}

#' Small cache access methods
#'
#' These methods are packaged here for methods in packages `bit64` and `ff`.
#'
#' @param x some object
#' @param ... ignored
#'
#' @details
#' All these functions benefit from a [sortcache()], [ordercache()] or
#'   [sortordercache()]. `na.count()`, `nvalid()` and `nunique()` also
#'   benefit from a [hashcache()].
#'
#' @note
#' If a [cache()] exists but the desired value is not cached, then these
#'   functions will store their result in the cache. We do not consider this
#'   a relevant side-effect, since these small cache results do not have a
#'   relevant memory footprint.
#'
#' @return
#' `is.sorted` returns a logical scalar, the other methods return an integer scalar.
#'
#' @seealso
#' [cache()] for caching functions and [sortordercache()] for functions creating big caches
#'
#' @examples
#'  x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
#'  length(x)
#'  na.count(x)
#'  nvalid(x)
#'  nunique(x)
#'  nties(x)
#'  table.integer64(x)
#'  x
#'
#' @keywords environment methods
#' @name is.sorted.integer64
NULL

#' @describeIn is.sorted.integer64 returns the number of `NA`s
#' @export
na.count.integer64 <- function(x, ...){
  env <- cache(x)
  if (is.null(env))
    return(.Call(C_r_ram_integer64_nacount, x = x, PACKAGE = "bit64"))
  if (exists("na.count", envir=env, inherits=FALSE))
    return(get("na.count", envir=env, inherits=FALSE))
  ret <- .Call(C_r_ram_integer64_nacount, x = x, PACKAGE = "bit64")
  assign("na.count", ret, envir=env)
  ret
}

#' @describeIn is.sorted.integer64 returns the number of valid data points,
#'   usually [length()] minus `na.count`.
#' @export
nvalid.integer64 <- function(x, ...){
    length(x) - na.count(x)
}

#' @describeIn is.sorted.integer64 checks for sortedness of `x` (NAs sorted first)
#' @export
is.sorted.integer64 <- function(x, ...){
  env <- cache(x)
  if (is.null(env))
    return(.Call(C_r_ram_integer64_issorted_asc, x = x, PACKAGE = "bit64"))
  if (exists("is.sorted", envir=env, inherits=FALSE))
    return(get("is.sorted", envir=env, inherits=FALSE))
  ret <- .Call(C_r_ram_integer64_issorted_asc, x = x, PACKAGE = "bit64")
  assign("is.sorted", ret, envir=env)
  ret
}

#' @describeIn is.sorted.integer64 returns the number of unique values
#' @export
nunique.integer64 <- function(x, ...){
    env <- cache(x)
    if(is.null(env))
        has.cache <- FALSE
    else if (exists("nunique", envir=env, inherits=FALSE))
        return(get("nunique", envir=env, inherits=FALSE))
    else
        has.cache <- TRUE
    if (is.sorted(x)){
        ret <- .Call(C_r_ram_integer64_sortnut
        , x = x
        , PACKAGE = "bit64"
        )
        if (has.cache){
            assign("nunique", ret[1L], envir=env)
            assign("nties", ret[2L], envir=env)
        }
        ret[1L]
    }else{
        h <- hashmap(x)
        if (has.cache)
          assign("nunique", h$nunique, envir=env)
        h$nunique
    }
}

#' @describeIn is.sorted.integer64 returns the number of tied values.
#' @export
nties.integer64 <- function(x, ...){
    cv <- getcache(x, "nties")
    if (is.null(cv)){
        if (is.sorted(x)){
            cv <- .Call(C_r_ram_integer64_sortnut
            , x = x
            , PACKAGE = "bit64"
            )[2L]
        }else{
            s <- clone(x)
            # nolint next: object_usage_linter. Keep the output of in-place ramsort for debugging.
            na.count <-
              ramsort(s, has.na = TRUE, na.last = FALSE, decreasing = FALSE, stable = FALSE, optimize = "time")
            cv <- .Call(C_r_ram_integer64_sortnut, x = s, PACKAGE = "bit64")[[2L]]
        }
    }
    cv
}
