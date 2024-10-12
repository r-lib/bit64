# /*
# R-Code for patching S3 generics
# S3 atomic 64bit integers for R
# (c) 2011 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2011-12-11
# Last changed:  2011-12-11
# */

#' Turning base R functions into S3 generics for bit64
#'
#' Turn those base functions S3 generic which are used in bit64
#'
#' @aliases bit64S3 : :.default :.integer64 is.double is.double.default
#'   is.double.integer64 match match.default %in% %in%.default rank
#'   rank.default order order.default
# @aliases table table.default
#' @usage
#'    from:to
#'  #--as-cran complains about \method{:}{default}(from, to)
#'  #--as-cran complains about \method{:}{integer64}(from, to)
#'    is.double(x)
#'  \method{is.double}{default}(x)
#'  \method{is.double}{integer64}(x)
#'     match(x, table, ...)
#'  \method{match}{default}(x, table, ...)
#'     x \%in\% table
#'  \method{\%in\%}{default}(x, table)
#'     rank(x, ...)
#'  \method{rank}{default}(x, ...)
#'     %table(...)
#'  %\method{table}{default}(...)
#'     order(...)
#'  \method{order}{default}(...)
#'
#' @param x integer64 vector: the values to be matched, optionally carrying a
#'   cache created with [hashcache()]
#' @param table integer64 vector: the values to be matched against, optionally
#'   carrying a cache created with [hashcache()] or [sortordercache()]
#' @param from scalar denoting first element of sequence
#' @param to scalar denoting last element of sequence
#' @param ... ignored
#'
#' @details
#' The following functions are turned into S3 generics in order to dispatch
#'   methods for [integer64()]:
#'
#'  - [`:`]
#'  - [is.double()]
#'  - [match()]
#'  - [`%in%`]
#   - [table()]
#'  - [rank()]
#'  - [order()]
#'
#' @return [invisible()]
#' @note
#'  - [is.double()] returns `FALSE` for \code{\link{integer64}}
#'  - [`:`] currently only dispatches at its first argument, thus
#'    `as.integer64(1):9` works but `1:as.integer64(9)` doesn't
#'  - [match()] currently only dispatches at its first argument and expects
#'    its second argument also to be integer64, otherwise throws an error.
#'    Beware of something like `match(2, as.integer64(0:3))`
#'  - [`%in%`] currently only dispatches at its first argument and expects
#'    its second argument also to be integer64, otherwise throws an error.
#'    Beware of something like `2 \%in\% as.integer64(0:3)`
#'  - [order()] currently only orders a single argument, trying more than
#'    one raises an error
#'
#' @seealso [bit64()], [S3]
#'
#' @examples
#'  is.double(as.integer64(1))
#'     as.integer64(1):9
#'  match(as.integer64(2), as.integer64(0:3))
#'  as.integer64(2) \%in\% as.integer64(0:3)
#'
#'  unique(as.integer64(c(1,1,2)))
#'  rank(as.integer64(c(1,1,2)))
#'
#'  %table(as.integer64(c(1,1,2)))
#'  %table(as.integer64(c(1,1,2)),as.integer64(c(3,4,4)))
#'  %table(as.integer64(c(1,1,2)),c(3,4,4))
#'  %table(c(1,1,2),as.integer64(c(3,4,4)))
#'
#'  order(as.integer64(c(1,NA,2)))
#'
#'  \dontshow{
#'  stopifnot(identical(match(as.integer64(2), as.integer64(0:3)), match(2, 0:3)))
#'  stopifnot(identical(as.integer64(2) \%in\% as.integer64(0:3), 2 \%in\% 0:3))
#'
#'  stopifnot(identical(unique(as.integer64(c(1,1,2))), as.integer64(unique(c(1,1,2)))))
#'  stopifnot(identical(rank(as.integer64(c(1,1,2))), rank(c(1,1,2))))
#'
#'  %stopifnot(identical(table(as.integer64(c(1,1,2))), table(c(1,1,2))))
#'  %stopifnot(identical(table(as.integer64(c(1,1,2)),as.integer64(c(3,4,4))), table(c(1,1,2),c(3,4,4))))
#'  %stopifnot(identical(table(as.integer64(c(1,1,2)),c(3,4,4)), table(c(1,1,2),c(3,4,4))))
#'  %stopifnot(identical(table(c(1,1,2),as.integer64(c(3,4,4))), table(c(1,1,2),c(3,4,4))))
#'
#'  stopifnot(identical(order(as.integer64(c(1,NA,2))), order(c(1,NA,2))))
#'  stopifnot(identical(order(as.integer64(c(1,NA,2)), decreasing=TRUE), order(c(1,NA,2), decreasing=TRUE)))
#'  }
#'
#' @keywords methods
#' @name bit64S3
NULL

# OCT 2013: bit64S3() at wish of CRAN maintainers replaced by direct conversion to S3 generics
# in order to avoid assigning to globalenv

if (!exists(":.default")){
    `:` <- function(from,to) UseMethod(":")
    `:.default` <- function(from,to) base::`:`(from,to)
}
`:.integer64` <- function(from, to)seq.integer64(from=from, to=to)

if (!exists("is.double.default")){
    is.double <- function(x) UseMethod("is.double")
    is.double.default <- function(x) base::is.double(x)
}
is.double.integer64 <- function(x)FALSE

if (!exists("match.default")){
    match <- function(x, table, ...) UseMethod("match")
    match.default <- function(x, table, ...) base::match(x, table, ...)
}

if (!exists("%in%.default")){
    `%in%` <- function(x, table) UseMethod("%in%")
    `%in%.default` <- function(x, table) base::`%in%`(x, table)
}

if (!exists("rank.default")){
    rank <- function(x, ...) UseMethod("rank")
    rank.default <- function(x, ...) base::rank(x, ...)
}

# not yet able to combinewith other column types - better leave table() as is and hope for as.factor.integer64
#if (!exists("table.default")){
#    "table" <- function(...) UseMethod("table")
#    "table.default" <- function(...) base::"table"(...)
#}

if (!exists("order.default")){
    order <- function(...) UseMethod("order")
    order.default <- function(...) base::order(...)
}
