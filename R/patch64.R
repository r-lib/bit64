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
#' @usage
#' from:to
#' is.double(x)
#' match(x, table, ...)
#' x \%in\% table
#' rank(x, ...)
#' order(...)
#' @aliases bit64S3 : :.default :.integer64 is.double is.double.default
#'   is.double.integer64 match match.default %in% %in%.default rank
#'   rank.default order order.default
#  @aliases table table.default
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
#'  - \code{\link[=match]{\%in\%}}
#   - [table()]
#'  - [rank()]
#'  - [order()]
#'
#' @return [invisible()]
#' @note
#'  - [is.double()] returns `FALSE` for [`integer64`]
#'  - [`:`] currently only dispatches at its first argument, thus
#'    `as.integer64(1):9` works but `1:as.integer64(9)` doesn't
#'  - [match()] currently only dispatches at its first argument and expects
#'    its second argument also to be integer64, otherwise throws an error.
#'    Beware of something like `match(2, as.integer64(0:3))`
#'  - \code{\link[=match]{\%in\%}} currently only dispatches at its first argument and expects
#'    its second argument also to be integer64, otherwise throws an error.
#'    Beware of something like `2 %in% as.integer64(0:3)`
#'  - [order()] currently only orders a single argument, trying more than
#'    one raises an error
#'
#' @seealso [bit64()], [S3]
#'
#' @examples
#' is.double(as.integer64(1))
#' as.integer64(1):9
#' match(as.integer64(2), as.integer64(0:3))
#' as.integer64(2) %in% as.integer64(0:3)
#'
#' unique(as.integer64(c(1,1,2)))
#' rank(as.integer64(c(1,1,2)))
#'
#   %table(as.integer64(c(1,1,2)))
#   %table(as.integer64(c(1,1,2)),as.integer64(c(3,4,4)))
#   %table(as.integer64(c(1,1,2)),c(3,4,4))
#   %table(c(1,1,2),as.integer64(c(3,4,4)))
#'
#' order(as.integer64(c(1,NA,2)))
#' @keywords methods
#' @name bit64S3
NULL

# OCT 2013: bit64S3() at wish of CRAN maintainers replaced by direct conversion to S3 generics
# in order to avoid assigning to globalenv

`:` <- function(from, to) UseMethod(":")
#' @export
`:.default` <- function(from, to) base::`:`(from, to)

#' @export
`:.integer64` <- function(from, to) seq.integer64(from=from, to=to)

is.double <- function(x) UseMethod("is.double")
#' @rdname bit64S3
#' @export
is.double.default <- function(x) base::is.double(x)

#' @rdname bit64S3
#' @export
is.double.integer64 <- function(x) FALSE

# TODO(R>=4.2.0): Remove workarounds for match(). Needed for #85 and #111.
#' @rdname bit64S3
#' @rawNamespace if (getRversion() >= "4.2.0") S3method(mtfrm,integer64)
mtfrm.integer64 = function(x) as.character(x)

match <- function(x, table, ...) UseMethod("match")
#' @rdname bit64S3
#' @export
match.default <- function(x, table, ...) {
  if (!exists("mtfrm", baseenv()) && is.integer64(table)) base::match(as.character(x), as.character(table), ...) # nocov
  else base::match(x, table, ...)
}

`%in%` <- function(x, table) UseMethod("%in%")
#' @rdname bit64S3
#' @export
`%in%.default` <- function(x, table) base::`%in%`(x, table)

rank <- function(x, ...) UseMethod("rank")
#' @rdname bit64S3
#' @export
rank.default <- function(x, ...) base::rank(x, ...)

# not yet able to combinewith other column types - better leave table() as is and hope for as.factor.integer64
#if (!exists("table.default")) {
#    "table" <- function(...) UseMethod("table")
#    "table.default" <- function(...) base::"table"(...)
#}

order <- function(...) UseMethod("order")
#' @rdname bit64S3
#' @export
order.default <- function(...) base::order(...)
