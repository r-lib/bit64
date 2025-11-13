# /*
# R-Code
# S3 atomic 64bit integers for R
# (c) 2011-2024 Jens Oehlschägel
# (c) 2025 Michael Chirico
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2011-12-11
#*/

#' Identity function for class 'integer64'
#'
#' This will discover any deviation between objects containing integer64 vectors.
#'
#' This is simply a wrapper to [identical()] with default arguments
#'   `num.eq = FALSE, single.NA = FALSE`.
#'
#' @param x,y Atomic vector of class 'integer64'
#' @param num.eq,single.NA,attrib.as.set,ignore.bytecode,ignore.environment,ignore.srcref
#'   See [identical()].
#' @param ... Passed on to `identical()`. Only `extptr.as.ref=` is available as of R 4.4.1,
#'   and then only for versions of R >= 4.2.0.
#'
#' @return A single logical value, `TRUE` or `FALSE`, never `NA` and never
#'   anything other than a single value.
#' @keywords classes manip
#' @seealso [`==.integer64`] [identical()] [integer64()]
#' @examples
#'   i64 <- as.double(NA); class(i64) <- "integer64"
#'   identical(i64-1, i64+1)
#'   identical.integer64(i64-1, i64+1)
#' @name identical.integer64
NULL

#' Coerce from integer64
#'
#' Methods to coerce integer64 to other atomic types. 'as.bitstring' coerces
#'   to a human-readable bit representation (strings of zeroes and ones).
#'   The methods [format()], [as.character()], [as.double()],
#'   [as.logical()], [as.integer()] do what you would expect.
#'
#' @param x an integer64 vector
#' @param keep.names FALSE, set to TRUE to keep a names vector
#' @param ... further arguments to the [NextMethod()]
#'
#' @return `as.bitstring` returns a string of class 'bitstring'.
#'
#' The other methods return atomic vectors of the expected types
#'
#' @keywords classes manip
#' @seealso [as.integer64.character()] [integer64()]
#' @examples
#'   as.character(lim.integer64())
#'   as.bitstring(lim.integer64())
#'   as.bitstring(as.integer64(c(-2, -1, NA, 0:2)))
#' @name as.character.integer64
NULL

#' Coerce to integer64
#'
#' Methods to coerce from other atomic types to integer64.
#'
#' @param x an atomic vector
#' @param keep.names FALSE, set to TRUE to keep a names vector
#' @param ... further arguments to the [NextMethod()]
#'
#' @details
#' `as.integer64.character` is realized using C function `strtoll` which
#'   does not support scientific notation. Instead of '1e6' use '1000000'.
#'   `as.integer64.bitstring` evaluates characters '0' and ' ' as zero-bit,
#'   all other one byte characters as one-bit, multi-byte characters are not allowed,
#'   strings shorter than 64 characters are treated as if they were left-padded with '0',
#'   strings longer than 64 bytes are mapped to `NA_INTEGER64` and a warning is emitted.
#'
#' @return The other methods return atomic vectors of the expected types
#'
#' @keywords classes manip
#' @seealso [as.character.integer64()] [integer64()]
#' @examples
#' as.integer64(as.character(lim.integer64()))
#' as.integer64(
#'   structure(c("1111111111111111111111111111111111111111111111111111111111111110",
#'               "1111111111111111111111111111111111111111111111111111111111111111",
#'               "1000000000000000000000000000000000000000000000000000000000000000",
#'               "0000000000000000000000000000000000000000000000000000000000000000",
#'               "0000000000000000000000000000000000000000000000000000000000000001",
#'               "0000000000000000000000000000000000000000000000000000000000000010"
#'   ), class = "bitstring")
#' )
#' as.integer64(
#'  structure(c("............................................................... ",
#'              "................................................................",
#'              ".                                                               ",
#'              "",
#'              ".",
#'              "10"
#'   ), class = "bitstring")
#' )
#' @name as.integer64.character
NULL

#' Extract or Replace Parts of an integer64 vector
#'
#' Methods to extract and replace parts of an integer64 vector.
#'
#' @param x an atomic vector
#' @param i indices specifying elements to extract
#' @param value an atomic vector with values to be assigned
#' @param ... further arguments to the [NextMethod()]
#'
#' @note
#'   You should not subscript non-existing elements and not use `NA`s as subscripts.
#'   The current implementation returns `9218868437227407266` instead of `NA`.
#' @returns A vector or scalar of class 'integer64'
#' @keywords classes manip
#' @seealso [`[`][base::Extract] [integer64()]
#' @examples
#'   as.integer64(1:12)[1:3]
#'   x <- as.integer64(1:12)
#'   dim(x) <- c(3, 4)
#'   x
#'   x[]
#'   x[, 2:3]
#' @name extract.replace.integer64
NULL

#' Unary operators and functions for integer64 vectors
#'
#' Unary operators and functions for integer64 vectors.
#'
#' @param x an atomic vector of class 'integer64'
#' @param base an atomic scalar (we save 50% log-calls by not allowing
#'   a vector base)
#' @param digits integer indicating the number of decimal places (round)
#'   or significant digits (signif) to be used. Negative values are allowed
#'   (see [round()])
#' @param justify should it be right-justified (the default), left-justified,
#'   centred or left alone.
#' @param center see [scale()]
#' @param scale see [scale()]
#' @param ... further arguments to the [NextMethod()]
#'
#' @returns
#'   [format()] returns a character vector
#'
#'   [is.na()] and [`!`] return a logical vector
#'
#'   [sqrt()], [log()], [log2()] and [log10()] return a double vector
#'
#'   [sign()], [abs()], [floor()], [ceiling()], [trunc()] and
#'   [round()] return a vector of class 'integer64'
#'
#'   [signif()] is not implemented
#'
#' @keywords classes manip
#' @seealso [xor.integer64()] [integer64()]
#' @examples
#'   sqrt(as.integer64(1:12))
#' @name format.integer64
NULL

#' Binary operators for integer64 vectors
#'
#' Binary operators for integer64 vectors.
#'
#' @param e1 an atomic vector of class 'integer64'
#' @param e2 an atomic vector of class 'integer64'
#' @param x an atomic vector of class 'integer64'
#' @param y an atomic vector of class 'integer64'
#'
#' @returns
#'   [`&`], [`|`], [xor()], [`!=`], [`==`],
#'   [`<`], [`<=`], [`>`], [`>=`] return a logical vector
#'
#'   [`^`] and [`/`] return a double vector
#'
#'   [`+`], [`-`], [`*`], [`%/%`], [`%%`]
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
#' @name xor.integer64
NULL

#' Summary functions for integer64 vectors
#'
#' Summary functions for integer64 vectors. Function 'range' without arguments
#'   returns the smallest and largest value of the 'integer64' class.
#'
#' @param ... atomic vectors of class 'integer64'
#' @param na.rm logical scalar indicating whether to ignore NAs
#' @param finite logical scalar indicating whether to ignore NAs (just for
#'   compatibility with [range.default()])
#'
#' @details
#' The numerical summary methods always return `integer64`. Wherever integer methods would
#'   return `Inf` (or its negation), here the extreme 64-bit integer `9223372036854775807` is
#'   returned. See [min()] for more details about the behavior.
#'
#' `lim.integer64` returns these limits in proper order
#'   `-9223372036854775807, +9223372036854775807` and without a [warning()].
#'
#' @returns
#'   [all()] and [any()] return a logical scalar
#'
#'   [range()] returns a integer64 vector with two elements
#'
#'   [min()], [max()], [sum()] and [prod()] return a integer64 scalar
#'
#' @keywords classes manip
#' @seealso [mean.integer64()] [cumsum.integer64()] [integer64()]
#' @examples
#'   lim.integer64()
#'   range(as.integer64(1:12))
#' @name sum.integer64
NULL

#' Cumulative Sums, Products, Extremes and lagged differences
#'
#' Cumulative Sums, Products, Extremes and lagged differences
#'
#' @param x an atomic vector of class 'integer64'
#' @param lag see [diff()]
#' @param differences see [diff()]
#' @param ... ignored
#'
#' @returns
#'   [cummin()], [cummax()] , [cumsum()] and [cumprod()]
#'      return a integer64 vector of the same length as their input
#'
#'   [diff()] returns a integer64 vector shorter by `lag*differences` elements
#'
#' @keywords classes manip
#' @seealso [sum.integer64()] [integer64()]
#' @examples
#'   cumsum(rep(as.integer64(1), 12))
#'   diff(as.integer64(c(0, 1:12)))
#'   cumsum(as.integer64(c(0, 1:12)))
#'   diff(cumsum(as.integer64(c(0, 0, 1:12))), differences=2)
#' @name cumsum.integer64
NULL

#' Concatenating integer64 vectors
#'
#' The ususal functions 'c', 'cbind' and 'rbind'
#'
#' @param ... two or more arguments coerced to 'integer64' and
#'   passed to [NextMethod()]
#' @param recursive logical. If `recursive = TRUE`, the function
#'   recursively descends through lists (and pairlists) combining all
#'   their elements into a vector.
#'
#' @returns
#'   [c()] returns a integer64 vector of the total length of the input
#'
#'   [cbind()] and [rbind()] return a integer64 matrix
#'
#' @note
#' R currently only dispatches generic 'c' to method 'c.integer64' if the
#'   first argument is 'integer64'
#'
#' @keywords classes manip
#' @seealso [rep.integer64()] [seq.integer64()] [as.data.frame.integer64()]
#'   [integer64()]
#'
#' @examples
#'   c(as.integer64(1), 2:6)
#'   cbind(1:6, as.integer64(1:6))
#'   rbind(1:6, as.integer64(1:6))
#' @name c.integer64
NULL

#' Replicate elements of integer64 vectors
#'
#' Replicate elements of integer64 vectors
#'
#' @param x a vector of 'integer64' to be replicated
#' @param ... further arguments passed to [NextMethod()]
#'
#' @returns [rep()] returns a integer64 vector
#' @keywords classes manip
#' @seealso [c.integer64()] [rep.integer64()]
#'   [as.data.frame.integer64()] [integer64()]
#'
#' @examples
#'   rep(as.integer64(1:2), 6)
#'   rep(as.integer64(1:2), c(6, 6))
#'   rep(as.integer64(1:2), length.out=6)
#' @name rep.integer64
NULL

#' integer64: Sequence Generation
#'
#' Generating sequence of integer64 values
#'
#' @param from integer64 scalar (in order to dispatch the integer64 method of [seq()])
#' @param to scalar
#' @param by scalar
#' @param length.out scalar
#' @param along.with scalar
#' @param ... ignored
#' @details
#'   `seq.integer64` does coerce its arguments 'from', 'to' and 'by' to `integer64`.
#'   If not provided, the argument 'by' is automatically determined as `+1` or `-1`,
#'   but the size of 'by' is not calculated as in [seq()] (because this might result
#'   in a non-integer value).
#'
#' @returns an integer64 vector with the generated sequence
#' @note
#'   In base R [`:`] currently is not generic and does not dispatch, see section
#'   "Limitations inherited from Base R" in [integer64()]
#'
#' @keywords classes manip
#' @seealso [c.integer64()] [rep.integer64()]
#'   [as.data.frame.integer64()] [integer64()]
#' @examples
#'   # colon not activated: as.integer64(1):12
#'   seq(as.integer64(1), 12, 2)
#'   seq(as.integer64(1), by=2, length.out=6)
#' @name seq.integer64
NULL

#' integer64: Coercing to data.frame column
#'
#' Coercing integer64 vector to data.frame.
#'
#' @param x an integer64 vector
#' @param ... passed to NextMethod [as.data.frame()] after removing the
#'   'integer64' class attribute
#'
#' @returns a one-column data.frame containing an integer64 vector
#' @details
#'   'as.data.frame.integer64' is rather not intended to be called directly,
#'   but it is required to allow integer64 as data.frame columns.
#' @note This is currently very slow -- any ideas for improvement?
#' @keywords classes manip
#' @seealso
#'   [cbind.integer64()] [integer64()]
#    as.vector.integer64 removed as requested by the CRAN maintainer [as.vector.integer64()]
#' @examples
#'   as.data.frame.integer64(as.integer64(1:12))
#'   data.frame(a=1:12, b=as.integer64(1:12))
#' @name as.data.frame.integer64
NULL

#' integer64: Maintaining S3 class attribute
#'
#' Maintaining integer64 S3 class attribute.
#'
#' @param class NULL or a character vector of class attributes
#' @param whichclass the (single) class name to add or remove from the class vector
#'
#' @returns NULL or a character vector of class attributes
#'
#' @keywords classes manip internal
#' @seealso [oldClass()] [integer64()]
#' @examples
#'   plusclass("inheritingclass", "integer64")
#'   minusclass(c("inheritingclass", "integer64"), "integer64")
#' @name plusclass
NULL

#' Test if two integer64 vectors are all.equal
#'
#' A utility to compare integer64 objects 'x' and 'y' testing for
#'   ‘near equality’, see [all.equal()].
#'
#' @param target a vector of 'integer64' or an object that can be coerced
#'   with [as.integer64()]
#' @param current a vector of 'integer64' or an object that can be coerced
#'   with [as.integer64()]
#' @param tolerance numeric > 0.  Differences smaller than `tolerance` are
#'   not reported.  The default value is close to `1.5e-8`.
#' @param scale `NULL` or numeric > 0, typically of length 1 or
#'   `length(target)`. See Details.
#' @param countEQ logical indicating if the `target == current` cases should
#'   be counted when computing the mean (absolute or relative) differences.
#'   The default, `FALSE` may seem misleading in cases where `target` and
#'   `current` only differ in a few places; see the extensive example.
#' @param formatFUN a [function()] of two arguments, `err`, the relative,
#'   absolute or scaled error, and `what`, a character string indicating the
#'   _kind_ of error; maybe used, e.g., to format relative and absolute errors
#'   differently.
#' @param ... further arguments are ignored
#' @param check.attributes logical indicating if the [attributes()] of `target`
#'   and `current` (other than the names) should be compared.
#'
#' @returns
#'   Either ‘TRUE’ (‘NULL’ for ‘attr.all.equal’) or a vector of ‘mode’
#'   ‘"character"’ describing the differences between ‘target’ and
#'   ‘current’.
#'
#' @details
#'  In [all.equal.numeric()] the type `integer` is treated as a proper subset
#'   of `double` i.e. does not complain about comparing `integer` with `double`.
#'   Following this logic `all.equal.integer64` treats `integer` as a proper
#'   subset of `integer64` and does not complain about comparing `integer` with
#'   `integer64`. `double` also compares without warning as long as the values
#'   are within [lim.integer64()], if `double` are bigger `all.equal.integer64`
#'   complains about the `all.equal.integer64 overflow warning`. For further
#'   details see [all.equal()].
#'
#' @note
#'    [all.equal()] only dispatches to this method if the first argument is `integer64`,
#'    calling [all.equal()] with a `non-integer64` first and a `integer64` second argument
#'    gives undefined behavior!
#'
#' @seealso [all.equal()]
#' @examples
#'   all.equal(as.integer64(1:10), as.integer64(0:9))
#'   all.equal(as.integer64(1:10), as.integer(1:10))
#'   all.equal(as.integer64(1:10), as.double(1:10))
#'   all.equal(as.integer64(1), as.double(1e300))
#' @name all.equal.integer64
NULL

methods::setOldClass("integer64")

# contributed by Leonardo Silvestri with modifications of JO
#' @rdname all.equal.integer64
#' @method all.equal integer64
#' @exportS3Method all.equal integer64
all.equal.integer64  <- function(target, current,
                                 tolerance = sqrt(.Machine$double.eps),
                                 scale = NULL,
                                 countEQ = FALSE,
                                 formatFUN = function(err, what) format(err),
                                 ...,
                                 check.attributes = TRUE) {
  if (!is.numeric(tolerance))
    stop("'tolerance' should be numeric")
  if (!is.numeric(scale) && !is.null(scale))
    stop("'scale' should be numeric or NULL")
  if (!is.logical(check.attributes))
    stop(gettextf("'%s' must be logical", "check.attributes"),
         domain = NA)
  # JO: BEGIN respect that integer is a proper subset of integer64 like integer is a proper subset of double
  oldwarn <- getOption("warn")
  on.exit(options(warn=oldwarn))
  options(warn=2L)
  if (!is.integer64(target)) {
    cl <- oldClass(target)
    oldClass(target) <- NULL
    target <- try(as.integer64(target))
    if (inherits(target, 'try-error'))
      return(paste("while coercing 'target' to 'integer64':",  attr(target, "condition")$message))
    oldClass(target) <- c(cl, "integer64")
  }
  if (!is.integer64(current)) {
    cl <- oldClass(current)
    oldClass(current) <- NULL
    current <- try(as.integer64(current))
    if (inherits(current, 'try-error'))
      return(paste("while coercing 'current' to 'integer64':",  attr(current, "condition")$message))
    oldClass(current) <- c(cl, "integer64")
  }
  # JO: END respect that integer is a proper subset of integer64 like integer is a proper subset of double
  msg <- NULL
  msg <- if (check.attributes)
           attr.all.equal(target, current, tolerance = tolerance,
                          scale = scale, ...)
  if (data.class(target) != data.class(current)) {
    msg <- c(msg, paste0("target is ", data.class(target),
                         ", current is ", data.class(current)))
    return(msg)
  }
  lt <- length(target)
  lc <- length(current)
  if (lt != lc) {
    if (!is.null(msg))
      msg <- msg[-grep("\\bLengths\\b", msg)]
    msg <- c(msg, paste0("integer64: lengths (", lt, ", ", lc, ") differ"))
    return(msg)
  }
  out <- is.na(target)
  if (any(out != is.na(current))) {
    msg <- c(msg, paste("'is.NA' value mismatch:", sum(is.na(current)),
                        "in current", sum(out), "in target"))
    return(msg)
  }
  out <- out | target == current
  if (all(out))
    # TODO(R>=4.4.0): msg %||% TRUE
    return(if (is.null(msg)) TRUE else msg)
  anyO <- any(out)
  sabst0 <- if (countEQ && anyO) mean(abs(target[out])) else 0.0
  if (anyO) {
    keep <- which(!out)
    target  <- target [keep]
    current <- current[keep]
    if (!is.null(scale) && length(scale) > 1L) {
      # TODO(R>=4.0.0): Try removing this ocl part when rep() dispatching WAI on all versions (#100)
      ocl = class(scale)
      scale = rep_len(scale, length(out))[keep]
      class(scale) = ocl
    }
  }
  N <- length(target)
  what <- if (is.null(scale)) {
    scale <- sabst0 + sum(abs(target)/N)
    if (is.finite(scale) && scale > tolerance) {
      "relative"
    } else {
      scale <- 1.0
      "absolute"
    }
  } else {
    stopifnot(scale > 0.0)
    if (all(abs(scale - 1.0) < 1e-07))
      "absolute"
    else
      "scaled"
  }
  xy <- sum(abs(target - current) / (N*scale))
  if (is.na(xy) || xy > tolerance)
    msg <- c(msg, paste("Mean", what, "difference:", formatFUN(xy, what)))
  # TODO(R>=4.4.0): msg %||% TRUE
  if (is.null(msg)) TRUE else msg
}

# TODO(R>=4.2.0): Consider restoring extptr.as.ref= to the signature.
#' @rdname identical.integer64
#' @exportS3Method identical integer64
#' @export
identical.integer64 <- function(x, y,
                                num.eq=FALSE,
                                single.NA=FALSE,
                                attrib.as.set=TRUE,
                                ignore.bytecode=TRUE,
                                ignore.environment=FALSE,
                                ignore.srcref=TRUE,
                                ...) {
  identical(x=x, y=y,
    num.eq=num.eq,
    single.NA=single.NA,
    attrib.as.set=attrib.as.set,
    ignore.bytecode=ignore.bytecode,
    ignore.environment=ignore.environment,
    ignore.srcref=ignore.srcref,
    ...
  )
}


#' @rdname as.integer64.character
#' @export
as.integer64 <- function(x, ...) UseMethod("as.integer64")

#' @rdname as.character.integer64
#' @export
as.bitstring <- function(x, ...) UseMethod("as.bitstring")

#' @rdname plusclass
#' @export
minusclass <- function(class, whichclass) {
  if (!length(class)) return(class)
  i <- whichclass == class
  if (any(i))
    class[!i]
  else
    class
}

#' @export
plusclass <- function(class, whichclass) {
  if (!length(class)) return(whichclass)
  c(class, if (!any(whichclass == class)) whichclass)
}

# Version of Leonardo Silvestri
#' @rdname xor.integer64
#' @export
binattr <- function(e1, e2) {
  d1 <- dim(e1)
  d2 <- dim(e2)
  n1 <- length(e1)
  n2 <- length(e2)

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
      ae1 <- attributes(e1)[c("class", "dim", "dimnames")]
      ae2 <- attributes(e2)[c("class", "dim", "dimnames")]
    }
    ae1 <- attributes(e1)
    ae2 <- attributes(e2)
    nae1 <- names(attributes(e1))
    nae2 <- names(attributes(e2))
    if (n1==0L) {
      ae1 <- ae1[nae1 %in% c("class", "dim", "dimnames")]
      ae2 <- ae1[nae1 %in% c("class", "dim", "dimnames")]
    }
    allattr <- list()
    for (a in union(nae1, nae2))
      if (a %in% nae1)
        allattr[[a]] <- ae1[[a]]
    else
      allattr[[a]] <- ae2[[a]]
    allattr
  } else if (n1 == 0L || n1 > n2) {
    attributes(e1)
  } else {
    attributes(e2)
  }
}

#' @rdname bit64-package
#' @param length length of vector using [integer()]
#' @return `integer64` returns a vector of 'integer64', i.e.,
#'   a vector of [double()] decorated with class 'integer64'.
#' @export
integer64 <- function(length=0L) {
  ret <- double(length)
  oldClass(ret) <- "integer64"
  ret
}

#' @rdname bit64-package
#' @param x an integer64 vector
#' @export
is.integer64 <- function(x) inherits(x, "integer64")

#' @rdname as.integer64.character
#' @export
as.integer64.NULL <- function(x, ...) {
  ret <- double()
  oldClass(ret) <- "integer64"
  ret
}

#' @rdname as.integer64.character
#' @export
as.integer64.integer64 <- function(x, ...) x

#' @rdname as.integer64.character
#' @export
as.integer64.double <- function(x, keep.names=FALSE, ...) {
  ret <- .Call(C_as_integer64_double, x, double(length(x)))
  if (keep.names)
    names(ret) <- names(x)
  oldClass(ret) <- "integer64"
  ret
}

#' @rdname as.integer64.character
#' @export
as.integer64.integer <- function(x, ...) {
  ret <- .Call(C_as_integer64_integer, x, double(length(x)))
  oldClass(ret) <- "integer64"
  ret
}

#' @rdname as.integer64.character
#' @export
as.integer64.logical <- as.integer64.integer

#' @rdname as.integer64.character
#' @export
as.integer64.character <- function(x, ...) {
  n <- length(x)
  ret <- .Call(C_as_integer64_character, x, rep(NA_real_, n))
  oldClass(ret) <- "integer64"
  ret
}

#' @rdname as.integer64.character
#' @export
as.integer64.factor <- function(x, ...) as.integer64(unclass(x), ...)

#' @rdname as.character.integer64
#' @export
as.double.integer64 <- function(x, keep.names=FALSE, ...) {
  ret <- .Call(C_as_double_integer64, x, double(length(x)))
  if (keep.names)
    names(ret) <- names(x)
  ret
}

#' @rdname as.character.integer64
#' @export
as.integer.integer64 <- function(x, ...) {
  .Call(C_as_integer_integer64, x, integer(length(x)))
}

#' @rdname as.character.integer64
#' @export
as.logical.integer64 <- function(x, ...) {
  .Call(C_as_logical_integer64, x, logical(length(x)))
}

#' @rdname as.character.integer64
#' @export
as.character.integer64 <- function(x, ...) {
  n <- length(x)
  .Call(C_as_character_integer64, x, rep(NA_character_, n))
}

#' @rdname as.character.integer64
#' @export
as.bitstring.integer64 <- function(x, ...) {
  n <- length(x)
  ret <- .Call(C_as_bitstring_integer64, x, rep(NA_character_, n))
  oldClass(ret) <- 'bitstring'
  ret
}

#' @rdname as.character.integer64
#' @export
print.bitstring <- function(x, ...) {
  oldClass(x) <- minusclass(class(x), 'bitstring')
  NextMethod(x)
}

#' @rdname as.integer64.character
#' @export
as.integer64.bitstring <- function(x, ...) {
  ret <- .Call(C_as_integer64_bitstring, x, double(length(x)))
  oldClass(ret) <- "integer64"
  ret
}


# read.table expects S4 as()
methods::setAs("character", "integer64", function(from) as.integer64.character(from))
methods::setAs("integer64", "character", function(from) as.character.integer64(from))

# this is a trick to generate NA_integer64_ for namespace export before
# as.integer64() is available because dll is not loaded
#' @rdname as.integer64.character
#' @export
NA_integer64_ <- unserialize(as.raw(c(
  0x58, 0x0a, 0x00, 0x00, 0x00, 0x02, 0x00, 0x03, 0x03, 0x00, 0x00, 0x02, 0x03, 0x00, 0x00, 0x00,
  0x03, 0x0e, 0x00, 0x00, 0x00, 0x01, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x05, 0x63, 0x6c,
  0x61, 0x73, 0x73, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00,
  0x00, 0x00, 0x09, 0x69, 0x6e, 0x74, 0x65, 0x67, 0x65, 0x72, 0x36, 0x34, 0x00, 0x00, 0x00, 0xfe
)))

#' @rdname bit64-package
#' @param value an integer64 vector of values to be assigned
#' @export
`length<-.integer64` <- function(x, value) {
  cl <- oldClass(x)
  n <- length(x)
  x <- NextMethod()
  oldClass(x) <- cl
  if (value>n)
    x[(n+1L):value] <- 0L
  x
}

#' @rdname format.integer64
#' @export
format.integer64 <- function(x, justify="right", ...) {
  a <- attributes(x)
  x <- as.character(x)
  ret <- format(x, justify=justify, ...)
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

#' @rdname bit64-package
#' @param quote logical, indicating whether or not strings should be printed with surrounding quotes.
#' @param ... further arguments to the [NextMethod()]
#' @export
print.integer64 <- function(x, quote=FALSE, ...) {
  a <- attributes(x)
  if (length(x)) {
    cat("integer64\n")
    ret <- as.character(x)
    a$class <- minusclass(a$class, "integer64")
    attributes(ret) <- a
    print(ret, quote=quote, ...)
  } else {
    cat("integer64(0)\n")
  }
  invisible(x)
}

#' @rdname bit64-package
#' @param object an integer64 vector
#' @param vec.len,give.head,give.length see [utils::str()]
#' @export
str.integer64 <- function(object,
                          vec.len  = strO$vec.len,
                          give.head = TRUE,
                          give.length = give.head,
                          ...) {
  strO <- strOptions()
  vec.len <- 2L*vec.len
  n <- length(object)
  if (n > vec.len)
    object <- object[seq_len(vec.len)]
  cat(
    if (give.head) paste0("integer64 ", if (give.length && n>1L) paste0("[1:", n, "] ")),
    paste(as.character(object), collapse=" "),
    if (n > vec.len) " ...",
    " \n",
    sep=""
  )
  invisible()
}

#' @rdname extract.replace.integer64
#' @export
`[.integer64` <- function(x, i, ...) {
    cl <- oldClass(x)
    ret <- NextMethod()
    # Begin NA-handling from Leonardo Silvestri
    if (!missing(i)) {
        if (inherits(i, "character")) {
          na_idx <- union(which(!(i %in% names(x))), which(is.na(i)))
          if (length(na_idx))
                ret[na_idx] <- NA_integer64_
        } else {
      ni <- length(i)
      nx <- length(x)
      if (inherits(i, "logical")) {
            if (ni>nx) {
              na_idx <- is.na(i) | (i & seq_along(i)>nx)
              na_idx <- na_idx[is.na(i) | i]
            } else {
          i <- i[is.na(i) | i]
          na_idx <- rep_len(is.na(i), length(ret))
            }
          } else if (ni && min(i, na.rm=TRUE)>=0L) {
            i <- i[is.na(i) | i>0L]
            na_idx <- is.na(i) | i>length(x)
          } else {
            na_idx <- FALSE
          }
          if (any(na_idx))
                ret[na_idx] <- NA_integer64_
        }
    }
    # End NA-handling from Leonardo Silvestri
    oldClass(ret) <- cl
    remcache(ret)
    ret
}


`[.integer64` <- function(x, i, ...) {
  cl <- oldClass(x)
  ret <- NextMethod()
  # Begin NA-handling from Leonardo Silvestri
  if (!missing(i)) {
    if (inherits(i, "character")) {
      na_idx <- union(which(!(i %in% names(x))), which(is.na(i)))
      if (length(na_idx))
        ret[na_idx] <- NA_integer64_
    } else {
      na_idx <- is.na(rep(TRUE, length(x))[i])
      if (any(na_idx))
        ret[na_idx] <- NA_integer64_
    }
  }
  # End NA-handling from Leonardo Silvestri
  oldClass(ret) <- cl
  remcache(ret)
  ret
}

#' @rdname extract.replace.integer64
#' @export
`[<-.integer64` <- function(x, ..., value) {
  cl <- oldClass(x)
  value <- as.integer64(value)
  ret <- NextMethod()
  oldClass(ret) <- cl
  ret
}

#' @rdname extract.replace.integer64
#' @export
`[[.integer64` <- function(x, ...) {
  cl <- oldClass(x)
  ret <- NextMethod()
  oldClass(ret) <- cl
  ret
}

#' @rdname extract.replace.integer64
#' @export
`[[<-.integer64` <- function(x, ..., value) {
  cl <- oldClass(x)
  value <- as.integer64(value)
  ret <- NextMethod()
  oldClass(ret) <- cl
  ret
}

#' @rdname c.integer64
#' @export
c.integer64 <- function(..., recursive = FALSE) {
  l <- list(...)
  for (k in seq_along(l)) {
    if (recursive && is.list(l[[k]])) {
      l[[k]] <- do.call(c.integer64, c(l[[k]], list(recursive = TRUE)))
    } else {
      if (!is.integer64(l[[k]])) {
        nam <- names(l[[k]])
        l[[k]] <- as.integer64(l[[k]])
        names(l[[k]]) <- nam
      }
      oldClass(l[[k]]) <- NULL
    }
  }
  ret <- do.call(c, l)
  oldClass(ret) <- "integer64"
  ret
}

#' @rdname c.integer64
#' @export
cbind.integer64 <- function(...) {
  l <- list(...)
    K <- length(l)
  for (k in 1:K) {
        if (!is.integer64(l[[k]])) {
            nam <- names(l[[k]])
            l[[k]] <- as.integer64(l[[k]])
            names(l[[k]]) <- nam
        }
        oldClass(l[[k]]) <- NULL
  }
  ret <- do.call(cbind, l)
    oldClass(ret) <- "integer64"
  ret
}

#' @rdname c.integer64
#' @export
rbind.integer64 <- function(...) {
  l <- list(...)
    K <- length(l)
  for (k in 1:K) {
        if (!is.integer64(l[[k]])) {
            nam <- names(l[[k]])
            l[[k]] <- as.integer64(l[[k]])
            names(l[[k]]) <- nam
        }
        oldClass(l[[k]]) <- NULL
  }
  ret <- do.call(rbind, l)
    oldClass(ret) <- "integer64"
  ret
}

#' @rdname as.data.frame.integer64
#' @export
as.data.frame.integer64 <- function(x, ...) {
  cl <- oldClass(x)
  on.exit(setattr(x, "class", cl))
  # tenfold runtime if using attr() here instead of setattr()
  setattr(x, "class", minusclass(cl, "integer64"))
  ret <- as.data.frame(x, ...)
  k <- length(ret)
  for (i in 1:k)
   setattr(ret[[i]], "class", cl)
  ret
}

#' @export
rep.integer64 <- function(x, ...) {
    cl <- oldClass(x)
    ret <- NextMethod()
    oldClass(ret) <- cl
    ret
}

# FIXME no method dispatch for :
`:.integer64` <- function(from, to) {
  from <- as.integer64(from)
  to <- as.integer64(to)
  ret <- .Call(C_seq_integer64, from, as.integer64(1L), double(as.integer(to-from+1L)))
  oldClass(ret) <- "integer64"
  ret
}

#' @export
seq.integer64 <- function(from=NULL, to=NULL, by=NULL, length.out=NULL, along.with=NULL, ...) {
  if (is.null(length.out))
    length.out <- length(along.with)
  else
    length.out <- as.integer(length.out)

  if (is.null(by)) {
    if (is.null(from) || is.null(to))
      by <- as.integer64(1L)
    else
      by <- as.integer64(if (to < from) -1L else 1L)
  } else {
    by <- as.integer64(by)
    if (!is.null(from) && !is.null(to) && (sign(by) != (if (to < from) -1L else 1L)))
      stop("wrong sign of 'by' argument")
  }

  if (is.null(from)) {
    if (length.out && length(to))
      from <- to - (length.out-1L)*by
    else
      from <- as.integer64(1L)
  } else {
    from <- as.integer64(from)
  }

  if (!length(to)) {
    if (length.out)
      to <- from + (length.out-1L)*by
    else
      stop("not enough information provided")
  }

  if (!length.out) {
    length.out <- (to-from) %/% by + 1L
  }

  if (!length.out) return(integer64())
  if (length.out==1L) return(from)
  #return(cumsum(c(from, rep(by, length.out-1L))))
  ret <- .Call(C_seq_integer64, from, by, double(as.integer(length.out)))
  oldClass(ret) <- "integer64"
  return(ret)
}

#' @rdname xor.integer64
#' @export
`+.integer64` <- function(e1, e2) {
  if (missing(e2))
    return(e1)
  a <- binattr(e1, e2)
  e1 <- as.integer64(e1)
  e2 <- as.integer64(e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret <- double(l)
  ret <- .Call(C_plus_integer64, e1, e2, ret)
  a$class <- plusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

#' @rdname xor.integer64
#' @export
`-.integer64` <- function(e1, e2) {
  if (missing(e2)) {
    e2 <- e1
    e1 <- 0L
  }
  a <- binattr(e1, e2)
  e1 <- as.integer64(e1)
  e2 <- as.integer64(e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret <- double(l)
  .Call(C_minus_integer64, e1, e2, ret)
  a$class <- plusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

#' @rdname xor.integer64
#' @export
`%/%.integer64` <- function(e1, e2) {
  a <- binattr(e1, e2)
  e1 <- as.integer64(e1)
  e2 <- as.integer64(e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret <- double(l)
  .Call(C_intdiv_integer64, e1, e2, ret)
  a$class <- plusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

#' @rdname xor.integer64
#' @export
`%%.integer64` <- function(e1, e2) {
  a <- binattr(e1, e2)
  e1 <- as.integer64(e1)
  e2 <- as.integer64(e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret <- double(l)
  .Call(C_mod_integer64, e1, e2, ret)
  a$class <- plusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

#' @rdname xor.integer64
#' @export
`*.integer64` <- function(e1, e2) {
  a <- binattr(e1, e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret <- double(l)
  if (getOption("integer64_semantics", "old") == "old") {
    if (is.double(e2))  # implies !is.integer64(e2)
      ret <- .Call(C_times_integer64_double, as.integer64(e1), e2, ret)
    else
      ret <- .Call(C_times_integer64_integer64, as.integer64(e1), as.integer64(e2), ret)
  } else {
    # nolint next: unnecessary_nesting_linter. Good parallelism, and on a to-be-deprecated code path.
    if (is.double(e2))  # implies !is.integer64(e2)
      ret <- .Call(C_times_integer64_double, as.integer64(e1), e2, ret)
    else if (is.double(e1))
      ret <- .Call(C_times_integer64_double, as.integer64(e2), e1, ret)
    else
      ret <- .Call(C_times_integer64_integer64, as.integer64(e1), as.integer64(e2), ret)
  }
  a$class <- plusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

#' @rdname xor.integer64
#' @export
`^.integer64` <- function(e1, e2) {
  a <- binattr(e1, e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret <- double(l)
  if (is.double(e2))  # implies !is.integer64(e2)
    ret <- .Call(C_power_integer64_double, as.integer64(e1), e2, ret)
  else
    ret <- .Call(C_power_integer64_integer64, as.integer64(e1), as.integer64(e2), ret)
  a$class <- plusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

#' @rdname xor.integer64
#' @export
`/.integer64` <- function(e1, e2) {
  a <- binattr(e1, e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret <- double(l)
  if (getOption("integer64_semantics", "old") == "old") {
    if (is.double(e2))  # implies !is.integer64(e2)
      ret <- .Call(C_divide_integer64_double, as.integer64(e1), e2, ret)
    else
      ret <- .Call(C_divide_integer64_integer64, as.integer64(e1), as.integer64(e2), ret)
  } else {
    # nolint next: unnecessary_nesting_linter. Good parallelism, and on a to-be-deprecated code path.
    if (is.double(e2))  # implies !is.integer64(e2)
      ret <- .Call(C_divide_integer64_double, as.integer64(e1), e2, ret)
    else if (is.double(e1))
      ret <- .Call(C_divide_double_integer64, e1, e2, ret)
    else
      ret <- .Call(C_divide_integer64_integer64, as.integer64(e1), as.integer64(e2), ret)
  }
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}


#' @rdname format.integer64
#' @export
sign.integer64 <- function(x) {
  a <- attributes(x)
  ret <- .Call(C_sign_integer64, x, double(length(x)))
  attributes(ret) <- a
  ret
}

#' @rdname format.integer64
#' @export
abs.integer64 <- function(x) {
  a <- attributes(x)
  ret <- .Call(C_abs_integer64, x, double(length(x)))
  attributes(ret) <- a
  ret
}

#' @rdname format.integer64
#' @export
sqrt.integer64 <- function(x) {
  a <- attributes(x)
  ret <- .Call(C_sqrt_integer64, x, double(length(x)))
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

#' @rdname format.integer64
#' @export
log.integer64 <- function(x, base=NULL) {
  a <- attributes(x)
  l.x <- length(x)
  l.base <- length(base)
  l <- if (l.x==0L || (!is.null(base) && l.base==0L)) 0L else max(l.base, l.x)
  ret <- double(l)
  if (is.null(base)) {
    .Call(C_log_integer64, x, ret)
  } else if (length(base)==1L) {
    .Call(C_logbase_integer64, x, as.double(base), ret)
  } else {
    .Call(C_logvect_integer64, x, as.double(base), ret)
  }
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

#' @rdname format.integer64
#' @export
log10.integer64 <- function(x) {
  a <- attributes(x)
  ret <- .Call(C_log10_integer64, x, double(length(x)))
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

#' @rdname format.integer64
#' @export
log2.integer64 <- function(x) {
  a <- attributes(x)
  ret <- .Call(C_log2_integer64, x, double(length(x)))
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

#' @rdname format.integer64
#' @export
trunc.integer64 <- function(x, ...) x
#' @rdname format.integer64
#' @export
floor.integer64 <- function(x) x
#' @rdname format.integer64
#' @export
ceiling.integer64 <- function(x) x

#' @rdname format.integer64
#' @export
signif.integer64 <- function(x, digits=6L) x

#' @rdname format.integer64
#' @export
scale.integer64 <- function(x, center = TRUE, scale = TRUE) {
  scale(as.double(x, keep.names=TRUE), center=center, scale=scale)
}

#' @rdname format.integer64
#' @export
round.integer64 <- function(x, digits=0L) {
  if (digits >= 0L) return(x)
  a <- attributes(x)
  b <- 10L^round(-digits)
  b2 <- b %/% 2L
  d <- (x %/% b)
  db <- d * b
  r <- abs(x-db)
  ret <- ifelse((r < b2) | (r == b2 & ((d %% 2L) == 0L)), db, db + sign(x)*b)
  #a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

#' @rdname sum.integer64
#' @export
any.integer64 = function(..., na.rm=FALSE) {
  l <- list(...)
  if (length(l) == 1L)
    return(.Call(C_any_integer64, l[[1L]], na.rm, logical(1L)))
  any_elts = vapply(l, FUN.VALUE=logical(1L), function(e) {
    if (is.integer64(e)) {
      .Call(C_any_integer64, e, na.rm, logical(1L))
    } else {
      any(e, na.rm=na.rm)
    }
  })
  any(any_elts, na.rm=na.rm)
}

#' @rdname sum.integer64
#' @export
all.integer64 = function(..., na.rm=FALSE) {
  l <- list(...)
  if (length(l) == 1L)
    return(.Call(C_all_integer64, l[[1L]], na.rm, logical(1L)))
  all_elts = vapply(l, FUN.VALUE=logical(1L), function(e) {
    if (is.integer64(e)) {
      .Call(C_all_integer64, e, na.rm, logical(1L))
    } else {
      all(e, na.rm=na.rm)
    }
  })
  all(all_elts, na.rm=na.rm)
}

#' @rdname sum.integer64
#' @export
sum.integer64 = function(..., na.rm=FALSE) {
  l = list(...)
  if (length(l) == 1L) {
    ret = .Call(C_sum_integer64, l[[1L]], na.rm, double(1L))
    oldClass(ret) = "integer64"
    ret
  } else {
    ret = vapply(l, FUN.VALUE=integer64(1L), function(e) {
      if (is.integer64(e)) {
        .Call(C_sum_integer64, e, na.rm, double(1L))
      } else {
        as.integer64(sum(e, na.rm=na.rm))
      }
    })
    oldClass(ret) = "integer64"
    sum(ret, na.rm=na.rm)
  }
}

#' @rdname sum.integer64
#' @export
prod.integer64 <- function(..., na.rm=FALSE) {
  l = list(...)
  if (length(l) == 1L) {
    ret = .Call(C_prod_integer64, l[[1L]], na.rm, double(1L))
    oldClass(ret) = "integer64"
    ret
  } else {
    ret <- vapply(l, FUN.VALUE=integer64(1L), function(e) {
      if (is.integer64(e)) {
        .Call(C_prod_integer64, e, na.rm, double(1L))
      } else {
        as.integer64(prod(e, na.rm=na.rm))
      }
    })
    oldClass(ret) = "integer64"
    prod(ret, na.rm=na.rm)
  }
}

# not exactly analogous to anyNA, but convenient for min/max/range
has_no_values = function(x, na.rm) {
  if (!length(x)) return(TRUE)
  if (!na.rm) return(FALSE)
  if (is.integer64(x)) {
    .Call(C_r_ram_integer64_all_na, x=x)
  } else {
    all(is.na(x))
  }
}

#' @rdname sum.integer64
#' @export
min.integer64 = function(..., na.rm=FALSE) {
  l = list(...)
  na.rm = isTRUE(na.rm)
  ret = NULL
  no_values = NULL
  
  if (length(l) == 1L) {
    if (length(l[[1]]) > 0L) {
      ret = .Call(C_min_integer64, l[[1L]], na.rm, double(1L))
      oldClass(ret) = "integer64"
    }
  } else {
    ret = vapply(Filter(length, l), FUN.VALUE=integer64(1L), function(e) {
      if (is.integer64(e)) {
        .Call(C_min_integer64, e, na.rm, double(1L))
      } else {
        suppressWarnings(as.integer64(min(e, na.rm=na.rm)))
      }
    })
    oldClass(ret) = "integer64"
    no_values = has_no_values(ret, na.rm)
    if (!no_values) {
      ret = min(ret, na.rm=na.rm)
      no_values = NULL
    }
  }
  if (is.null(no_values))
    no_values = has_no_values(ret, na.rm)
  if (no_values) {
    ret = lim.integer64()[2L]
    warning("no non-NA value, returning the highest possible integer64 value +", ret)
  }
  ret
}

#' @rdname sum.integer64
#' @export
max.integer64 = function(..., na.rm=FALSE) {
  l = list(...)
  na.rm = isTRUE(na.rm)
  ret = NULL
  no_values = NULL

  if (length(l) == 1L) {
    if (length(l[[1]]) > 0L) {
      ret = .Call(C_max_integer64, l[[1L]], na.rm, double(1L))
      oldClass(ret) = "integer64"
    }
  } else {
    ret = vapply(Filter(length, l), FUN.VALUE=integer64(1L), function(e) {
      if (is.integer64(e)) {
        .Call(C_max_integer64, e, na.rm, double(1L))
      } else {
        suppressWarnings(as.integer64(max(e, na.rm=na.rm)))
      }
    })
    oldClass(ret) = "integer64"
    no_values = has_no_values(ret, na.rm)
    if (!no_values) {
      ret = max(ret, na.rm=na.rm)
      no_values = NULL
    }
  }
  if (is.null(no_values))
    no_values = has_no_values(ret, na.rm)
  if (no_values) {
    ret = lim.integer64()[1L]
    warning("no non-NA value, returning the lowest possible integer64 value ", ret)
  }
  ret
}

#' @rdname sum.integer64
#' @export
range.integer64 = function(..., na.rm=FALSE, finite=FALSE) {
  l = list(...)
  if (isTRUE(finite)) {
    na.rm = TRUE
  } else {
    na.rm = isTRUE(na.rm)
  }
  ret = NULL
  no_values = NULL
  
  if (length(l) == 1L) {
    if (length(l[[1]]) > 0L) {
      ret = .Call(C_range_integer64, l[[1L]], na.rm, double(2L))
      oldClass(ret) = "integer64"
    }
  } else {
    ret = vapply(Filter(length, l), FUN.VALUE=integer64(2L), function(e) {
      if (is.integer64(e)) {
        .Call(C_range_integer64, e, na.rm, double(2L))
      } else {
        suppressWarnings(as.integer64(range(e, na.rm=na.rm)))
      }
    })
    oldClass(ret) = "integer64"
    no_values = has_no_values(ret, na.rm)
    if (!no_values) {
      ret = range(ret, na.rm=na.rm)
      no_values = NULL
    }
  }
  if (is.null(no_values))
    no_values = has_no_values(ret, na.rm)
  if (no_values) {
    ret = c(lim.integer64()[2L], lim.integer64()[1L])
    warning("no non-NA value, returning c(+", ret[1L], ", ", ret[2L], ")")
  }
  ret
}

#' @rdname sum.integer64
#' @export
lim.integer64 = function() {
  ret = .Call(C_lim_integer64, double(2L))
  oldClass(ret) = "integer64"
  ret
}

#' @rdname cumsum.integer64
#' @export
diff.integer64 <- function(x, lag=1L, differences=1L, ...) {
  lag <- as.integer(lag)
  n <- length(x)
  d <- differences <- as.integer(differences)
  while (d > 0L) {
    n <- n - lag
    if (n <= 0L) {
      ret <- double()
      break
    }
    # not assigning ret<-.Call in the following is intended because faster
    if (d==differences) {
      ret <- double(n)
      .Call(C_diff_integer64, x, as.integer64(lag), as.integer64(n), ret)
    } else {
      .Call(C_diff_integer64, ret, as.integer64(lag), as.integer64(n), ret)
    }
    d <- d - 1L
  }
  # length of ret is only change once here
  length(ret) <- n
  oldClass(ret) <- "integer64"
  ret
}

#' @rdname cumsum.integer64
#' @export
cummin.integer64 <- function(x) {
  ret <- .Call(C_cummin_integer64, x, double(length(x)))
  oldClass(ret) <- "integer64"
  ret
}
#' @rdname cumsum.integer64
#' @export
cummax.integer64 <- function(x) {
  ret <- .Call(C_cummax_integer64, x, double(length(x)))
  oldClass(ret) <- "integer64"
  ret
}

#' @rdname cumsum.integer64
#' @export
cumsum.integer64 <- function(x) {
  ret <- .Call(C_cumsum_integer64, x, double(length(x)))
  oldClass(ret) <- "integer64"
  ret
}

#' @rdname cumsum.integer64
#' @export
cumprod.integer64 <- function(x) {
  ret <- .Call(C_cumprod_integer64, x, double(length(x)))
  oldClass(ret) <- "integer64"
  ret
}

#' @rdname format.integer64
#' @export
is.na.integer64 <- function(x) {
  a <- attributes(x)
  ret <- .Call(C_isna_integer64, x, logical(length(x)))
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

#' @rdname format.integer64
#' @export
is.finite.integer64 <- function(x) !is.na(x)
#' @rdname format.integer64
#' @export
is.infinite.integer64 <- function(x) rep(FALSE, length(x))
#' @rdname format.integer64
#' @export
is.nan.integer64 <- function(x) rep(FALSE, length(x))


#' @rdname xor.integer64
#' @export
`==.integer64` <- function(e1, e2) {
  a <- binattr(e1, e2)
  e1 <- as.integer64(e1)
  e2 <- as.integer64(e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret <- logical(l)
  .Call(C_EQ_integer64, e1, e2, ret)
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

#' @rdname xor.integer64
#' @export
`!=.integer64` <- function(e1, e2) {
  a <- binattr(e1, e2)
  e1 <- as.integer64(e1)
  e2 <- as.integer64(e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret <- logical(l)
  .Call(C_NE_integer64, e1, e2, ret)
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

#' @rdname xor.integer64
#' @export
`<.integer64` <- function(e1, e2) {
  a <- binattr(e1, e2)
  e1 <- as.integer64(e1)
  e2 <- as.integer64(e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret <- logical(l)
  .Call(C_LT_integer64, e1, e2, ret)
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

#' @rdname xor.integer64
#' @export
`<=.integer64` <- function(e1, e2) {
  a <- binattr(e1, e2)
  e1 <- as.integer64(e1)
  e2 <- as.integer64(e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret <- logical(l)
  .Call(C_LE_integer64, e1, e2, ret)
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

#' @rdname xor.integer64
#' @export
`>.integer64` <- function(e1, e2) {
  a <- binattr(e1, e2)
  e1 <- as.integer64(e1)
  e2 <- as.integer64(e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret <- logical(l)
  .Call(C_GT_integer64, e1, e2, ret)
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

#' @rdname xor.integer64
#' @export
`>=.integer64` <- function(e1, e2) {
  a <- binattr(e1, e2)
  e1 <- as.integer64(e1)
  e2 <- as.integer64(e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1, l2)
  ret <- logical(l)
  .Call(C_GE_integer64, e1, e2, ret)
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

#' @rdname xor.integer64
#' @export
`&.integer64` <- function(e1, e2) {
  a <- binattr(e1, e2)
  ret <- as.logical(e1) & as.logical(e2)
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

#' @rdname xor.integer64
#' @export
`|.integer64` <- function(e1, e2) {
  a <- binattr(e1, e2)
  ret <- as.logical(e1) | as.logical(e2)
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

#' @rdname xor.integer64
#' @export
xor.integer64 <- function(x, y) {
  a <- binattr(x, y)
  ret <- as.logical(x) != as.logical(y)
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}


#' @rdname format.integer64
#' @export
`!.integer64` <- function(x) {
  a <- attributes(x)
  ret <- !as.logical(x)
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

# as.vector.integer64 removed as requested by the CRAN maintainer
# as.vector.integer64 <- function(x, mode="any") {
  # ret <- NextMethod()
  # if (mode=="any")
    # oldClass(ret) <- "integer64"
  # ret
# }

# bug in R does not dispatch
#' @exportS3Method is.vector integer64
is.vector.integer64 <- function(x, mode="any") {
  cl <- minusclass(oldClass(x), "integer64")
  a <- setdiff(names(attributes(x)), c("class", "names"))
  is.na(match(mode, c("any", "integer64"))) && !length(cl) && !length(a)
}

#' @rdname as.character.integer64
#' @export
as.list.integer64 <- function(x, ...) {
  ret <- NextMethod("as.list", x, ...)
  .Call(C_as_list_integer64, ret)
}


#' @exportS3Method base::anyNA integer64
anyNA.integer64 = function(x, recursive) {
  .Call(C_r_ram_integer64_any_na, x=x)
}
