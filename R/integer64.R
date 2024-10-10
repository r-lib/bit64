# /*
# R-Code
# S3 atomic 64bit integers for R
# (c) 2011 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2011-12-11
# Last changed:  2011-12-11
#*/

#! \name{identical.integer64}
#! \alias{identical.integer64}
#! \title{
#!    Identity function for class 'integer64'
#! }
#! \description{
#!   This will discover any deviation between objects containing integer64 vectors.
#! }
#! \usage{
#!  identical.integer64(x, y, num.eq = FALSE, single.NA = FALSE
#! , attrib.as.set = TRUE, ignore.bytecode = TRUE)
#! }
#! \arguments{
#!   \item{x}{ atomic vector of class 'integer64' }
#!   \item{y}{ atomic vector of class 'integer64' }
#!   \item{num.eq}{ see \code{\link{identical}} }
#!   \item{single.NA}{ see \code{\link{identical}} }
#!   \item{attrib.as.set}{ see \code{\link{identical}} }
#!   \item{ignore.bytecode}{ see \code{\link{identical}} }
#!   \item{ignore.environment}{ see \code{\link{identical}} }
#!   \item{ignore.srcref}{ see \code{\link{identical}} }
#!   \item{extptr.as.ref}{ see \code{\link{identical}} }
#! }
#! \details{
#!   This is simply a wrapper to \code{\link{identical}} with default arguments \code{num.eq = FALSE, single.NA = FALSE}.
#! }
#! \value{
#!   A single logical value, \code{TRUE} or \code{FALSE}, never \code{NA} and never anything other than a single value.
#! }
#! \author{
#! Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
#! }
#! \keyword{ classes }
#! \keyword{ manip }
#! \seealso{ \code{\link{==.integer64}} \code{\link{identical}} \code{\link{integer64}}  }
#! \examples{
#!   i64 <- as.double(NA); class(i64) <- "integer64"
#!   identical(i64-1, i64+1)
#!   identical.integer64(i64-1, i64+1)
#! }


#! \name{as.character.integer64}
#! \alias{as.character.integer64}
#! \alias{as.double.integer64}
#! \alias{as.integer.integer64}
#! \alias{as.logical.integer64}
#! \alias{as.bitstring}
#! \alias{print.bitstring}
#! \alias{as.bitstring.integer64}
#! \alias{as.factor.integer64}
#! \alias{as.ordered.integer64}
#! \alias{as.list.integer64}
#! \title{
#!    Coerce from integer64
#! }
#! \description{
#!   Methods to coerce integer64 to other atomic types.
#!   'as.bitstring' coerces to a human-readable bit representation (strings of zeroes and ones).
#!   The methods \code{\link{format}}, \code{\link{as.character}}, \code{\link{as.double}},
#!   \code{\link{as.logical}}, \code{\link{as.integer}} do what you would expect.
#! }
#! \usage{
#!  as.bitstring(x, \dots)
#!  \method{as.bitstring}{integer64}(x, \dots)
#!  \method{print}{bitstring}(x, \dots)
#!  \method{as.character}{integer64}(x, \dots)
#!  \method{as.double}{integer64}(x, keep.names = FALSE, \dots)
#!  \method{as.integer}{integer64}(x, \dots)
#!  \method{as.logical}{integer64}(x, \dots)
#!  \method{as.factor}{integer64}(x)
#!  \method{as.ordered}{integer64}(x)
#!  \method{as.list}{integer64}(x, \dots)
#! }
#! \arguments{
#!   \item{x}{ an integer64 vector }
#!   \item{keep.names}{ FALSE, set to TRUE to keep a names vector }
#!   \item{\dots}{ further arguments to the \code{\link{NextMethod}} }
#! }
#! \value{
#!   \code{as.bitstring} returns a string of class 'bitstring'. \cr
#!   The other methods return atomic vectors of the expected types
#! }
#! \author{
#! Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
#! }
#! \keyword{ classes }
#! \keyword{ manip }
#! \seealso{ \code{\link{as.integer64.character}} \code{\link{integer64}}  }
#! \examples{
#!   as.character(lim.integer64())
#!   as.bitstring(lim.integer64())
#!   as.bitstring(as.integer64(c(
#!    -2,-1,NA,0:2
#!   )))
#! }

#! \name{as.integer64.character}
#! \alias{as.integer64}
#! \alias{as.integer64.integer64}
#! \alias{as.integer64.NULL}
#! \alias{as.integer64.bitstring}
#! \alias{as.integer64.character}
#! \alias{as.integer64.double}
#! \alias{as.integer64.integer}
#! \alias{as.integer64.logical}
#! \alias{as.integer64.factor}
#! \alias{NA_integer64_}
#! \title{
#!    Coerce to integer64
#! }
#! \description{
#!   Methods to coerce from other atomic types to integer64.
#! }
#! \usage{
#!  NA_integer64_
#!  as.integer64(x, \dots)
#!  \method{as.integer64}{integer64}(x, \dots)
#!  \method{as.integer64}{NULL}(x, \dots)
#!  \method{as.integer64}{character}(x, \dots)
#!  \method{as.integer64}{bitstring}(x, \dots)
#!  \method{as.integer64}{double}(x, keep.names = FALSE, \dots)
#!  \method{as.integer64}{integer}(x, \dots)
#!  \method{as.integer64}{logical}(x, \dots)
#!  \method{as.integer64}{factor}(x, \dots)
#! }
#! \arguments{
#!   \item{x}{ an atomic vector }
#!   \item{keep.names}{ FALSE, set to TRUE to keep a names vector }
#!   \item{\dots}{ further arguments to the \code{\link{NextMethod}} }
#! }
#! \details{
#!   \code{as.integer64.character} is realized using C function \code{strtoll} which does not support scientific notation.
#!   Instead of '1e6' use '1000000'.
#!   \code{as.integer64.bitstring} evaluates characters '0' anbd ' ' as zero-bit,
#!   all other one byte characters as one-bit,
#!   multi-byte characters are not allowed,
#!   strings shorter than 64 characters are treated as if they were left-padded with '0',
#!   strings longer than 64 bytes are mapped to \code{NA_INTEGER64} and a warning is emitted.
#! }
#! \value{
#!   The other methods return atomic vectors of the expected types
#! }
#! \author{
#! Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
#! }
#! \keyword{ classes }
#! \keyword{ manip }
#! \seealso{ \code{\link{as.character.integer64}} \code{\link{integer64}}  }
#! \examples{
#! as.integer64(as.character(lim.integer64()))
#! as.integer64(
#!   structure(c("1111111111111111111111111111111111111111111111111111111111111110",
#!               "1111111111111111111111111111111111111111111111111111111111111111",
#!               "1000000000000000000000000000000000000000000000000000000000000000",
#!               "0000000000000000000000000000000000000000000000000000000000000000",
#!               "0000000000000000000000000000000000000000000000000000000000000001",
#!               "0000000000000000000000000000000000000000000000000000000000000010"
#!   ), class = "bitstring")
#! )
#! as.integer64(
#!  structure(c("............................................................... ",
#!              "................................................................",
#!              ".                                                               ",
#!              "",
#!              ".",
#!              "10"
#!   ), class = "bitstring")
#! )
#! }


#! \name{extract.replace.integer64}
#! \alias{[.integer64}
#! \alias{[[.integer64}
#! \alias{[[<-.integer64}
#! \alias{[<-.integer64}
#! \title{
#!    Extract or Replace Parts of an integer64 vector
#! }
#! \description{
#!   Methods to extract and replace parts of an integer64 vector.
#! }
#! \usage{
#!  \method{[}{integer64}(x, i, \dots)
#!  \method{[}{integer64}(x, \dots) <- value
#!  \method{[[}{integer64}(x, \dots)
#!  \method{[[}{integer64}(x, \dots) <- value
#! }
#! \arguments{
#!   \item{x}{ an atomic vector }
#!   \item{i}{ indices specifying elements to extract }
#!   \item{value}{ an atomic vector with values to be assigned }
#!   \item{\dots}{ further arguments to the \code{\link{NextMethod}} }
#! }
#! \note{
#!   You should not subscript non-existing elements and not use \code{NA}s as subscripts.
#!   The current implementation returns \code{9218868437227407266} instead of \code{NA}.
#! }
#! \value{
#!   A vector or scalar of class 'integer64'
#! }
#! \author{
#! Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
#! }
#! \keyword{ classes }
#! \keyword{ manip }
#! \seealso{ \code{\link{[}} \code{\link{integer64}}  }
#! \examples{
#!   as.integer64(1:12)[1:3]
#!   x <- as.integer64(1:12)
#!   dim(x) <- c(3,4)
#!   x
#!   x[]
#!   x[,2:3]
#!   \dontshow{
#!     r <- c(runif64(1e3, lim.integer64()[1], lim.integer64()[2]), NA, -2:2)
#!     stopifnot(identical(r, as.integer64(as.bitstring(r))))
#!   }
#! }

#! \name{format.integer64}
#! \alias{format.integer64}
#! \alias{is.na.integer64}
#! \alias{is.nan.integer64}
#! \alias{is.finite.integer64}
#! \alias{is.infinite.integer64}
#! \alias{!.integer64}
#! \alias{sign.integer64}
#! \alias{abs.integer64}
#! \alias{sqrt.integer64}
#! \alias{log.integer64}
#! \alias{log2.integer64}
#! \alias{log10.integer64}
#! \alias{floor.integer64}
#! \alias{ceiling.integer64}
#! \alias{trunc.integer64}
#! \alias{round.integer64}
#! \alias{signif.integer64}
#! \alias{scale.integer64}
#! \title{
#!    Unary operators and functions for integer64 vectors
#! }
#! \description{
#!   Unary operators and functions for integer64 vectors.
#! }
#! \usage{
#! \method{format}{integer64}(x, justify="right", \dots)
#! \method{is.na}{integer64}(x)
#! \method{is.nan}{integer64}(x)
#! \method{is.finite}{integer64}(x)
#! \method{is.infinite}{integer64}(x)
#! \method{!}{integer64}(x)
#! \method{sign}{integer64}(x)
#! \method{abs}{integer64}(x)
#! \method{sqrt}{integer64}(x)
#! \method{log}{integer64}(x, base)
#! \method{log2}{integer64}(x)
#! \method{log10}{integer64}(x)
#! \method{floor}{integer64}(x)
#! \method{ceiling}{integer64}(x)
#! \method{trunc}{integer64}(x, \dots)
#! \method{round}{integer64}(x, digits=0)
#! \method{signif}{integer64}(x, digits=6)
#! \method{scale}{integer64}(x, center = TRUE, scale = TRUE)
#! }
#! \arguments{
#!   \item{x}{ an atomic vector of class 'integer64'}
#!   \item{base}{ an atomic scalar (we save 50\% log-calls by not allowing a vector base) }
#!   \item{digits}{ integer indicating the number of decimal places (round) or significant digits (signif) to be used.
#!                  Negative values are allowed (see \code{\link{round}}) }
#!   \item{justify}{ should it be right-justified (the default), left-justified, centred or left alone. }
#!   \item{center}{ see \code{\link{scale}} }
#!   \item{scale}{  see \code{\link{scale}} }
#!   \item{\dots}{ further arguments to the \code{\link{NextMethod}} }
#! }
#! \value{
#!   \code{\link{format}} returns a character vector \cr
#!   \code{\link{is.na}} and \code{\link{!}} return a logical vector \cr
#!   \code{\link{sqrt}}, \code{\link{log}}, \code{\link{log2}} and \code{\link{log10}} return a double vector \cr
#!   \code{\link{sign}}, \code{\link{abs}}, \code{\link{floor}}, \code{\link{ceiling}}, \code{\link{trunc}} and
#!   \code{\link{round}} return a vector of class 'integer64' \cr
#!   \code{\link{signif}} is not implemented
#! }
#! \author{
#! Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
#! }
#! \keyword{ classes }
#! \keyword{ manip }
#! \seealso{ \code{\link{xor.integer64}} \code{\link{integer64}}  }
#! \examples{
#!   sqrt(as.integer64(1:12))
#! \dontshow{
#! i <- -999:999
#! for (s in -3:3){
#! r <- as.integer64(round(as.integer(i), s))
#!   r64 <- round(as.integer64(i), s)
#!   stopifnot(identical(r,r64))
#! }
#! }
#! }

#! \name{xor.integer64}
#! \alias{&.integer64}
#! \alias{|.integer64}
#! \alias{xor.integer64}
#! \alias{!=.integer64}
#! \alias{==.integer64}
#! \alias{<.integer64}
#! \alias{<=.integer64}
#! \alias{>.integer64}
#! \alias{>=.integer64}
#! \alias{+.integer64}
#! \alias{-.integer64}
#! \alias{*.integer64}
#! \alias{^.integer64}
#! \alias{/.integer64}
#! \alias{\%/\%.integer64}
#! \alias{\%\%.integer64}
#! \alias{binattr}
#! \title{
#!    Binary operators for integer64 vectors
#! }
#! \description{
#!   Binary operators for integer64 vectors.
#! }
#! \usage{
#! \method{&}{integer64}(e1,e2)
#! \method{|}{integer64}(e1,e2)
#! \method{xor}{integer64}(x,y)
#! \method{!=}{integer64}(e1,e2)
#! \method{==}{integer64}(e1,e2)
#! \method{<}{integer64}(e1,e2)
#! \method{<=}{integer64}(e1,e2)
#! \method{>}{integer64}(e1,e2)
#! \method{>=}{integer64}(e1,e2)
#! \method{+}{integer64}(e1,e2)
#! \method{-}{integer64}(e1,e2)
#! \method{*}{integer64}(e1,e2)
#! \method{^}{integer64}(e1,e2)
#! \method{/}{integer64}(e1,e2)
#! \method{\%/\%}{integer64}(e1,e2)
#! \method{\%\%}{integer64}(e1,e2)
#! binattr(e1,e2) # for internal use only
#! }
#! \arguments{
#!   \item{e1}{ an atomic vector of class 'integer64'}
#!   \item{e2}{ an atomic vector of class 'integer64'}
#!   \item{x}{ an atomic vector of class 'integer64'}
#!   \item{y}{ an atomic vector of class 'integer64'}
#! }
#! \value{
#!   \code{\link{&}}, \code{\link{|}}, \code{\link{xor}}, \code{\link{!=}}, \code{\link{==}},
#!   \code{\link{<}}, \code{\link{<=}}, \code{\link{>}}, \code{\link{>=}} return a logical vector \cr
#!   \code{\link{^}} and \code{\link{/}} return a double vector\cr
#!   \code{\link{+}}, \code{\link{-}}, \code{\link{*}}, \code{\link{\%/\%}}, \code{\link{\%\%}}
#!    return a vector of class 'integer64'
#! }
#! \author{
#! Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
#! }
#! \keyword{ classes }
#! \keyword{ manip }
#! \seealso{ \code{\link{format.integer64}} \code{\link{integer64}}  }
#! \examples{
#!   as.integer64(1:12) - 1
#!   options(integer64_semantics="new")
#!   d <- 2.5
#!   i <- as.integer64(5)
#!   d/i  # new 0.5
#!   d*i  # new 13
#!   i*d  # new 13
#!   options(integer64_semantics="old")
#!   d/i  # old: 0.4
#!   d*i  # old: 10
#!   i*d  # old: 13
#! }


#! \name{sum.integer64}
#! \alias{all.integer64}
#! \alias{any.integer64}
#! \alias{min.integer64}
#! \alias{max.integer64}
#! \alias{range.integer64}
#! \alias{lim.integer64}
#! \alias{sum.integer64}
#! \alias{prod.integer64}
#! \title{
#!    Summary functions for integer64 vectors
#! }
#! \description{
#!   Summary functions for integer64 vectors.
#!   Function 'range' without arguments returns the smallest and largest value of the 'integer64' class.
#! }
#! \usage{
#! \method{all}{integer64}(\dots, na.rm = FALSE)
#! \method{any}{integer64}(\dots, na.rm = FALSE)
#! \method{min}{integer64}(\dots, na.rm = FALSE)
#! \method{max}{integer64}(\dots, na.rm = FALSE)
#! \method{range}{integer64}(\dots, na.rm = FALSE, finite = FALSE)
#! lim.integer64()
#! \method{sum}{integer64}(\dots, na.rm = FALSE)
#! \method{prod}{integer64}(\dots, na.rm = FALSE)
#! }
#! \arguments{
#!   \item{\dots}{ atomic vectors of class 'integer64'}
#!   \item{na.rm}{ logical scalar indicating whether to ignore NAs }
#!   \item{finite}{ logical scalar indicating whether to ignore NAs (just for compatibility with \code{\link{range.default}}) }
#! }
#! \details{
#!   The numerical summary methods always return \code{integer64}.
#!   Therefor the methods for \code{min},\code{max} and \code{range} do not return \code{+Inf,-Inf}
#!   on empty arguments, but \code{+9223372036854775807, -9223372036854775807} (in this sequence).
#!   The same is true if only  \code{NA}s are submitted with argument \code{na.rm=TRUE}.
#!  \cr
#!   \code{lim.integer64} returns these limits in proper order \code{-9223372036854775807, +9223372036854775807} and without a \code{\link{warning}}.
#! }
#! \value{
#!   \code{\link{all}} and \code{\link{any}} return a logical scalar\cr
#!   \code{\link{range}} returns a integer64 vector with two elements\cr
#!   \code{\link{min}}, \code{\link{max}}, \code{\link{sum}} and \code{\link{prod}} return a integer64 scalar
#! }
#! \author{
#! Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
#! }
#! \keyword{ classes }
#! \keyword{ manip }
#! \seealso{ \code{\link{mean.integer64}} \code{\link{cumsum.integer64}} \code{\link{integer64}}  }
#! \examples{
#!   lim.integer64()
#!   range(as.integer64(1:12))
#! }


#! \name{cumsum.integer64}
#! \alias{cummin.integer64}
#! \alias{cummax.integer64}
#! \alias{cumsum.integer64}
#! \alias{cumprod.integer64}
#! \alias{diff.integer64}
#! \title{
#!    Cumulative Sums, Products, Extremes and lagged differences
#! }
#! \description{
#!   Cumulative Sums, Products, Extremes and lagged differences
#! }
#! \usage{
#! \method{cummin}{integer64}(x)
#! \method{cummax}{integer64}(x)
#! \method{cumsum}{integer64}(x)
#! \method{cumprod}{integer64}(x)
#! \method{diff}{integer64}(x, lag = 1L, differences = 1L, \dots)
#! }
#! \arguments{
#!   \item{x}{ an atomic vector of class 'integer64'}
#!   \item{lag}{ see \code{\link{diff}} }
#!   \item{differences}{ see \code{\link{diff}} }
#!   \item{\dots}{ ignored }
#! }
#! \value{
#!   \code{\link{cummin}}, \code{\link{cummax}} , \code{\link{cumsum}} and \code{\link{cumprod}}
#!      return a integer64 vector of the same length as their input\cr
#!   \code{\link{diff}} returns a integer64 vector shorter by \code{lag*differences} elements \cr
#! }
#! \author{
#! Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
#! }
#! \keyword{ classes }
#! \keyword{ manip }
#! \seealso{ \code{\link{sum.integer64}} \code{\link{integer64}}  }
#! \examples{
#!   cumsum(rep(as.integer64(1), 12))
#!   diff(as.integer64(c(0,1:12)))
#!   cumsum(as.integer64(c(0, 1:12)))
#!   diff(cumsum(as.integer64(c(0,0,1:12))), differences=2)
#! }


#! \name{c.integer64}
#! \alias{c.integer64}
#! \alias{cbind.integer64}
#! \alias{rbind.integer64}
#! \title{
#!    Concatenating integer64 vectors
#! }
#! \description{
#!   The ususal functions 'c', 'cbind' and 'rbind'
#! }
#! \usage{
#! \method{c}{integer64}(\dots, recursive = FALSE)
#! \method{cbind}{integer64}(\dots)
#! \method{rbind}{integer64}(\dots)
#! }
#! \arguments{
#!   \item{\dots}{ two or more arguments coerced to 'integer64' and passed to \code{\link{NextMethod}} }
#!   \item{recursive}{ logical. If \code{recursive = TRUE}, the function recursively descends through lists (and pairlists) combining all their elements into a vector. }
#! }
#! \value{
#!   \code{\link{c}} returns a integer64 vector of the total length of the input \cr
#!   \code{\link{cbind}} and \code{\link{rbind}} return a integer64 matrix
#! }
#! \note{
#!   R currently only dispatches generic 'c' to method 'c.integer64' if the first argument is 'integer64'
#! }
#! \author{
#! Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
#! }
#! \keyword{ classes }
#! \keyword{ manip }
#! \seealso{ \code{\link{rep.integer64}} \code{\link{seq.integer64}}
#!           \code{\link{as.data.frame.integer64}} \code{\link{integer64}}
#! }
#! \examples{
#!   c(as.integer64(1), 2:6)
#!   cbind(1:6, as.integer(1:6))
#!   rbind(1:6, as.integer(1:6))
#! }


#! \name{rep.integer64}
#! \alias{rep.integer64}
#! \title{
#!    Replicate elements of integer64 vectors
#! }
#! \description{
#!   Replicate elements of integer64 vectors
#! }
#! \usage{
#! \method{rep}{integer64}(x, \dots)
#! }
#! \arguments{
#!   \item{x}{ a vector of 'integer64' to be replicated }
#!   \item{\dots}{ further arguments passed to \code{\link{NextMethod}} }
#! }
#! \value{
#!   \code{\link{rep}} returns a integer64 vector
#! }
#! \author{
#! Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
#! }
#! \keyword{ classes }
#! \keyword{ manip }
#! \seealso{ \code{\link{c.integer64}} \code{\link{rep.integer64}}
#!           \code{\link{as.data.frame.integer64}} \code{\link{integer64}}
#! }
#! \examples{
#!   rep(as.integer64(1:2), 6)
#!   rep(as.integer64(1:2), c(6,6))
#!   rep(as.integer64(1:2), length.out=6)
#! }


#! \name{seq.integer64}
#! \alias{seq.integer64}
#! \title{
#!    integer64: Sequence Generation
#! }
#! \description{
#!   Generating sequence of integer64 values
#! }
#! \usage{
#! \method{seq}{integer64}(from = NULL, to = NULL, by = NULL, length.out = NULL, along.with = NULL, \dots)
#! }
#! \arguments{
#!   \item{from}{ integer64 scalar (in order to dispatch the integer64 method of \code{\link{seq}} }
#!   \item{to}{ scalar }
#!   \item{by}{ scalar }
#!   \item{length.out}{ scalar }
#!   \item{along.with}{ scalar }
#!   \item{\dots}{ ignored }
#! }
#! \details{
#!   \code{seq.integer64} does coerce its arguments 'from', 'to' and 'by' to \code{integer64}.
#!   If not provided, the argument 'by' is automatically determined as \code{+1} or \code{-1},
#!   but the size of 'by' is not calculated as in \code{\link{seq}} (because this might result in a non-integer value).
#! }
#! \value{
#!   an integer64 vector with the generated sequence
#! }
#! \note{
#!   In base R \code{\link{:}} currently is not generic and does not dispatch, see section "Limitations inherited from Base R" in \code{\link{integer64}}
#! }
#! \author{
#! Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
#! }
#! \keyword{ classes }
#! \keyword{ manip }
#! \seealso{ \code{\link{c.integer64}} \code{\link{rep.integer64}}
#!           \code{\link{as.data.frame.integer64}} \code{\link{integer64}}
#! }
#! \examples{
#!   # colon not activated: as.integer64(1):12
#!   seq(as.integer64(1), 12, 2)
#!   seq(as.integer64(1), by=2, length.out=6)
#! }


#! \name{as.data.frame.integer64}
#! \alias{as.data.frame.integer64}
#! \title{
#!    integer64: Coercing to data.frame column
#! }
#! \description{
#!   Coercing integer64 vector to data.frame.
#! }
#! \usage{
#!   \method{as.data.frame}{integer64}(x, \dots)
#! }
#! \arguments{
#!   \item{x}{ an integer64 vector }
#!   \item{\dots}{ passed to NextMethod \code{\link{as.data.frame}} after removing the 'integer64' class attribute }
#! }
#! \value{
#!   a one-column data.frame containing an integer64 vector
#! }
#! \details{
#!   'as.data.frame.integer64' is rather not intended to be called directly,
#!   but it is required to allow integer64 as data.frame columns.
#! }
#! \note{
#!   This is currently very slow -- any ideas for improvement?
#! }
#! \author{
#! Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
#! }
#! \keyword{ classes }
#! \keyword{ manip }
#! \seealso{
#!   \code{\link{cbind.integer64}} \code{\link{integer64}}  %as.vector.integer64 removed as requested by the CRAN maintainer \code{\link{as.vector.integer64}}
#! }
#! \examples{
#!   as.data.frame.integer64(as.integer64(1:12))
#!   data.frame(a=1:12, b=as.integer64(1:12))
#! }



#! \name{plusclass}
#! \alias{plusclass}
#! \alias{minusclass}
#! \title{
#!    integer64: Maintaining S3 class attribute
#! }
#! \description{
#!   Maintaining integer64 S3 class attribute.
#! }
#! \usage{
#!   plusclass(class, whichclass)
#!   minusclass(class, whichclass)
#! }
#! \arguments{
#!   \item{class}{ NULL or a character vector of class attributes }
#!   \item{whichclass}{ the (single) class name to add or remove from the class vector  }
#! }
#! \value{
#!   NULL or a character vector of class attributes
#! }
#! \author{
#! Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
#! }
#! \keyword{ classes }
#! \keyword{ manip }
#! \keyword{ internal }
#! \seealso{
#!   \code{\link{oldClass}} \code{\link{integer64}}
#! }
#! \examples{
#!   plusclass("inheritingclass","integer64")
#!   minusclass(c("inheritingclass","integer64"), "integer64")
#! }


#! \name{all.equal.integer64}
#! \alias{all.equal.integer64}
#! \title{
#!    Test if two integer64 vectors are all.equal
#! }
#! \description{
#!    A utility to compare integer64 objects 'x' and 'y' testing for ‘near equality’, see \code{\link{all.equal}}.
#! }
#! \usage{
#!   \method{all.equal}{integer64}(
#!   target
#! , current
#! , tolerance = sqrt(.Machine$double.eps)
#! , scale = NULL
#! , countEQ = FALSE
#! , formatFUN = function(err, what) format(err)
#! , ...
#! , check.attributes = TRUE
#! )
#! }
#! \arguments{
#!   \item{target}{ a vector of 'integer64' or an object that can be coerced with \code{\link{as.integer64}} }
#!   \item{current}{ a vector of 'integer64' or an object that can be coerced with \code{\link{as.integer64}} }
#!   \item{tolerance}{numeric \eqn{\ge} 0.  Differences smaller than
#!     \code{tolerance} are not reported.  The default value is close to
#!     \code{1.5e-8}.}
#!   \item{scale}{\code{NULL} or numeric > 0, typically of length 1 or
#!     \code{length(target)}.  See \sQuote{Details}.}
#!   \item{countEQ}{logical indicating if the \code{target == current}
#!     cases should be counted when computing the mean (absolute or
#!     relative) differences.  The default, \code{FALSE} may seem
#!     misleading in cases where \code{target} and \code{current} only
#!     differ in a few places; see the extensive example.}
#!   \item{formatFUN}{a \code{\link{function}} of two arguments,
#!     \code{err}, the relative, absolute or scaled error, and
#!     \code{what}, a character string indicating the \emph{kind} of error;
#!     maybe used, e.g., to format relative and absolute errors differently.}
#!   \item{\dots}{further arguments are ignored}
#!   \item{check.attributes}{logical indicating if the
#!     \code{\link{attributes}} of \code{target} and \code{current}
#!     (other than the names) should be compared.}
#! }
#! \value{
#!   Either ‘TRUE’ (‘NULL’ for ‘attr.all.equal’) or a vector of ‘mode’
#!   ‘"character"’ describing the differences between ‘target’ and
#!   ‘current’.
#! }
#! \details{
#!    In \code{\link{all.equal.numeric}} the type \code{integer} is treated as a proper subset of \code{double}
#!    i.e. does not complain about comparing \code{integer} with \code{double}.
#!    Following this logic \code{all.equal.integer64} treats \code{integer} as a proper subset of \code{integer64}
#!    and does not complain about comparing \code{integer} with \code{integer64}. \code{double} also compares without warning
#!    as long as the values are within \code{\link{lim.integer64}}, if \code{double} are bigger \code{all.equal.integer64}
#!    complains about the \code{all.equal.integer64 overflow warning}. For further details see \code{\link{all.equal}}.
#! }
#! \note{
#!    \code{\link{all.equal}} only dispatches to this method if the first argument is \code{integer64},
#!    calling \code{\link{all.equal}} with a \code{non-integer64} first and a \code{integer64} second argument
#!    gives undefined behavior!
#! }
#! \author{
#!   Leonardo Silvestri (for package nanotime)
#! }
#! \seealso{
#!   \code{\link{all.equal}}
#! }
#! \examples{
#!   all.equal(as.integer64(1:10), as.integer64(0:9))
#!   all.equal(as.integer64(1:10), as.integer(1:10))
#!   all.equal(as.integer64(1:10), as.double(1:10))
#!   all.equal(as.integer64(1), as.double(1e300))
#! }

# if (!exists(":.default")){
    # ":.default" <- get(":")
    # ":" <- function(from,to)UseMethod(":")
# }

setOldClass("integer64")


# contributed by Leonardo Silvestri with modifications of JO
all.equal.integer64  <- function (
  target
, current
, tolerance = sqrt(.Machine$double.eps)
, scale = NULL
, countEQ = FALSE
, formatFUN = function(err, what) format(err)
, ...
, check.attributes = TRUE
)
{
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
  if (!is.integer64(target)){
    cl <- oldClass(target)
    oldClass(target) <- NULL
    target <- try(as.integer64(target))
    if (inherits(target, 'try-error'))
      return(paste("while coercing 'target' to 'integer64':",  attr(target, "condition")$message))
    oldClass(target) <- c(cl, "integer64")
  }
  if (!is.integer64(current)){
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
    return(if (is.null(msg)) TRUE else msg)
  if (countEQ) {
    N <- length(out)
    sabst0 <- sum(abs(target[out]))
  } else {
    sabst0 <- 0.0
  }
  target <- target[!out]
  current <- current[!out]
  if (!countEQ)
    N <- length(target)
  xy <- sum(abs(target - current))/N
  what <- if (is.null(scale)) {
            xn <- (sabst0 + sum(abs(target)))/N
            if (is.finite(xn) && xn > tolerance) {
              xy <- xy/xn
              "relative"
            } else {
              "absolute"
            }
          } else {
            stopifnot(scale > 0.0)
            xy <- xy/scale
            if (all(abs(scale - 1.0) < 1e-07))
              "absolute"
            else "scaled"
          }
  if (is.na(xy) || xy > tolerance)
    msg <- c(msg, paste("Mean", what, "difference:", formatFUN(xy, what)))
  if (is.null(msg)) {
    TRUE
  } else msg
}

# nocov start
if (FALSE){
  require(bit64)
  a <- as.integer64(1L)
  b <- 10L
  oldClass(a) <- c("j", oldClass(a))
  oldClass(b) <- c("j", oldClass(b))
  all.equal(a,b)

  a <- 1.0
  b <- 10L
  oldClass(a) <- c("j", oldClass(a))
  oldClass(b) <- c("j", oldClass(b))
  all.equal(a,b)

  a <- as.integer64(9e17)
  b <- 9e18
  oldClass(a) <- c("j", oldClass(a))
  oldClass(b) <- c("j", oldClass(b))
  all.equal(a,b)

  a <- as.integer64(9e18)
  b <- 9e19
  oldClass(a) <- c("j", oldClass(a))
  oldClass(b) <- c("j", oldClass(b))
  all.equal(a,b)

  a <- as.integer64(c(1L,NA))
  b <- as.integer(c(1L,NA))
  all.equal(a,b)

  a <- as.integer64(c(1L,NA))
  b <- as.double(c(1L,NA))
  all.equal(a,b)

  a <- as.integer64(c(1.0,Inf))
  b <- as.integer(c(1.0,Inf))
  all.equal(a,b)

  a <- as.integer64(c(1.0,Inf))
  b <- as.double(c(1.0,Inf))
  all.equal(a,b)
}
# nocov end

identical.integer64 <- function(x, y
, num.eq = FALSE
, single.NA = FALSE
, attrib.as.set = TRUE
, ignore.bytecode = TRUE
, ignore.environment = FALSE
, ignore.srcref = TRUE
, extptr.as.ref = FALSE
)
identical(x=x, y=y
, num.eq = num.eq
, single.NA = single.NA
, attrib.as.set = attrib.as.set
, ignore.bytecode = ignore.bytecode
, ignore.environment = ignore.environment
, ignore.srcref = ignore.srcref
, extptr.as.ref = extptr.as.ref
)


as.integer64 <- function (x, ...)
UseMethod("as.integer64")

as.bitstring <- function(x, ...)
UseMethod("as.bitstring")



minusclass <- function(class, whichclass){
  if (length(class)){
      i <- whichclass==class
      if (any(i))
        class[!i]
      else
        class
  }else
    class
}

plusclass <- function(class, whichclass){
  if (length(class)){
      i <- whichclass==class
      if (any(i))
        class
      else
        c(class, whichclass)
  }else
    whichclass
}

# nocov start
if (FALSE){
  # version until 0.9-7
  binattr <- function(e1, e2) {
    d1 <- dim(e1)
    d2 <- dim(e2)
    n1 <- length(e1)
    n2 <- length(e2)
    if (length(d1)) {
      if (length(d2)) {
        if (!identical(dim(e1),dim(e2)))
          stop("non-conformable arrays")
      } else {
        if (n2>n1)
          stop("length(e2) does not match dim(e1)")
        if (n1%%n2)
          warning("length(e1) not a multiple length(e2)")
      }
      attributes(e1)
    } else if (length(d2)) {
      if (n1>n2)
        stop("length(e1) does not match dim(n2)")
      if (n2%%n1)
      warning("length(e2) not a multiple length(e1)")
      attributes(e2)
    } else {
      if (n1<n2) {
        if (n2%%n1)
          warning("length(e2) not a multiple length(e1)")
      } else {
        # nolint next: unnecessary_nesting_linter. Good parallelism.
        if (n1%%n2)
          warning("length(e1) not a multiple length(e2)")
      }
      attributes(e1)
    }
  }
}
# nocov end

# Version of Leonardo Silvestri
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
        stop("non-conformable arrays")
    } else{
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
  if (n1 == n2){
    ## if same size take attribute from e1 if it exists, else from e2
    if (n1==0L){
      ae1 <- attributes(e1)[c("class","dim","dimnames")]
      ae2 <- attributes(e2)[c("class","dim","dimnames")]
    }
    ae1 <- attributes(e1)
    ae2 <- attributes(e2)
    nae1 <- names(attributes(e1))
    nae2 <- names(attributes(e2))
    if (n1==0L){
      ae1 <- ae1[nae1 %in% c("class","dim","dimnames")]
      ae2 <- ae1[nae1 %in% c("class","dim","dimnames")]
    }
    allattr <- list()
    for (a in union(nae1, nae2))
      if (a %in% nae1)
        allattr[[a]] <- ae1[[a]]
    else
      allattr[[a]] <- ae2[[a]]
    allattr
  }else if (n1 == 0L || n1 > n2) {
    attributes(e1)
  } else {
    attributes(e2)
  }
}


# as.matrix.integer64 <- function (x, ...) {
#   if (!is.matrix(x)){
#     dim(x) <- c(length(x), 1L)
#     dimnames(x) <- if (!is.null(names(x))) list(names(x), NULL) else NULL
#   }
#   x
# }



# nocov start
if (FALSE){
  x <- integer64(0L)
  y <- integer64(0L)
  #dim(x) <- c(2L,2L)
  dim(y) <- c(0L,0L)
  dimnames(y) <- list(character(0L),character(0L))
  #dim(x) <- c(1L,4L)
  #dim(y) <- c(4L,1L)

  attr(x,"x") <- "x"
  attr(y,"y") <- "y"

  z <- x - y
  z
  dim(z)
  dimnames(z)

  z <- y - x
  z
  dim(z)
  dimnames(z)

  ret <- "integer64(0L)"
  attributes(ret) <- list(dim = c(0L, 0L), class = character(0L), dimnames = list(NULL,NULL))

}
# nocov end

integer64 <- function(length=0L){
  ret <- double(length)
  oldClass(ret) <- "integer64"
  ret
}

is.integer64 <- function(x)inherits(x, "integer64")

as.integer64.NULL <- function (x, ...){
  ret <- double()
  oldClass(ret) <- "integer64"
  ret
}

as.integer64.integer64 <- function(x, ...)x

as.integer64.double <- function(x, keep.names=FALSE, ...){
  ret <- .Call(C_as_integer64_double, x, double(length(x)))
  if (keep.names)
    names(ret) <- names(x)
  oldClass(ret) <- "integer64"
  ret
}

as.integer64.logical <- as.integer64.integer <- function(x, ...){
  ret <- .Call(C_as_integer64_integer, x, double(length(x)))
  oldClass(ret) <- "integer64"
  ret
}

as.integer64.character <- function(x, ...){
  n <- length(x)
  ret <- .Call(C_as_integer64_character, x, rep(NA_real_, n))
  oldClass(ret) <- "integer64"
  ret
}

as.integer64.factor <- function(x, ...)
as.integer64(unclass(x), ...)

as.double.integer64 <- function(x, keep.names=FALSE, ...){
  ret <- .Call(C_as_double_integer64, x, double(length(x)))
  if (keep.names)
    names(ret) <- names(x)
  ret
}

as.integer.integer64 <- function(x, ...){
  .Call(C_as_integer_integer64, x, integer(length(x)))
}

as.logical.integer64 <- function(x, ...){
  .Call(C_as_logical_integer64, x, logical(length(x)))
}

as.character.integer64 <- function(x, ...){
  n <- length(x)
  .Call(C_as_character_integer64, x, rep(NA_character_, n))
}

as.bitstring.integer64 <- function(x, ...){
  n <- length(x)
  ret <- .Call(C_as_bitstring_integer64, x, rep(NA_character_, n))
  oldClass(ret) <- 'bitstring'
  ret
}

print.bitstring <- function(x, ...){
  oldClass(x) <- minusclass(class(x), 'bitstring')
  NextMethod(x)
}

as.integer64.bitstring <- function(x, ...){
  ret <- .Call(C_as_integer64_bitstring, x, double(length(x)))
  oldClass(ret) <- "integer64"
  ret
}


# read.table expects S4 as()
methods::setAs("character", "integer64", function(from) as.integer64.character(from))
methods::setAs("integer64", "character", function(from) as.character.integer64(from))

# this is a trick to generate NA_integer64_ for namespace export before
# as.integer64() is available because dll is not loaded
NA_integer64_ <- unserialize(as.raw(c(
  0x58, 0x0a, 0x00, 0x00, 0x00, 0x02, 0x00, 0x03, 0x03, 0x00, 0x00, 0x02, 0x03, 0x00, 0x00, 0x00,
  0x03, 0x0e, 0x00, 0x00, 0x00, 0x01, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x05, 0x63, 0x6c,
  0x61, 0x73, 0x73, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00,
  0x00, 0x00, 0x09, 0x69, 0x6e, 0x74, 0x65, 0x67, 0x65, 0x72, 0x36, 0x34, 0x00, 0x00, 0x00, 0xfe
)))

`length<-.integer64` <- function(x, value){
  cl <- oldClass(x)
  n <- length(x)
  x <- NextMethod()
  oldClass(x) <- cl
  if (value>n)
    x[(n+1L):value] <- 0L
  x
}


format.integer64 <- function(x, justify="right", ...){
  a <- attributes(x)
  x <- as.character(x)
  ret <- format(x, justify=justify, ...)
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

print.integer64 <- function(x, quote=FALSE, ...){
  a <- attributes(x)
  if (length(x)){
    cat("integer64\n")
    ret <- as.character(x)
    a$class <- minusclass(a$class, "integer64")
    attributes(ret) <- a
    print(ret, quote=quote, ...)
  }else{
    cat("integer64(0)\n")
  }
  invisible(x)
}

str.integer64 <- function(object
, vec.len  = strO$vec.len
, give.head = TRUE
, give.length = give.head
, ...
){
  strO <- strOptions()
  vec.len <- 2L*vec.len
  n <- length(object)
  if (n>vec.len)
    object <- object[seq_len(vec.len)]
  cat(if (give.head)paste0("integer64 ", if (give.length && n>1L) paste0("[1:",n,"] ")), paste(as.character(object), collapse=" "),if(n>vec.len)" ...", " \n", sep="")
  invisible()
}

# nocov start
if (FALSE){
    require(microbenchmark)
    require(bit64)
    x <- runif64(10000000L)
    microbenchmark(x[TRUE], times=10L)
    microbenchmark(x[NA], times=10L)
  i <- seq_along(x)
    i[1L] <- NA
    microbenchmark(x[i], times=10L)
    i <- rep(TRUE, length(x))
    i[1L] <- NA
    microbenchmark(x[i], times=10L)
  i <- seq_along(x)
    microbenchmark(x[i], times=10L)
    i <- rep(TRUE, length(x))
    microbenchmark(x[i], times=10L)

}
# nocov end


`[.integer64` <- function(x, i, ...){
    cl <- oldClass(x)
    ret <- NextMethod()
    # Begin NA-handling from Leonardo Silvestri
    if (!missing(i)){
        if (inherits(i, "character")) {
          na_idx <- union(which(!(i %in% names(x))), which(is.na(i)))
          if (length(na_idx))
                ret[na_idx] <- NA_integer64_
        }else{
      ni <- length(i)
      nx <- length(x)
      if (inherits(i, "logical")){
            if (ni>nx){
              na_idx <- is.na(i) | (i & seq_along(i)>nx)
              na_idx <- na_idx[is.na(i) | i]
            }else{
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


`[.integer64` <- function(x, i, ...){
  cl <- oldClass(x)
  ret <- NextMethod()
  # Begin NA-handling from Leonardo Silvestri
  if (!missing(i)){
    if (inherits(i, "character")) {
      na_idx <- union(which(!(i %in% names(x))), which(is.na(i)))
      if (length(na_idx))
        ret[na_idx] <- NA_integer64_
    }else{
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



`[<-.integer64` <- function(x,...,value){
  cl <- oldClass(x)
  value <- as.integer64(value)
  ret <- NextMethod()
  oldClass(ret) <- cl
  ret
}

`[[.integer64` <- function(x,...){
  cl <- oldClass(x)
  ret <- NextMethod()
  oldClass(ret) <- cl
  ret
}

`[[<-.integer64` <- function(x,...,value){
  cl <- oldClass(x)
  value <- as.integer64(value)
  ret <- NextMethod()
  oldClass(ret) <- cl
  ret
}

c.integer64 <-
function (..., recursive = FALSE)
{
    l <- list(...)
    K <- length(l)
    for (k in 1:K){
        if (recursive && is.list(l[[k]])){
            l[[k]] <- do.call(c.integer64, c(l[[k]], list(recursive = TRUE)))
        }else{
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


cbind.integer64 <- function(...){
  l <- list(...)
    K <- length(l)
  for (k in 1:K){
        if (!is.integer64(l[[k]])){
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

rbind.integer64 <- function(...){
  l <- list(...)
    K <- length(l)
  for (k in 1:K){
        if (!is.integer64(l[[k]])){
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

# tenfold runtime if using attr() here instead of setattr()
# as.data.frame.integer64 <- function(x, ...){
  # cl <- oldClass(x)
  # oldClass(x) <- minusclass(cl, "integer64")
  # ret <- as.data.frame(x, ...)
  # k <- length(ret)
  # for (i in 1:k)
    # oldClass(ret[[i]]) <- cl
  # ret
# }
as.data.frame.integer64 <- function(x, ...){
  cl <- oldClass(x)
  on.exit(setattr(x, "class", cl))
  setattr(x, "class", minusclass(cl, "integer64"))
  ret <- as.data.frame(x, ...)
  k <- length(ret)
  for (i in 1:k)
   setattr(ret[[i]], "class", cl)
  ret
}


rep.integer64 <- function(x, ...){
    cl <- oldClass(x)
    ret <- NextMethod()
    oldClass(ret) <- cl
    ret
}

# FIXME no method dispatch for :
`:.integer64` <- function(from, to){
  from <- as.integer64(from)
  to <- as.integer64(to)
  ret <- .Call(C_seq_integer64, from, as.integer64(1L), double(as.integer(to-from+1L)))
  oldClass(ret) <- "integer64"
  ret
}

seq.integer64 <- function(from=NULL, to=NULL, by=NULL, length.out=NULL, along.with=NULL, ...){
    if (is.null(length.out))
      length.out <- length(along.with)
    else
      length.out <- as.integer(length.out)

    if (is.null(by)){
      if (is.null(from) || is.null(to))
        by <- as.integer64(1L)
      else
        by <- as.integer64(if (to < from) -1L else 1L)
    }else{
      by <- as.integer64(by)
      if ((!is.null(from)) && (!is.null(to)) && sign(by)!=(if (to < from) -1L else 1L))
        stop("wrong sign of 'by' argument")
    }

    if (is.null(from)){
      if (length.out && length(to))
        from <- to - (length.out-1L)*by
      else
        from <- as.integer64(1L)
    }else
      from <- as.integer64(from)

    if (!length(to)){
      if (length.out)
        to <- from + (length.out-1L)*by
      else
        stop("not enough informatoin provided")
    }

    if (!length.out){
      length.out <- (to-from) %/% by + 1L
    }

    if (length.out){
      if (length.out==1L)
        return(from)
      else{
        #return(cumsum(c(from, rep(by, length.out-1L))))
        ret <- .Call(C_seq_integer64, from, by, double(as.integer(length.out)))
        oldClass(ret) <- "integer64"
        return(ret)
      }
    }else
      return(integer64())
}


`+.integer64` <- function(e1, e2){
  if (missing(e2))
    return(e1)
  a <- binattr(e1,e2)
  e1 <- as.integer64(e1)
  e2 <- as.integer64(e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1,l2)
  ret <- double(l)
  ret <- .Call(C_plus_integer64, e1, e2, ret)
  a$class <- plusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

`-.integer64` <- function(e1, e2){
  if (missing(e2)){
    e2 <- e1
    e1 <- 0L
  }
  a <- binattr(e1,e2)
  e1 <- as.integer64(e1)
  e2 <- as.integer64(e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1,l2)
  ret <- double(l)
  .Call(C_minus_integer64, e1, e2, ret)
  a$class <- plusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

`%/%.integer64` <- function(e1, e2){
  a <- binattr(e1,e2)
  e1 <- as.integer64(e1)
  e2 <- as.integer64(e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1,l2)
  ret <- double(l)
  .Call(C_intdiv_integer64, e1, e2, ret)
  a$class <- plusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

`%%.integer64` <- function(e1, e2){
  a <- binattr(e1,e2)
  e1 <- as.integer64(e1)
  e2 <- as.integer64(e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1,l2)
  ret <- double(l)
  .Call(C_mod_integer64, e1, e2, ret)
  a$class <- plusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

`*.integer64` <- function(e1, e2){
  a <- binattr(e1,e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1,l2)
  ret <- double(l)
  if (getOption("integer64_semantics", "old") == "old"){
    if (is.double(e2))  # implies !is.integer64(e2)
      ret <- .Call(C_times_integer64_double, as.integer64(e1), e2, ret)
    else
      ret <- .Call(C_times_integer64_integer64, as.integer64(e1), as.integer64(e2), ret)
  }else{
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

`^.integer64` <- function(e1, e2){
  a <- binattr(e1,e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1,l2)
  ret <- double(l)
  if (is.double(e2))  # implies !is.integer64(e2)
    ret <- .Call(C_power_integer64_double, as.integer64(e1), e2, ret)
  else
    ret <- .Call(C_power_integer64_integer64, as.integer64(e1), as.integer64(e2), ret)
  a$class <- plusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

`/.integer64` <- function(e1, e2){
  a <- binattr(e1,e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1,l2)
  ret <- double(l)
  if (getOption("integer64_semantics", "old") == "old"){
    if (is.double(e2))  # implies !is.integer64(e2)
      ret <- .Call(C_divide_integer64_double, as.integer64(e1), e2, ret)
    else
      ret <- .Call(C_divide_integer64_integer64, as.integer64(e1), as.integer64(e2), ret)
  }else{
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


sign.integer64 <- function(x){
  a <- attributes(x)
  ret <- .Call(C_sign_integer64, x, double(length(x)))
  attributes(ret) <- a
  ret
}

abs.integer64 <- function(x){
  a <- attributes(x)
  ret <- .Call(C_abs_integer64, x, double(length(x)))
  attributes(ret) <- a
  ret
}

sqrt.integer64 <- function(x){
  a <- attributes(x)
  ret <- .Call(C_sqrt_integer64, x, double(length(x)))
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

log.integer64 <- function(x, base=NULL){
  a <- attributes(x)
  l.x <- length(x)
  l.base <- length(base)
  l <- if (l.x==0L || (!is.null(base) && l.base==0L)) 0L else max(l.base,l.x)
  ret <- double(l)
  if (is.null(base)){
      .Call(C_log_integer64, x, ret)
  }else if(length(base)==1L){
    .Call(C_logbase_integer64, x, as.double(base), ret)
  }else{
    .Call(C_logvect_integer64, x, as.double(base), ret)
  }
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

log10.integer64 <- function(x){
  a <- attributes(x)
  ret <- .Call(C_log10_integer64, x, double(length(x)))
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

log2.integer64 <- function(x){
  a <- attributes(x)
  ret <- .Call(C_log2_integer64, x, double(length(x)))
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

trunc.integer64 <- function(x, ...) x
floor.integer64 <- ceiling.integer64 <- function(x) x

signif.integer64 <- function(x, digits=6L) x

scale.integer64 <- function(x, center = TRUE, scale = TRUE)
  scale(as.double(x, keep.names=TRUE), center=center, scale=scale)

round.integer64 <- function(x, digits=0L){
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

any.integer64 <- function(..., na.rm = FALSE){
  l <- list(...)
  if (length(l)==1L){
          .Call(C_any_integer64, l[[1L]], na.rm, logical(1L))
  }else{
      any(sapply(l, function(e){
          if (is.integer64(e)){
            .Call(C_any_integer64, e, na.rm, logical(1L))
          }else{
            any(e, na.rm = na.rm)
          }
      }), na.rm = na.rm)
  }
}

all.integer64 <- function(..., na.rm = FALSE){
  l <- list(...)
  if (length(l)==1L){
          .Call(C_all_integer64, l[[1L]], na.rm, logical(1L))
  }else{
      all(sapply(l, function(e){
          if (is.integer64(e)){
            .Call(C_all_integer64, e, na.rm, logical(1L))
          }else{
            all(e, na.rm = na.rm)
          }
      }), na.rm = na.rm)
  }
}

sum.integer64 <- function(..., na.rm = FALSE){
  l <- list(...)
  if (length(l)==1L){
          ret <- .Call(C_sum_integer64, l[[1L]], na.rm, double(1L))
          oldClass(ret) <- "integer64"
          ret
  }else{
      ret <- sapply(l, function(e){
        if (is.integer64(e)){
          .Call(C_sum_integer64, e, na.rm, double(1L))
        }else{
          as.integer64(sum(e, na.rm = na.rm))
        }
      })
    oldClass(ret) <- "integer64"
      sum(ret, na.rm = na.rm)
  }
}

prod.integer64 <- function(..., na.rm = FALSE){
  l <- list(...)
  if (length(l)==1L){
    ret <- .Call(C_prod_integer64, l[[1L]], na.rm, double(1L))
        oldClass(ret) <- "integer64"
        ret
  }else{
      ret <- sapply(l, function(e){
        if (is.integer64(e)){
          .Call(C_prod_integer64, e, na.rm, double(1L))
        }else{
          as.integer64(prod(e, na.rm = na.rm))
        }
      })
      oldClass(ret) <- "integer64"
      prod(ret, na.rm = na.rm)
  }
}

min.integer64 <- function(..., na.rm = FALSE){
  l <- list(...)
  noval <- TRUE
  if (length(l)==1L){
    if (length(l[[1L]]))
      noval <- FALSE
    ret <- .Call(C_min_integer64, l[[1L]], na.rm, double(1L))
    oldClass(ret) <- "integer64"
  }else{
    ret <- sapply(l, function(e){
      if (length(e))
        noval <<- FALSE
      if (is.integer64(e)){
        .Call(C_min_integer64, e, na.rm, double(1L))
      }else{
        as.integer64(min(e, na.rm = na.rm))
      }
    })
    oldClass(ret) <- "integer64"
    ret <- min(ret, na.rm = na.rm)
  }
  if (noval)
    warning("no non-NA value, returning the highest possible integer64 value +9223372036854775807")
  ret
}

max.integer64 <- function(..., na.rm = FALSE){
  l <- list(...)
  noval <- TRUE
  if (length(l)==1L){
    if (length(l[[1L]]))
      noval <- FALSE
      ret <- .Call(C_max_integer64, l[[1L]], na.rm, double(1L))
    oldClass(ret) <- "integer64"
  }else{
    ret <- sapply(l, function(e){
        if (length(e))
          noval <<- FALSE
        if (is.integer64(e)){
          .Call(C_max_integer64, e, na.rm, double(1L))
        }else{
          as.integer64(max(e, na.rm = na.rm))
        }
    })
    oldClass(ret) <- "integer64"
    ret <- max(ret, na.rm = na.rm)
  }
  if (noval)
      warning("no non-NA value, returning the lowest possible integer64 value -9223372036854775807")
  ret
}

range.integer64 <- function(..., na.rm = FALSE, finite = FALSE){
  if (finite)
    na.rm = TRUE
  l <- list(...)
  noval <- TRUE
  if (length(l)==1L){
    if (length(l[[1L]]))
      noval <- FALSE
      ret <- .Call(C_range_integer64, l[[1L]], na.rm, double(2L))
    oldClass(ret) <- "integer64"
  }else{
      ret <- unlist(sapply(l, function(e){
        if (length(e))
          noval <<- FALSE
        if (is.integer64(e)){
          .Call(C_range_integer64, e, na.rm, double(2L))
        }else{
          as.integer64(range(e, na.rm = na.rm))
        }
      }))
      oldClass(ret) <- "integer64"
      ret <- range(ret, na.rm = na.rm)
  }
  if (noval)
    warning("no non-NA value, returning c(+9223372036854775807, -9223372036854775807)")
  ret
}

lim.integer64 <- function(){
  ret <- .Call(C_lim_integer64, double(2L))
    oldClass(ret) <- "integer64"
    ret
}

diff.integer64 <- function(x, lag=1L, differences=1L, ...){
  lag <- as.integer(lag)
  n <- length(x)
  d <- differences <- as.integer(differences)
  while(d > 0L){
    n <- n - lag
    if (n <= 0L){
      ret <- double()
      break
    }
    # not assigning ret<-.Call in the following is intended because faster
    if (d==differences){
      ret <- double(n)
      .Call(C_diff_integer64, x, as.integer64(lag), as.integer64(n), ret)
    }else{
      .Call(C_diff_integer64, ret, as.integer64(lag), as.integer64(n), ret)
    }
    d <- d - 1L
  }
  # length of ret is only change once here
  length(ret) <- n
  oldClass(ret) <- "integer64"
  ret
}

cummin.integer64 <- function(x){
  ret <- .Call(C_cummin_integer64, x, double(length(x)))
  oldClass(ret) <- "integer64"
  ret
}
cummax.integer64 <- function(x){
  ret <- .Call(C_cummax_integer64, x, double(length(x)))
  oldClass(ret) <- "integer64"
  ret
}

cumsum.integer64 <- function(x){
  ret <- .Call(C_cumsum_integer64, x, double(length(x)))
  oldClass(ret) <- "integer64"
  ret
}

cumprod.integer64 <- function(x){
  ret <- .Call(C_cumprod_integer64, x, double(length(x)))
  oldClass(ret) <- "integer64"
  ret
}



is.na.integer64 <- function(x){
  a <- attributes(x)
  ret <- .Call(C_isna_integer64, x, logical(length(x)))
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

is.finite.integer64 <- function(x)!is.na(x)
is.infinite.integer64 <- function(x)rep(FALSE, length(x))
is.nan.integer64 <- function(x)rep(FALSE, length(x))


`==.integer64` <- function(e1, e2){
  a <- binattr(e1,e2)
  e1 <- as.integer64(e1)
  e2 <- as.integer64(e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1,l2)
  ret <- logical(l)
  .Call(C_EQ_integer64, e1, e2, ret)
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

`!=.integer64` <- function(e1, e2){
  a <- binattr(e1,e2)
  e1 <- as.integer64(e1)
  e2 <- as.integer64(e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1,l2)
  ret <- logical(l)
  .Call(C_NE_integer64, e1, e2, ret)
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

`<.integer64` <- function(e1, e2){
  a <- binattr(e1,e2)
  e1 <- as.integer64(e1)
  e2 <- as.integer64(e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1,l2)
  ret <- logical(l)
  .Call(C_LT_integer64, e1, e2, ret)
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

`<=.integer64` <- function(e1, e2){
  a <- binattr(e1,e2)
  e1 <- as.integer64(e1)
  e2 <- as.integer64(e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1,l2)
  ret <- logical(l)
  .Call(C_LE_integer64, e1, e2, ret)
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

`>.integer64` <- function(e1, e2){
  a <- binattr(e1,e2)
  e1 <- as.integer64(e1)
  e2 <- as.integer64(e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1,l2)
  ret <- logical(l)
  .Call(C_GT_integer64, e1, e2, ret)
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

`>=.integer64` <- function(e1, e2){
  a <- binattr(e1,e2)
  e1 <- as.integer64(e1)
  e2 <- as.integer64(e2)
  l1 <- length(e1)
  l2 <- length(e2)
  l <- if (l1 == 0L || l2 == 0L) 0L else max(l1,l2)
  ret <- logical(l)
  .Call(C_GE_integer64, e1, e2, ret)
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

`&.integer64` <- function(e1, e2){
  a <- binattr(e1,e2)
  ret <- as.logical(e1) & as.logical(e2)
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

`|.integer64` <- function(e1, e2){
  a <- binattr(e1,e2)
  ret <- as.logical(e1) | as.logical(e2)
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

xor.integer64 <- function(x, y){
  a <- binattr(x,y)
  ret <- as.logical(x) != as.logical(y)
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}


`!.integer64` <- function(x){
  a <- attributes(x)
  ret <- !as.logical(x)
  a$class <- minusclass(a$class, "integer64")
  attributes(ret) <- a
  ret
}

# as.vector.integer64 removed as requested by the CRAN maintainer
# as.vector.integer64 <- function(x, mode="any"){
  # ret <- NextMethod()
  # if (mode=="any")
    # oldClass(ret) <- "integer64"
  # ret
# }

# bug in R does not dispatch
is.vector.integer64 <- function(x, mode="any"){
  cl <- minusclass(oldClass(x), "integer64")
  a <- attributes(x)
  a$class <- NULL
  a$names <- NULL
  if (is.na(match(mode, c("any","integer64"))) || length(cl) || length(a) )
    FALSE
  else
    TRUE
}


as.list.integer64 <- function (x, ...){
  ret <- NextMethod("as.list", x, ...)
  .Call(C_as_list_integer64, ret)
}
