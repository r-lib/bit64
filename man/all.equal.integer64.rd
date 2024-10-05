\name{all.equal.integer64}
\alias{all.equal.integer64}
\title{
   Test if two integer64 vectors are all.equal
}
\description{
   A utility to compare integer64 objects 'x' and 'y' testing for ‘near equality’, see \code{\link{all.equal}}.
}
\usage{
  \method{all.equal}{integer64}(
  target
, current
, tolerance = sqrt(.Machine$double.eps)
, scale = NULL
, countEQ = FALSE
, formatFUN = function(err, what) format(err)
, ...
, check.attributes = TRUE
)
}
\arguments{
  \item{target}{ a vector of 'integer64' or an object that can be coerced with \code{\link{as.integer64}} }
  \item{current}{ a vector of 'integer64' or an object that can be coerced with \code{\link{as.integer64}} }
  \item{tolerance}{numeric \eqn{\ge} 0.  Differences smaller than
    \code{tolerance} are not reported.  The default value is close to
    \code{1.5e-8}.}
  \item{scale}{\code{NULL} or numeric > 0, typically of length 1 or
    \code{length(target)}.  See \sQuote{Details}.}
  \item{countEQ}{logical indicating if the \code{target == current}
    cases should be counted when computing the mean (absolute or
    relative) differences.  The default, \code{FALSE} may seem
    misleading in cases where \code{target} and \code{current} only
    differ in a few places; see the extensive example.}
  \item{formatFUN}{a \code{\link{function}} of two arguments,
    \code{err}, the relative, absolute or scaled error, and
    \code{what}, a character string indicating the \emph{kind} of error;
    maybe used, e.g., to format relative and absolute errors differently.}
  \item{\dots}{further arguments are ignored}
  \item{check.attributes}{logical indicating if the
    \code{\link{attributes}} of \code{target} and \code{current}
    (other than the names) should be compared.}
}
\value{
  Either ‘TRUE’ (‘NULL’ for ‘attr.all.equal’) or a vector of ‘mode’
  ‘"character"’ describing the differences between ‘target’ and
  ‘current’.
}
\details{
   In \code{\link{all.equal.numeric}} the type \code{integer} is treated as a proper subset of \code{double}
   i.e. does not complain about comparing \code{integer} with \code{double}.
   Following this logic \code{all.equal.integer64} treats \code{integer} as a proper subset of \code{integer64}
   and does not complain about comparing \code{integer} with \code{integer64}. \code{double} also compares without warning
   as long as the values are within \code{\link{lim.integer64}}, if \code{double} are bigger \code{all.equal.integer64}
   complains about the \code{all.equal.integer64 overflow warning}. For further details see \code{\link{all.equal}}.
}
\note{
   \code{\link{all.equal}} only dispatches to this method if the first argument is \code{integer64},
   calling \code{\link{all.equal}} with a \code{non-integer64} first and a \code{integer64} second argument
   gives undefined behavior!
}
\author{
  Leonardo Silvestri (for package nanotime)
}
\seealso{
  \code{\link{all.equal}}
}
\examples{
  all.equal(as.integer64(1:10), as.integer64(0:9))
  all.equal(as.integer64(1:10), as.integer(1:10))
  all.equal(as.integer64(1:10), as.double(1:10))
  all.equal(as.integer64(1), as.double(1e300))
}
