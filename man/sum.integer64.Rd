\name{sum.integer64}
\alias{all.integer64}
\alias{any.integer64}
\alias{min.integer64}
\alias{max.integer64}
\alias{range.integer64}
\alias{lim.integer64}
\alias{sum.integer64}
\alias{prod.integer64}
\title{
   Summary functions for integer64 vectors
}
\usage{
\method{all}{integer64}(..., na.rm = FALSE)

\method{any}{integer64}(..., na.rm = FALSE)

\method{min}{integer64}(..., na.rm = FALSE)

\method{max}{integer64}(..., na.rm = FALSE)

\method{range}{integer64}(..., na.rm = FALSE, finite = FALSE)

lim.integer64()

\method{sum}{integer64}(..., na.rm = FALSE)

\method{prod}{integer64}(..., na.rm = FALSE)
}
\arguments{
  \item{...}{ atomic vectors of class 'integer64'}

  \item{na.rm}{ logical scalar indicating whether to ignore NAs }

  \item{finite}{ logical scalar indicating whether to ignore NAs (just for compatibility with \code{\link{range.default}}) }
}
\value{
  \code{\link{all}} and \code{\link{any}} return a logical scalar\cr
  \code{\link{range}} returns a integer64 vector with two elements\cr
  \code{\link{min}}, \code{\link{max}}, \code{\link{sum}} and \code{\link{prod}} return a integer64 scalar
}
\description{
  Summary functions for integer64 vectors.
  Function 'range' without arguments returns the smallest and largest value of the 'integer64' class.
}
\details{
  The numerical summary methods always return \code{integer64}.
  Therefor the methods for \code{min},\code{max} and \code{range} do not return \code{+Inf,-Inf}
  on empty arguments, but \code{+9223372036854775807, -9223372036854775807} (in this sequence).
  The same is true if only  \code{NA}s are submitted with argument \code{na.rm=TRUE}.
 \cr
  \code{lim.integer64} returns these limits in proper order \code{-9223372036854775807, +9223372036854775807} and without a \code{\link{warning}}.
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\examples{
  lim.integer64()
  range(as.integer64(1:12))
}
\seealso{ \code{\link{mean.integer64}} \code{\link{cumsum.integer64}} \code{\link{integer64}}  }
\keyword{ classes }
\keyword{ manip }
