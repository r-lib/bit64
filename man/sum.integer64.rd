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
\description{
  Summary functions for integer64 vectors. 
  Function 'range' without arguments returns the smallest and largest value of the 'integer64' class.
}
\usage{
\method{all}{integer64}(\dots, na.rm = FALSE)
\method{any}{integer64}(\dots, na.rm = FALSE)
\method{min}{integer64}(\dots, na.rm = FALSE)
\method{max}{integer64}(\dots, na.rm = FALSE)
\method{range}{integer64}(\dots, na.rm = FALSE)
lim.integer64()
\method{sum}{integer64}(\dots, na.rm = FALSE)
\method{prod}{integer64}(\dots, na.rm = FALSE)
}
\arguments{
  \item{\dots}{ atomic vectors of class 'integer64'}
  \item{na.rm}{ logical scalar indicating whether to ignore NAs }
}
\details{
  The numerical summary methods always return \code{integer64}. 
  Therefor the methods for \code{min},\code{max} and \code{range} do not return \code{+Inf,-Inf}
  on empty arguments, but \code{+9223372036854775807, -9223372036854775807} (in this sequence).
  The same is true if only  \code{NA}s are submitted with argument \code{na.rm=TRUE}. 
 \cr
  \code{lim.integer64} returns these limits in proper order \code{-9223372036854775807, +9223372036854775807} and without a \code{\link{warning}}.
}
\value{
  \code{\link{all}} and \code{\link{any}} return a logical scalar\cr
  \code{\link{range}} returns a integer64 vector with two elements\cr
  \code{\link{min}}, \code{\link{max}}, \code{\link{sum}} and \code{\link{prod}} return a integer64 scalar
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\keyword{ classes }
\keyword{ manip }
\seealso{ \code{\link{mean.integer64}} \code{\link{cumsum.integer64}} \code{\link{integer64}}  }
\examples{
  lim.integer64()
  range(as.integer64(1:12))
}
