\name{as.character.integer64}
\alias{as.character.integer64}
\alias{as.double.integer64}
\alias{as.integer.integer64}
\alias{as.logical.integer64}
\alias{as.bitstring}
\alias{print.bitstring}
\alias{as.bitstring.integer64}
\alias{as.list.integer64}
\title{
   Coerce from integer64
}
\usage{
 as.bitstring(x, ...)

 \method{as.bitstring}{integer64}(x, ...)

 \method{print}{bitstring}(x, ...)

 \method{as.character}{integer64}(x, ...)

 \method{as.double}{integer64}(x, keep.names = FALSE, ...)

 \method{as.integer}{integer64}(x, ...)

 \method{as.logical}{integer64}(x, ...)

 \method{as.list}{integer64}(x, ...)
}
\arguments{
  \item{x}{ an integer64 vector }
  \item{keep.names}{ FALSE, set to TRUE to keep a names vector }
  \item{...}{ further arguments to the \code{\link{NextMethod}} }
}
\value{
  \code{as.bitstring} returns a string of class 'bitstring'. \cr
  The other methods return atomic vectors of the expected types
}
\description{
  Methods to coerce integer64 to other atomic types.
  'as.bitstring' coerces to a human-readable bit representation (strings of zeroes and ones).
  The methods \code{\link{format}}, \code{\link{as.character}}, \code{\link{as.double}},
  \code{\link{as.logical}}, \code{\link{as.integer}} do what you would expect.
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\examples{
  as.character(lim.integer64())
  as.bitstring(lim.integer64())
  as.bitstring(as.integer64(c(
   -2,-1,NA,0:2
  )))
}
\seealso{ \code{\link{as.integer64.character}} \code{\link{integer64}}  }
\keyword{ classes }
\keyword{ manip }
