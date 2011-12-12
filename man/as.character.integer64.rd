\name{as.character.integer64}
\alias{as.character.integer64}
\alias{as.double.integer64}
\alias{as.integer.integer64}
\alias{as.logical.integer64}
\alias{as.bitstring}
\alias{as.bitstring.integer64}
\title{
   Coerce from integer64
}
\description{
  Methods to coerce integer64 to other atomic types. 
  'as.bitstring' coerces to a human-readable bit representation (strings of zeroes and ones). 
  The methods \code{\link{format}}, \code{\link{as.character}}, \code{\link{as.double}},
  \code{\link{as.logical}}, \code{\link{as.integer}} do what you would expect.
}
\usage{
 as.bitstring(x, \dots)
 \method{as.bitstring}{integer64}(x, \dots)
 \method{as.character}{integer64}(x, \dots)
 \method{as.double}{integer64}(x, \dots)
 \method{as.integer}{integer64}(x, \dots)
 \method{as.logical}{integer64}(x, \dots)
}
\arguments{
  \item{x}{ an integer64 vector }
  \item{\dots}{ further arguments to the \code{\link{NextMethod}} }
}
\value{
  \code{as.bitstring} returns a string of . \cr
  The other methods return atomic vectors of the expected types
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\keyword{ classes }
\keyword{ manip }
\seealso{ \code{\link{as.integer64.character}} \code{\link{integer64}}  }
\examples{
  as.character(lim.integer64())
  as.bitstring(lim.integer64())
}
