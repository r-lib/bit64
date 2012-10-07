\name{as.integer64.character}
\alias{as.integer64}
\alias{as.integer64.integer64}
\alias{as.integer64.NULL}
\alias{as.integer64.character}
\alias{as.integer64.double}
\alias{as.integer64.integer}
\alias{as.integer64.logical}
\alias{as.integer64.factor}
\title{
   Coerce to integer64
}
\description{
  Methods to coerce from other atomic types to integer64. 
}
\usage{
 as.integer64(x, \dots)
 \method{as.integer64}{integer64}(x, \dots)
 \method{as.integer64}{NULL}(x, \dots)
 \method{as.integer64}{character}(x, \dots)
 \method{as.integer64}{double}(x, \dots)
 \method{as.integer64}{integer}(x, \dots)
 \method{as.integer64}{logical}(x, \dots)
 \method{as.integer64}{factor}(x, \dots)
}
\arguments{
  \item{x}{ an atomic vector }
  \item{\dots}{ further arguments to the \code{\link{NextMethod}} }
}
\details{
  \code{as.integer64.character} is realized using C function \code{strtoll} which does not support scientific notation. 
  Instead of '1e6' use '1000000'.
}
\value{
  The other methods return atomic vectors of the expected types
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\keyword{ classes }
\keyword{ manip }
\seealso{ \code{\link{as.character.integer64}} \code{\link{integer64}}  }
\examples{
  as.integer64(as.character(lim.integer64()))
}
