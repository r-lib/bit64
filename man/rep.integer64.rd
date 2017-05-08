\name{rep.integer64}
\alias{rep.integer64}
\title{
   Replicate elements of integer64 vectors
}
\description{
  Replicate elements of integer64 vectors
}
\usage{
\method{rep}{integer64}(x, \dots)
}
\arguments{
  \item{x}{ a vector of 'integer64' to be replicated }
  \item{\dots}{ further arguments passed to \code{\link{NextMethod}} }
}
\value{
  \code{\link{rep}} returns a integer64 vector
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\keyword{ classes }
\keyword{ manip }
\seealso{ \code{\link{c.integer64}} \code{\link{rep.integer64}} 
          \code{\link{as.data.frame.integer64}} \code{\link{integer64}}  
}
\examples{
  rep(as.integer64(1:2), 6)
  rep(as.integer64(1:2), c(6,6))
  rep(as.integer64(1:2), length.out=6)
}
