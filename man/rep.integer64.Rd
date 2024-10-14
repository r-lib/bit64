\name{rep.integer64}
\alias{rep.integer64}
\title{
   Replicate elements of integer64 vectors
}
\usage{
\method{rep}{integer64}(x, ...)
}
\arguments{
  \item{x}{ a vector of 'integer64' to be replicated }

  \item{...}{ further arguments passed to \code{\link[=NextMethod]{NextMethod()}} }
}
\value{
  \code{\link[=rep]{rep()}} returns a integer64 vector
}
\description{
  Replicate elements of integer64 vectors
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\examples{
  rep(as.integer64(1:2), 6)
  rep(as.integer64(1:2), c(6,6))
  rep(as.integer64(1:2), length.out=6)
}
\seealso{ \code{\link[=c.integer64]{c.integer64()}} \code{\link[=rep.integer64]{rep.integer64()}}
          \code{\link[=as.data.frame.integer64]{as.data.frame.integer64()}} \code{\link{integer64}}
}
\keyword{ classes }
\keyword{ manip }
