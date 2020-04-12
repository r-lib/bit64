\name{cumsum.integer64}
\alias{cummin.integer64}
\alias{cummax.integer64}
\alias{cumsum.integer64}
\alias{cumprod.integer64}
\alias{diff.integer64}
\title{
   Cumulative Sums, Products, Extremes and lagged differences
}
\description{
  Cumulative Sums, Products, Extremes and lagged differences
}
\usage{
\method{cummin}{integer64}(x)
\method{cummax}{integer64}(x)
\method{cumsum}{integer64}(x)
\method{cumprod}{integer64}(x)
\method{diff}{integer64}(x, lag = 1L, differences = 1L, \dots)
}
\arguments{
  \item{x}{ an atomic vector of class 'integer64'}
  \item{lag}{ see \code{\link{diff}} }
  \item{differences}{ see \code{\link{diff}} }
  \item{\dots}{ ignored }
}
\value{
  \code{\link{cummin}}, \code{\link{cummax}} , \code{\link{cumsum}} and \code{\link{cumprod}} 
     return a integer64 vector of the same length as their input\cr
  \code{\link{diff}} returns a integer64 vector shorter by \code{lag*differences} elements \cr
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\keyword{ classes }
\keyword{ manip }
\seealso{ \code{\link{sum.integer64}} \code{\link{integer64}}  }
\examples{
  cumsum(rep(as.integer64(1), 12))
  diff(as.integer64(c(0,1:12)))
  cumsum(as.integer64(c(0, 1:12)))
  diff(cumsum(as.integer64(c(0,0,1:12))), differences=2)
}
