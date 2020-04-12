\name{tiepos}
\alias{tiepos}
\alias{tiepos.integer64}
\title{Extract Positions of Tied Elements}
\description{
  \code{tiepos} returns the positions of those elements that participate in ties.
}
\usage{
tiepos(x, \dots)
\method{tiepos}{integer64}(x, nties = NULL, method = NULL, \dots)
}
\arguments{
  \item{x}{a vector or a data frame or an array or \code{NULL}.}
  \item{nties}{
	NULL or the number of tied values (including NA). Providing \code{nties} can speed-up when \code{x} has no cache. Note that a wrong nties can cause undefined behaviour up to a crash.
}
  \item{method}{
	NULL for automatic method selection or a suitable low-level method, see details
}
  \item{\dots}{ignored}
}
\details{
  This function automatically chooses from several low-level functions considering the size of \code{x} and the availability of a cache. 
  Suitable methods are \code{\link{sortordertie}} (fast ordering) 
and \code{\link{ordertie}} (memory saving ordering).
}
\value{
  an integer vector of positions
}
\author{
	Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\seealso{
  \code{\link{rank.integer64}} for possibly tied ranks and \code{\link{unipos.integer64}} for positions of unique values.
}
\examples{
x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
tiepos(x)

stopifnot(identical(tiepos(x),  (1:length(x))[duplicated(x) | rev(duplicated(rev(x)))]))
}
\keyword{manip}
\keyword{univar}
