\name{rank.integer64}
\alias{rank.integer64}
\title{Sample Ranks from integer64}
\description{
  Returns the sample ranks of the values in a vector.  Ties (i.e., equal
  values) are averaged and missing values propagated.
}
\usage{
	\method{rank}{integer64}(x, method = NULL, \dots)
}
\arguments{
  \item{x}{a integer64 vector}
  \item{method}{
	NULL for automatic method selection or a suitable low-level method, see details
}
  \item{\dots}{ignored}
}
\details{
  This function automatically chooses from several low-level functions considering the size of \code{x} and the availability of a cache. 
  Suitable methods are \code{\link{sortorderrnk}} (fast ordering) 
and \code{\link{orderrnk}} (memory saving ordering).
}
\value{
  A numeric vector of the same length as \code{x}.
}
\author{
	Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\seealso{
  \code{\link{order.integer64}}, \code{\link{rank}} and \code{\link{prank}} for percent rank.
}
\examples{
x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
rank.integer64(x)

stopifnot(identical(rank.integer64(x),  rank(as.integer(x)
, na.last="keep", ties.method = "average")))
}
\keyword{univar}
