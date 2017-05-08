\name{keypos}
\alias{keypos}
\alias{keypos.integer64}
\title{Extract Positions in redundant dimension table}
\description{
  \code{keypos} returns the positions of the (fact table) elements that participate in their sorted unique subset (dimension table)
}
\usage{
keypos(x, \dots)
\method{keypos}{integer64}(x, method = NULL, \dots)
}
\arguments{
  \item{x}{a vector or a data frame or an array or \code{NULL}.}
  \item{method}{
	NULL for automatic method selection or a suitable low-level method, see details
}
  \item{\dots}{ignored}
}
\details{
  NAs are sorted first in the dimension table, see \code{\link{ramorder.integer64}}.
  \cr
  This function automatically chooses from several low-level functions considering the size of \code{x} and the availability of a cache. 
  Suitable methods are \code{\link{sortorderkey}} (fast ordering) 
and \code{\link{orderkey}} (memory saving ordering).
}
\value{
  an integer vector of the same length as code{x} containing positions relativ to code{sort(unique(x), na.last=FALSE)}
}
\author{
	Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\seealso{
  \code{\link{unique.integer64}} for the unique subset and \code{\link{match.integer64}} for finding positions in a different vector.
}
\examples{
x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
keypos(x)

stopifnot(identical(keypos(x),  match.integer64(x, sort(unique(x), na.last=FALSE))))
}
\keyword{manip}
\keyword{univar}
