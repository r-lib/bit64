\name{duplicated.integer64}
\alias{duplicated.integer64}
\title{Determine Duplicate Elements of integer64}
\description{
  \code{duplicated()} determines which elements of a vector or data frame are duplicates
  of elements with smaller subscripts, and returns a logical vector
  indicating which elements (rows) are duplicates.
}
\usage{
\method{duplicated}{integer64}(x, incomparables = FALSE, nunique = NULL, method = NULL, \dots)
}
\arguments{
  \item{x}{a vector or a data frame or an array or \code{NULL}.}
  \item{incomparables}{ignored}
  \item{nunique}{
	NULL or the number of unique values (including NA). Providing \code{nunique} can speed-up matching when \code{x} has no cache. Note that a wrong nunique can cause undefined behaviour up to a crash.
}
  \item{method}{
	NULL for automatic method selection or a suitable low-level method, see details
}
  \item{\dots}{ignored}
}
\details{
  This function automatically chooses from several low-level functions considering the size of \code{x} and the availability of a cache. 

  Suitable methods are \code{\link{hashdup}} (hashing), \code{\link{sortorderdup}} (fast ordering) and \code{\link{orderdup}} (memory saving ordering).
}
\value{
    \code{duplicated()}: a logical vector of the same length as \code{x}.  
}
\author{
	Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\seealso{ \code{\link{duplicated}}, \code{\link{unique.integer64}}  }
\examples{
x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
duplicated(x)

stopifnot(identical(duplicated(x),  duplicated(as.integer(x))))
}
\keyword{logic}
\keyword{manip}

