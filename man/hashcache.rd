\name{hashcache}
\alias{hashcache}
\alias{sortcache}
\alias{sortordercache}
\alias{ordercache}
\title{
		Big caching of hashing, sorting, ordering
}
\description{
	Functions to create cache that accelerates many operations
}
\usage{
hashcache(x, nunique=NULL, \dots)
sortcache(x, has.na = NULL)
sortordercache(x, has.na = NULL, stable = NULL)
ordercache(x, has.na = NULL, stable = NULL, optimize = "time")
}
\arguments{
  \item{x}{
		an atomic vector (note that currently only integer64 is supported)
}
  \item{nunique}{ giving \emph{correct} number of unique elements can help reducing the size of the hashmap }
  \item{has.na}{
boolean scalar defining whether the input vector might contain \code{NA}s. If we know we don't have NAs, this may speed-up.
\emph{Note} that you risk a crash if there are unexpected \code{NA}s with \code{has.na=FALSE}
}
  \item{stable}{
boolean scalar defining whether stable sorting is needed. Allowing non-stable may speed-up.
}
  \item{optimize}{
by default ramsort optimizes for 'time' which requires more RAM,
set to 'memory' to minimize RAM requirements and sacrifice speed
}
  \item{\dots}{
		passed to \code{\link{hashmap}}
}
}
\details{
	The result of relative expensive operations \code{\link{hashmap}}, \code{\link{ramsort}}, \code{\link{ramsortorder}} and \code{\link{ramorder}} can be stored in a cache in order to avoid multiple excutions. Unless in very specific situations, the recommended method is \code{hashsortorder} only.
}
\note{
  Note that we consider storing the big results from sorting and/or ordering as a relevant side-effect, 
and therefore storing them in the cache should require a conscious decision of the user.
}
\value{
	\code{x} with a \code{\link{cache}} that contains the result of the expensive operations, possible together with small derived information (such as \code{\link{nunique.integer64}}) and previously cached results.
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\seealso{
	\code{\link{cache}} for caching functions and \code{\link{nunique}} for methods bennefitting from small caches
}
\examples{
	x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
 sortordercache(x)
}
\keyword{ environment }
