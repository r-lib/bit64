\name{cache}
\alias{cache}
\alias{newcache}
\alias{jamcache}
\alias{setcache}
\alias{getcache}
\alias{remcache}
\alias{print.cache}
\alias{still.identical}
\title{
	Atomic Caching
}
\description{
	Functions for caching results attached to atomic objects
}
\usage{
newcache(x)
jamcache(x)
cache(x)
setcache(x, which, value)
getcache(x, which)
remcache(x)
\method{print}{cache}(x, all.names = FALSE, pattern, \dots)
still.identical(x, y)
}
\arguments{
  \item{x}{
  an integer64 vector (or a cache object in case of \code{print.cache})
}
  \item{y}{
  an integer64 vector
}
  \item{which}{
  A character naming the object to be retrieved from the cache or to be stored in the cache
}
  \item{value}{
  An object to be stored in the cache 
}
  \item{all.names}{
  passed to \code{\link{ls}} when listing the cache content
}
  \item{pattern}{
  passed to \code{\link{ls}} when listing the cache content
}
  \item{\dots}{
	ignored
}
}
\details{
	A \code{cache} is an \code{link{environment}} attached to an atomic object with the \code{link{attrib}} name 'cache'. 
	It contains at least a reference to the atomic object that carries the cache. 
	This is used when accessing the cache to detect whether the object carrying the cache has been modified meanwhile.
	Function \code{still.identical(x,y)} checks whether the objects \code{x} and \code{y} \cr
	Function \code{newcache(x)} creates a new cache referencing  \code{x} \cr
	Function \code{jamcache(x)} forces \code{x} to have a cache \cr
	Function \code{cache(x)} returns the cache attached to \code{x} if it is not found to be outdated \cr
	Function \code{setcache(x, which, value)} assigns a value into the cache of \code{x} \cr
	Function \code{getcache(x, which)} gets cache value 'which' from \code{x} \cr
	Function \code{remcache} removes the cache from \code{x} \cr
}
\value{
	see details
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\seealso{
	Functions that get and set small cache-content automatically when a cache is present: \code{\link{na.count}}, \code{\link{nvalid}}, \code{\link{is.sorted}}, \code{\link{nunique}} and \code{\link{nties}} \cr
	Setting big caches with a relevant memory footprint requires a conscious decision of the user: \code{\link{hashcache}}, \code{\link{sortcache}}, \code{\link{ordercache}} and \code{\link{sortordercache}} \cr
	Functions that use big caches: \code{\link{match.integer64}}, \code{\link{\%in\%.integer64}}, \code{\link{duplicated.integer64}}, \code{\link{unique.integer64}}, \code{\link{unipos}}, \code{\link{table.integer64}}, \code{\link{as.factor.integer64}}, \code{\link{as.ordered.integer64}}, \code{\link{keypos}}, \code{\link{tiepos}}, \code{\link{rank.integer64}}, \code{\link{prank}}, \code{\link{qtile}}, \code{\link{quantile.integer64}}, \code{\link{median.integer64}} and \code{\link{summary.integer64}} \cr
}
\examples{
	x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
	y <- x
	still.identical(x,y)
	y[1] <- NA
	still.identical(x,y)
	mycache <- newcache(x)
	ls(mycache)
	mycache
	rm(mycache)
	jamcache(x)
	cache(x)
	x[1] <- NA
	cache(x)
	getcache(x, "abc")
	setcache(x, "abc", 1)
	getcache(x, "abc")
	remcache(x)
	cache(x)
}
\keyword{ environment }
