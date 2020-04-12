\name{sort.integer64}
\alias{sort.integer64}
\alias{order.integer64}
\title{
   High-level intger64 methods for sorting and ordering
}
\description{
  Fast high-level methods for sorting and ordering. 
  These are wrappers to \code{\link{ramsort}} and friends and do not modify their arguments.
}
\usage{
\method{sort}{integer64}(x, decreasing = FALSE, has.na = TRUE, na.last = TRUE, stable = TRUE
, optimize = c("time", "memory"), VERBOSE = FALSE, \dots)
\method{order}{integer64}(\dots, na.last = TRUE, decreasing = FALSE, has.na = TRUE, stable = TRUE
, optimize = c("time", "memory"), VERBOSE = FALSE)
}
\arguments{
  \item{x}{ a vector to be sorted by \code{\link{ramsort}} and \code{\link{ramsortorder}}, i.e. the output of  \code{\link{sort}} }
  \item{has.na}{
boolean scalar defining whether the input vector might contain \code{NA}s. If we know we don't have NAs, this may speed-up.
\emph{Note} that you risk a crash if there are unexpected \code{NA}s with \code{has.na=FALSE}
}
  \item{na.last}{
boolean scalar telling ramsort whether to sort \code{NA}s last or first.
\emph{Note} that 'boolean' means that there is no third option \code{NA} as in \code{\link{sort}}
}
  \item{decreasing}{
boolean scalar telling ramsort whether to sort increasing or decreasing
}
  \item{stable}{
boolean scalar defining whether stable sorting is needed. Allowing non-stable may speed-up.
}
  \item{optimize}{
by default ramsort optimizes for 'time' which requires more RAM,
set to 'memory' to minimize RAM requirements and sacrifice speed
}
  \item{VERBOSE}{
  cat some info about chosen method
}
  \item{\dots}{ further arguments, passed from generics, ignored in methods }
}
\details{
 see \code{\link{sort}} and \code{\link{order}}
}
\value{
  \code{sort} returns the sorted vector and \code{vector} returns the order positions. 
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\keyword{ programming }
\keyword{ manip }
\seealso{ \code{\link[=sort.integer64]{sort}}, \code{\link{sortcache}} }
\examples{
  x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
  x
  sort(x)
  message("the following has default optimize='time' which is faster but requires more RAM
, this calls 'ramorder'")
  order.integer64(x)
  message("slower with less RAM, this calls 'ramsortorder'")
  order.integer64(x, optimize="memory")
}
