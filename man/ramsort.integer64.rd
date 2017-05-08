\name{ramsort.integer64}
\alias{ramsort.integer64}
\alias{shellsort.integer64}
\alias{quicksort.integer64}
\alias{mergesort.integer64}
\alias{radixsort.integer64}
\alias{ramorder.integer64}
\alias{shellorder.integer64}
\alias{quickorder.integer64}
\alias{mergeorder.integer64}
\alias{radixorder.integer64}
\alias{ramsortorder.integer64}
\alias{shellsortorder.integer64}
\alias{quicksortorder.integer64}
\alias{mergesortorder.integer64}
\alias{radixsortorder.integer64}
\title{
   Low-level intger64 methods for in-RAM sorting and ordering
}
\description{
  Fast low-level methods for sorting and ordering. 
  The \code{..sortorder} methods do sorting and ordering at once, which requires more RAM than ordering but is (almost) as fast as as sorting.
}
\note{
 Note that these methods purposely violate the functional programming paradigm: they are called for the side-effect of changing some of their arguments.
 The \code{sort}-methods change \code{x}, the \code{order}-methods change \code{i}, and the \code{sortoder}-methods change both \code{x} and \code{i}
}
\usage{
\method{shellsort}{integer64}(x, has.na=TRUE, na.last=FALSE, decreasing=FALSE, \dots)
\method{shellsortorder}{integer64}(x, i, has.na=TRUE, na.last=FALSE, decreasing=FALSE, \dots)
\method{shellorder}{integer64}(x, i, has.na=TRUE, na.last=FALSE, decreasing=FALSE, \dots)
\method{mergesort}{integer64}(x, has.na=TRUE, na.last=FALSE, decreasing=FALSE, \dots)
\method{mergeorder}{integer64}(x, i, has.na=TRUE, na.last=FALSE, decreasing=FALSE, \dots)
\method{mergesortorder}{integer64}(x, i, has.na=TRUE, na.last=FALSE, decreasing=FALSE, \dots)
\method{quicksort}{integer64}(x, has.na=TRUE, na.last=FALSE, decreasing=FALSE
, restlevel=floor(1.5*log2(length(x))), \dots)
\method{quicksortorder}{integer64}(x, i, has.na=TRUE, na.last=FALSE, decreasing=FALSE
, restlevel=floor(1.5*log2(length(x))), \dots)
\method{quickorder}{integer64}(x, i, has.na=TRUE, na.last=FALSE, decreasing=FALSE
, restlevel=floor(1.5*log2(length(x))), \dots)
\method{radixsort}{integer64}(x, has.na=TRUE, na.last=FALSE, decreasing=FALSE, radixbits=8L, \dots)
\method{radixsortorder}{integer64}(x, i, has.na=TRUE, na.last=FALSE, decreasing=FALSE, radixbits=8L, \dots)
\method{radixorder}{integer64}(x, i, has.na=TRUE, na.last=FALSE, decreasing=FALSE, radixbits=8L, \dots)
\method{ramsort}{integer64}(x, has.na = TRUE, na.last=FALSE, decreasing = FALSE, stable = TRUE
, optimize = c("time", "memory"), VERBOSE = FALSE, \dots)
\method{ramsortorder}{integer64}(x, i, has.na = TRUE, na.last=FALSE, decreasing = FALSE, stable = TRUE
, optimize = c("time", "memory"), VERBOSE = FALSE, \dots)
\method{ramorder}{integer64}(x, i, has.na = TRUE, na.last=FALSE, decreasing = FALSE, stable = TRUE
, optimize = c("time", "memory"), VERBOSE = FALSE, \dots)
}
\arguments{
  \item{x}{ a vector to be sorted by \code{\link{ramsort}} and \code{\link{ramsortorder}}, i.e. the output of  \code{\link{sort}} }
  \item{i}{ integer positions to be modified by \code{\link{ramorder}} and \code{\link{ramsortorder}}, default is 1:n, in this case the output is similar to \code{\link{order}} }
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
  \item{restlevel}{
number of remaining recursionlevels before \code{quicksort} switches from recursing to \code{shellsort}
}
  \item{radixbits}{
	size of radix in bits
}
  \item{VERBOSE}{
  cat some info about chosen method
}
  \item{\dots}{ further arguments, passed from generics, ignored in methods }
}
\details{
 see \code{\link[bit]{ramsort}}
}
\value{
  These functions return the number of \code{NAs} found or assumed during sorting
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\keyword{ programming }
\keyword{ manip }
\seealso{ \code{\link{ramsort}} for the generic, \code{\link[ff]{ramsort.default}} for the methods provided by package \code{\link[ff]{ff}}, \code{\link{sort.integer64}} for the sort interface and \code{\link{sortcache}} for caching the work of sorting}
\examples{
  x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
  x
  message("ramsort example")
  s <- clone(x)
  ramsort(s)
  message("s has been changed in-place - whether or not ramsort uses an in-place algorithm")
  s
  message("ramorder example")
  s <- clone(x)
  o <- seq_along(s)
  ramorder(s, o)
  message("o has been changed in-place - s remains unchanged")
  s
  o
  s[o]
  message("ramsortorder example")
  o <- seq_along(s)
  ramsortorder(s, o)
  message("s and o have both been changed in-place - this is much faster")
  s
  o
}
