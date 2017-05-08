\name{unique.integer64}
\alias{unique.integer64}
\title{Extract Unique Elements from integer64}
\description{
  \code{unique} returns a vector like \code{x} but with duplicate elements/rows removed.
}
\usage{
\method{unique}{integer64}(x, incomparables = FALSE, order = c("original","values","any")
, nunique = NULL, method = NULL, \dots)
}
\arguments{
  \item{x}{a vector or a data frame or an array or \code{NULL}.}
  \item{incomparables}{ignored}
  \item{order}{The order in which unique values will be returned, see details}
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
  Suitable methods are \code{\link{hashmapuni}} (simultaneously creating and using a hashmap)
, \code{\link{hashuni}} (first creating a hashmap then using it)
, \code{\link{sortuni}} (fast sorting for sorted order only)
, \code{\link{sortorderuni}} (fast ordering for original order only) 
and \code{\link{orderuni}} (memory saving ordering).
\cr
The default \code{order="original"} returns unique values in the order of the first appearance in \code{x} like in \code{\link{unique}}, this costs extra processing. 
\code{order="values"} returns unique values in sorted order like in \code{\link{table}}, this costs extra processing with the hash methods but comes for free. 
\code{order="any"} returns unique values in undefined order, possibly faster. For hash methods this will be a quasi random order, for sort methods this will be sorted order.
}
\value{
  For a vector, an object of the same type of \code{x}, but with only
  one copy of each duplicated element.  No attributes are copied (so
  the result has no names).
}
\author{
	Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\seealso{
  \code{\link{unique}} for the generic, \code{\link{unipos}} which gives the indices of the unique
  elements and \code{\link{table.integer64}} which gives frequencies of the unique elements.
}
\examples{
x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
unique(x)
unique(x, order="values")

stopifnot(identical(unique(x),  x[!duplicated(x)]))
stopifnot(identical(unique(x),  as.integer64(unique(as.integer(x)))))
stopifnot(identical(unique(x, order="values")
,  as.integer64(sort(unique(as.integer(x)), na.last=FALSE))))
}
\keyword{manip}
\keyword{logic}
