\name{unipos}
\alias{unipos}
\alias{unipos.integer64}
\title{Extract Positions of Unique Elements}
\description{
  \code{unipos} returns the positions of those elements returned by \code{\link{unique}}.
}
\usage{
unipos(x, incomparables = FALSE, order = c("original","values","any"), \dots)
\method{unipos}{integer64}(x, incomparables = FALSE, order = c("original","values","any")
, nunique = NULL, method = NULL, \dots)
}
\arguments{
  \item{x}{a vector or a data frame or an array or \code{NULL}.}
  \item{incomparables}{ignored}
  \item{order}{The order in which positions of unique values will be returned, see details}
  \item{nunique}{
	NULL or the number of unique values (including NA). Providing \code{nunique} can speed-up when \code{x} has no cache. Note that a wrong nunique can cause undefined behaviour up to a crash.
}
  \item{method}{
	NULL for automatic method selection or a suitable low-level method, see details
}
  \item{\dots}{ignored}
}
\details{
  This function automatically chooses from several low-level functions considering the size of \code{x} and the availability of a cache. 
  Suitable methods are \code{\link{hashmapupo}} (simultaneously creating and using a hashmap)
, \code{\link{hashupo}} (first creating a hashmap then using it)
, \code{\link{sortorderupo}} (fast ordering) 
and \code{\link{orderupo}} (memory saving ordering).
\cr
The default \code{order="original"} collects unique values in the order of the first appearance in \code{x} like in \code{\link{unique}}, this costs extra processing. 
\code{order="values"} collects unique values in sorted order like in \code{\link{table}}, this costs extra processing with the hash methods but comes for free. 
\code{order="any"} collects unique values in undefined order, possibly faster. For hash methods this will be a quasi random order, for sort methods this will be sorted order.
}
\value{
  an integer vector of positions
}
\author{
	Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\seealso{
  \code{\link{unique.integer64}} for unique values and \code{\link{match.integer64}} for general matching.
}
\examples{
x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
unipos(x)
unipos(x, order="values")

stopifnot(identical(unipos(x),  (1:length(x))[!duplicated(x)]))
stopifnot(identical(unipos(x),  match.integer64(unique(x), x)))
stopifnot(identical(unipos(x, order="values"),  match.integer64(unique(x, order="values"), x)))
stopifnot(identical(unique(x),  x[unipos(x)]))
stopifnot(identical(unique(x, order="values"),  x[unipos(x, order="values")]))
}
\keyword{manip}
\keyword{logic}
