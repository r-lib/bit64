% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/highlevel64.R
\name{rank.integer64}
\alias{rank.integer64}
\title{Sample Ranks from integer64}
\usage{
\method{rank}{integer64}(x, method = NULL, ...)
}
\arguments{
\item{x}{a integer64 vector}

\item{method}{NULL for automatic method selection or a suitable low-level
method, see details}

\item{...}{ignored}
}
\value{
A numeric vector of the same length as \code{x}.
}
\description{
Returns the sample ranks of the values in a vector.  Ties (i.e., equal
values) are averaged and missing values propagated.
}
\details{
This function automatically chooses from several low-level functions
considering the size of \code{x} and the availability of a cache.
Suitable methods are
\itemize{
\item \code{\link[=sortorderrnk]{sortorderrnk()}} (fast ordering)
\item \code{\link[=orderrnk]{orderrnk()}} (memory saving ordering).
}
}
\examples{
x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
rank.integer64(x)

stopifnot(identical(rank.integer64(x),  rank(as.integer(x)
, na.last="keep", ties.method = "average")))

}
\seealso{
\code{\link[=order.integer64]{order.integer64()}}, \code{\link[=rank]{rank()}} and \code{\link[=prank]{prank()}} for percent rank.
}
\keyword{univar}
