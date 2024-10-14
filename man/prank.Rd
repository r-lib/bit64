\name{prank}
\alias{prank}
\alias{prank.integer64}
\title{(P)ercent (Rank)s}
\usage{
    prank(x, ...)

    \method{prank}{integer64}(x, method = NULL, ...)
}
\arguments{
  \item{x}{a integer64 vector}

  \item{...}{ignored}

  \item{method}{
    NULL for automatic method selection or a suitable low-level method, see details
}
}
\value{
  \code{prank} returns a numeric vector of the same length as \code{x}.
}
\description{
    Function \code{prank.integer64}  projects the values [min..max] via ranks [1..n] to [0..1].
    \code{\link[=qtile.integer64]{qtile.integer64()}} is the inverse function of 'prank.integer64' and projects [0..1] to [min..max].
}
\details{
    Function \code{prank.integer64} is based on \code{\link[=rank.integer64]{rank.integer64()}}.
}
\author{
    Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\examples{
x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
prank(x)

x <- x[!is.na(x)]
stopifnot(identical(x,  unname(qtile(x, probs=prank(x)))))
}
\seealso{
  \code{\link[=rank.integer64]{rank.integer64()}} for simple ranks and \code{\link{qtile}} for the inverse function quantiles.
}
\keyword{univar}
