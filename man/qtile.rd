\name{qtile}
\alias{qtile}
\alias{qtile.integer64}
\alias{quantile.integer64}
\alias{median.integer64}
\alias{mean.integer64}
\alias{summary.integer64}
\title{(Q)uan(Tile)s }
\description{
	Function \code{\link{prank.integer64}}  projects the values [min..max] via ranks [1..n] to [0..1]. 
	\code{qtile.ineger64} is the inverse function of 'prank.integer64' and projects [0..1] to [min..max].
}
\usage{
	qtile(x, probs=seq(0, 1, 0.25), \dots)
	\method{qtile}{integer64}(x, probs = seq(0, 1, 0.25), names = TRUE, method = NULL, \dots)
	\method{quantile}{integer64}(x, probs = seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type=0L, \dots)
 \method{mean}{integer64}(x, na.rm = FALSE, \dots)
	\method{summary}{integer64}(object, \dots)
 ## mean(x, na.rm = FALSE, ...)
 ## or
 ## mean(x, na.rm = FALSE)
}
\arguments{
  \item{x}{a integer64 vector}
  \item{object}{a integer64 vector}
  \item{probs}{
		numeric vector of probabilities with values in [0,1] - possibly containing \code{NA}s
}
  \item{names}{
	logical; if \code{TRUE}, the result has a \code{names} attribute. Set to \code{FALSE} for speedup with many probs.
}
  \item{type}{
	an integer selecting the quantile algorithm, currently only 0 is supported, see details
}
  \item{method}{
	NULL for automatic method selection or a suitable low-level method, see details
}
  \item{na.rm}{
	logical; if \code{TRUE}, any \code{NA} and \code{NaN}'s are removed from \code{x} before the quantiles are computed.
}
  \item{\dots}{ignored}
}
\details{
 Functions \code{quantile.integer64} with \code{type=0} and \code{median.integer64} are convenience wrappers to \code{qtile}.
 \cr
	Function \code{qtile} behaves very similar to \code{quantile.default} with \code{type=1} 
 in that it only returns existing values, it is mostly symetric 
 but it is using 'round' rather than 'floor'. 
 \cr
 Note that this implies that \code{median.integer64} does not interpolate for even number of values 
(interpolation would create values that could not be represented as 64-bit integers).
 \cr
  This function automatically chooses from several low-level functions considering the size of \code{x} and the availability of a cache. 
  Suitable methods are \code{\link{sortqtl}} (fast sorting) 
and \code{\link{orderqtl}} (memory saving ordering).
}
\value{
  \code{prank} returns a numeric vector of the same length as \code{x}.
  \cr
  \code{qtile} returns a vector with elements from \code{x} 
  at the relative positions specified by \code{probs}.
}
\author{
	Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\seealso{
  \code{\link{rank.integer64}} for simple ranks and \code{\link{quantile}} for quantiles.
}
\examples{
x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
qtile(x, probs=seq(0, 1, 0.25))
quantile(x, probs=seq(0, 1, 0.25), na.rm=TRUE)
median(x, na.rm=TRUE)
summary(x)

x <- x[!is.na(x)]
stopifnot(identical(x,  unname(qtile(x, probs=prank(x)))))
}
\keyword{univar}
