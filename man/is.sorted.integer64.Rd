\name{is.sorted.integer64}
\alias{is.sorted.integer64}
\alias{na.count.integer64}
\alias{nvalid.integer64}
\alias{nunique.integer64}
\alias{nties.integer64}
\title{
    Small cache access methods
}
\usage{
    \method{is.sorted}{integer64}(x, ...)

    \method{na.count}{integer64}(x, ...)

    \method{nvalid}{integer64}(x, ...)

    \method{nunique}{integer64}(x, ...)

    \method{nties}{integer64}(x, ...)
}
\arguments{
  \item{x}{
    some object
    }

  \item{...}{
    ignored
    }
}
\value{
    \code{is.sorted} returns a logical scalar, the other methods return an integer scalar.
}
\description{
    These methods are packaged here for methods in packages \code{bit64} and \code{ff}.
}
\details{
  All these functions benefit from a \code{\link[=sortcache]{sortcache()}}, \code{\link[=ordercache]{ordercache()}} or \code{\link[=sortordercache]{sortordercache()}}.
  \code{na.count}, \code{nvalid} and \code{nunique} also benefit from a \code{\link[=hashcache]{hashcache()}}.
    \cr
    \code{is.sorted} checks for sortedness of \code{x} (NAs sorted first) \cr
 \code{na.count} returns the number of \code{NA}s \cr
 \code{nvalid} returns the number of valid data points, usually \code{\link[=length]{length()}} minus \code{na.count}. \cr
 \code{nunique} returns the number of unique values \cr
 \code{nties} returns the number of tied values.
}
\note{
    If a \code{\link[=cache]{cache()}} exists but the desired value is not cached,
 then these functions will store their result in the cache.
 We do not consider this a relevant side-effect,
 since these small cache results do not have a relevant memory footprint.
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\examples{
    x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
 length(x)
 na.count(x)
 nvalid(x)
 nunique(x)
 nties(x)
 table.integer64(x)
 x
}
\seealso{
    \code{\link[=cache]{cache()}} for caching functions and \code{\link[=sortordercache]{sortordercache()}} for functions creating big caches
}
\keyword{ environment }
\keyword{ methods }
