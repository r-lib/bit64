\name{identical.integer64}
\alias{identical.integer64}
\title{
   Identity function for class 'integer64'
}
\description{
  This will discover any deviation between objects containing integer64 vectors. 
}
\usage{
 identical.integer64(x, y, num.eq = FALSE, single.NA = FALSE
, attrib.as.set = TRUE, ignore.bytecode = TRUE)
}
\arguments{
  \item{x}{ atomic vector of class 'integer64' }
  \item{y}{ atomic vector of class 'integer64' }
  \item{num.eq}{ see \code{\link{identical}} }
  \item{single.NA}{ see \code{\link{identical}} }
  \item{attrib.as.set}{ see \code{\link{identical}} }
  \item{ignore.bytecode}{ see \code{\link{identical}} }
}
\details{
  This is simply a wrapper to \code{\link{identical}} with default arguments \code{num.eq = FALSE, single.NA = FALSE}.
}
\value{
  A single logical value, \code{TRUE} or \code{FALSE}, never \code{NA} and never anything other than a single value. 
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\keyword{ classes }
\keyword{ manip }
\seealso{ \code{\link{==.integer64}} \code{\link{identical}} \code{\link{integer64}}  }
\examples{
  i64 <- as.double(NA); class(i64) <- "integer64"
  identical(i64-1, i64+1)
  identical.integer64(i64-1, i64+1)
}
