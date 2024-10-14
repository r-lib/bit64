\name{c.integer64}
\alias{c.integer64}
\alias{cbind.integer64}
\alias{rbind.integer64}
\title{
   Concatenating integer64 vectors
}
\usage{
\method{c}{integer64}(..., recursive = FALSE)

\method{cbind}{integer64}(...)

\method{rbind}{integer64}(...)
}
\arguments{
  \item{...}{ two or more arguments coerced to 'integer64' and passed to code{link[=NextMethod]{NextMethod()}} }

  \item{recursive}{ logical. If \code{recursive = TRUE}, the function recursively descends through lists (and pairlists) combining all their elements into a vector. }
}
\value{
  code{link[=c]{c()}} returns a integer64 vector of the total length of the input \cr
  code{link[=cbind]{cbind()}} and \code{\link{rbind}} return a integer64 matrix
}
\description{
  The ususal functions 'c', 'cbind' and 'rbind'
}
\note{
  R currently only dispatches generic 'c' to method 'c.integer64' if the first argument is 'integer64'
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\examples{
  c(as.integer64(1), 2:6)
  cbind(1:6, as.integer(1:6))
  rbind(1:6, as.integer(1:6))
}
\seealso{ code{link[=rep.integer64]{rep.integer64()}} code{link[=seq.integer64]{seq.integer64()}}
          code{link[=as.data.frame.integer64]{as.data.frame.integer64()}} \code{\link{integer64}}
}
\keyword{ classes }
\keyword{ manip }
