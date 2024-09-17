\name{integer64_semantics}
\alias{integer64_semantics}
\title{
   Query integer64 semantics
}
\description{
  Query integer64 semantics regarding multiplication and division
}
\usage{
  integer64_semantics()
}
\value{
  \code{'old'} for the old asymmetric semantic or \code{'new'} for the new symmetric semantic suggested by Ofek Shilon.
}
\author{
  Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\keyword{ programming }
\keyword{ manip }
\seealso{ \code{\link{bit64-package}} }
\examples{
  integer64_semantics()
  d <- 2.5
  i <- as.integer64(5)
  d/i  # old: 0.4, new 0.5 
  d*i  # old: 10, new 13
  i*d  # old: 13, new 13
}
