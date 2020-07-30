\name{runif64}
\alias{runif64}
\title{
   integer64: random numbers
}
\description{
  Create uniform random 64-bit integers within defined range
}
\usage{
  runif64(n, min = lim.integer64()[1], max = lim.integer64()[2], replace=TRUE)
}
\arguments{
  \item{n}{ length of return vector }
  \item{min}{ lower inclusive bound for random numbers }
  \item{max}{ upper inclusive bound for random numbers }
  \item{replace}{ set to FALSE for sampleing from a finite pool, see \code{\link{sample}} }
}
\value{
  a integer64 vector
}
\details{
  For each random integer we call R's internal C interface \code{unif_rand()} twice.
  Each call is mapped to 2^32 unsigned integers. The two 32-bit patterns are concatenated
  to form the new integer64. This process is repeated until the result is not a \code{NA_INTEGER64}. 
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\keyword{ classes }
\keyword{distribution}
\keyword{sysdata}
\seealso{ 
  \code{\link{runif}}, \code{\link{hashfun}}
}
\examples{
  runif64(12)
  runif64(12, -16, 16)
  runif64(12, 0, as.integer64(2^60)-1)  # not 2^60-1 !
  var(runif(1e4))
  var(as.double(runif64(1e4, 0, 2^40))/2^40)  # ~ = 1/12 = .08333

  table(sample(16, replace=FALSE))
  table(runif64(16, 1, 16, replace=FALSE))
  table(sample(16, replace=TRUE))
  table(runif64(16, 1, 16, replace=TRUE))
}
