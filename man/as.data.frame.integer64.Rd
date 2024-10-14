\name{as.data.frame.integer64}
\alias{as.data.frame.integer64}
\title{
   integer64: Coercing to data.frame column
}
\usage{
  \method{as.data.frame}{integer64}(x, ...)
}
\arguments{
  \item{x}{ an integer64 vector }

  \item{...}{ passed to NextMethod \code{\link[=as.data.frame]{as.data.frame()}} after removing the 'integer64' class attribute }
}
\value{
  a one-column data.frame containing an integer64 vector
}
\description{
  Coercing integer64 vector to data.frame.
}
\details{
  'as.data.frame.integer64' is rather not intended to be called directly,
  but it is required to allow integer64 as data.frame columns.
}
\note{
  This is currently very slow -- any ideas for improvement?
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\examples{
  as.data.frame.integer64(as.integer64(1:12))
  data.frame(a=1:12, b=as.integer64(1:12))
}
\seealso{
  \code{\link[=cbind.integer64]{cbind.integer64()}} \code{\link{integer64}}  %as.vector.integer64 removed as requested by the CRAN maintainer \code{\link{as.vector.integer64}}
}
\keyword{ classes }
\keyword{ manip }
