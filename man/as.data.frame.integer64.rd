\name{as.data.frame.integer64}
\alias{as.data.frame.integer64}
\title{
   integer64: Coercing to data.frame column
}
\description{
  Coercing integer64 vector to data.frame.
}
\usage{
  \method{as.data.frame}{integer64}(x, \dots)
}
\arguments{
  \item{x}{ an integer64 vector }
  \item{\dots}{ passed to NextMethod \code{\link{as.data.frame}} after removing the 'integer64' class attribute }
}
\value{
  a one-column data.frame containing an integer64 vector
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
\keyword{ classes }
\keyword{ manip }
\seealso{ 
  \code{\link{cbind.integer64}} \code{\link{integer64}}  %as.vector.integer64 removed as requested by the CRAN maintainer \code{\link{as.vector.integer64}} 
}
\examples{
  as.data.frame.integer64(as.integer64(1:12))
  data.frame(a=1:12, b=as.integer64(1:12))
}
