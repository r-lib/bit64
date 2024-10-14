\name{seq.integer64}
\alias{seq.integer64}
\title{
   integer64: Sequence Generation
}
\usage{
\method{seq}{integer64}(from = NULL, to = NULL, by = NULL, length.out = NULL, along.with = NULL, ...)
}
\arguments{
  \item{from}{ integer64 scalar (in order to dispatch the integer64 method of \code{\link{seq}} }

  \item{to}{ scalar }

  \item{by}{ scalar }

  \item{length.out}{ scalar }

  \item{along.with}{ scalar }

  \item{...}{ ignored }
}
\value{
  an integer64 vector with the generated sequence
}
\description{
  Generating sequence of integer64 values
}
\details{
  \code{seq.integer64} does coerce its arguments 'from', 'to' and 'by' to \code{integer64}.
  If not provided, the argument 'by' is automatically determined as \code{+1} or \code{-1},
  but the size of 'by' is not calculated as in \code{\link{seq}} (because this might result in a non-integer value).
}
\note{
  In base R \code{\link{:}} currently is not generic and does not dispatch, see section "Limitations inherited from Base R" in \code{\link{integer64}}
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\examples{
  # colon not activated: as.integer64(1):12
  seq(as.integer64(1), 12, 2)
  seq(as.integer64(1), by=2, length.out=6)
}
\seealso{ \code{\link{c.integer64}} \code{\link{rep.integer64}}
          \code{\link{as.data.frame.integer64}} \code{\link{integer64}}
}
\keyword{ classes }
\keyword{ manip }
