\name{plusclass}
\alias{plusclass}
\alias{minusclass}
\title{
   integer64: Maintaining S3 class attribute
}
\usage{
  plusclass(class, whichclass)

  minusclass(class, whichclass)
}
\arguments{
  \item{class}{ NULL or a character vector of class attributes }

  \item{whichclass}{ the (single) class name to add or remove from the class vector  }
}
\value{
  NULL or a character vector of class attributes
}
\description{
  Maintaining integer64 S3 class attribute.
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\examples{
  plusclass("inheritingclass","integer64")
  minusclass(c("inheritingclass","integer64"), "integer64")
}
\seealso{
  code{link[=oldClass]{oldClass()}} \code{\link{integer64}}
}
\keyword{ classes }
\keyword{ manip }
\keyword{ internal }
