\name{bit64S3}
\alias{bit64S3}
\title{
  Turn functions S3 generic for bit64 
}
\description{
	Turn those base functions S3 generic which are used in bit64
}
\usage{
	bit64S3()
}
\details{
   Some S3 methods for \code{\link{integer64}} cannot be dispatched because the respective function in base R is not generic. Calling \code{bit64S3} turns them generic by assiging them with name extension \code{.default} to \code{\link{globalenv}} and creating a generic in \code{\link{globalenv}}. The following functions are turned generic: 
   \preformatted{
	   \code{\link{:}}
	   \code{\link{is.double}}
	   \code{\link{match}}
	   \code{\link{\%in\%}}
	   \code{\link{unique}}
	   \code{\link{table}}
	   \code{\link{rank}}
	   \code{\link{order}}
   }
}
\value{
	\code{\link{invisible}}
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\note{
	\code{\link{is.double}} returns \code{FALSE} for \code{\link{integer64}} after calling \code{bit64S3}.
}
\seealso{
	\code{\link{bit64}}, \code{\link{S3}}
}
\examples{
	\dontrun{
		bit64S3()
	}
}
\keyword{ methods }
