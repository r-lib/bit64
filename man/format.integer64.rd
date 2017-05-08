\name{format.integer64}
\alias{format.integer64}
\alias{is.na.integer64}
\alias{is.nan.integer64}
\alias{is.finite.integer64}
\alias{is.infinite.integer64}
\alias{!.integer64}
\alias{sign.integer64}
\alias{abs.integer64}
\alias{sqrt.integer64}
\alias{log.integer64}
\alias{log2.integer64}
\alias{log10.integer64}
\alias{floor.integer64}
\alias{ceiling.integer64}
\alias{trunc.integer64}
\alias{round.integer64}
\alias{signif.integer64}
\alias{scale.integer64}
\title{
   Unary operators and functions for integer64 vectors
}
\description{
  Unary operators and functions for integer64 vectors.
}
\usage{
\method{format}{integer64}(x, justify="right", \dots)
\method{is.na}{integer64}(x)
\method{is.nan}{integer64}(x)
\method{is.finite}{integer64}(x)
\method{is.infinite}{integer64}(x)
\method{!}{integer64}(x)
\method{sign}{integer64}(x)
\method{abs}{integer64}(x)
\method{sqrt}{integer64}(x)
\method{log}{integer64}(x, base)
\method{log2}{integer64}(x)
\method{log10}{integer64}(x)
\method{floor}{integer64}(x)
\method{ceiling}{integer64}(x)
\method{trunc}{integer64}(x, \dots)
\method{round}{integer64}(x, digits=0)
\method{signif}{integer64}(x, digits=6)
\method{scale}{integer64}(x, center = TRUE, scale = TRUE)
}
\arguments{
  \item{x}{ an atomic vector of class 'integer64'}
  \item{base}{ an atomic scalar (we save 50\% log-calls by not allowing a vector base) }
  \item{digits}{ integer indicating the number of decimal places (round) or significant digits (signif) to be used. 
                 Negative values are allowed (see \code{\link{round}}) }
  \item{justify}{ should it be right-justified (the default), left-justified, centred or left alone. }
  \item{center}{ see \code{\link{scale}} }
  \item{scale}{  see \code{\link{scale}} }
  \item{\dots}{ further arguments to the \code{\link{NextMethod}} }
}
\value{
  \code{\link{format}} returns a character vector \cr
  \code{\link{is.na}} and \code{\link{!}} return a logical vector \cr
  \code{\link{sqrt}}, \code{\link{log}}, \code{\link{log2}} and \code{\link{log10}} return a double vector \cr
  \code{\link{sign}}, \code{\link{abs}}, \code{\link{floor}}, \code{\link{ceiling}}, \code{\link{trunc}} and 
  \code{\link{round}} return a vector of class 'integer64' \cr
  \code{\link{signif}} is not implemented 
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\keyword{ classes }
\keyword{ manip }
\seealso{ \code{\link{xor.integer64}} \code{\link{integer64}}  }
\examples{
  sqrt(as.integer64(1:12))
}
