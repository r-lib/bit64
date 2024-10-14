\name{as.integer64.character}
\alias{as.integer64.character}
\alias{as.integer64}
\alias{as.integer64.NULL}
\alias{as.integer64.integer64}
\alias{as.integer64.double}
\alias{as.integer64.integer}
\alias{as.integer64.logical}
\alias{as.integer64.factor}
\alias{as.integer64.bitstring}
\alias{NA_integer64_}
\title{
   Coerce to integer64
}
\usage{
 NA_integer64_

 as.integer64(x, ...)

 \method{as.integer64}{integer64}(x, ...)

 \method{as.integer64}{NULL}(x, ...)

 \method{as.integer64}{character}(x, ...)

 \method{as.integer64}{bitstring}(x, ...)

 \method{as.integer64}{double}(x, keep.names = FALSE, ...)

 \method{as.integer64}{integer}(x, ...)

 \method{as.integer64}{logical}(x, ...)

 \method{as.integer64}{factor}(x, ...)
}
\arguments{
  \item{x}{ an atomic vector }

  \item{...}{ further arguments to the code{link[=NextMethod]{NextMethod()}} }

  \item{keep.names}{ FALSE, set to TRUE to keep a names vector }
}
\value{
  The other methods return atomic vectors of the expected types
}
\description{
  Methods to coerce from other atomic types to integer64.
}
\details{
  \code{as.integer64.character} is realized using C function \code{strtoll} which does not support scientific notation.
  Instead of '1e6' use '1000000'.
  \code{as.integer64.bitstring} evaluates characters '0' anbd ' ' as zero-bit,
  all other one byte characters as one-bit,
  multi-byte characters are not allowed,
  strings shorter than 64 characters are treated as if they were left-padded with '0',
  strings longer than 64 bytes are mapped to \code{NA_INTEGER64} and a warning is emitted.
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\examples{
as.integer64(as.character(lim.integer64()))
as.integer64(
  structure(c("1111111111111111111111111111111111111111111111111111111111111110",
              "1111111111111111111111111111111111111111111111111111111111111111",
              "1000000000000000000000000000000000000000000000000000000000000000",
              "0000000000000000000000000000000000000000000000000000000000000000",
              "0000000000000000000000000000000000000000000000000000000000000001",
              "0000000000000000000000000000000000000000000000000000000000000010"
  ), class = "bitstring")
)
as.integer64(
 structure(c("............................................................... ",
             "................................................................",
             ".                                                               ",
             "",
             ".",
             "10"
  ), class = "bitstring")
)
}
\seealso{ code{link[=as.character.integer64]{as.character.integer64()}} \code{\link{integer64}}  }
\keyword{ classes }
\keyword{ manip }
