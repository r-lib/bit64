\name{xor.integer64}
\alias{&.integer64}
\alias{|.integer64}
\alias{xor.integer64}
\alias{!=.integer64}
\alias{==.integer64}
\alias{<.integer64}
\alias{<=.integer64}
\alias{>.integer64}
\alias{>=.integer64}
\alias{+.integer64}
\alias{-.integer64}
\alias{*.integer64}
\alias{^.integer64}
\alias{/.integer64}
\alias{\%/\%.integer64}
\alias{\%\%.integer64}
\alias{binattr}
\title{
   Binary operators for integer64 vectors
}
\description{
  Binary operators for integer64 vectors.
}
\usage{
\method{&}{integer64}(e1,e2)
\method{|}{integer64}(e1,e2)
\method{xor}{integer64}(x,y)
\method{!=}{integer64}(e1,e2)
\method{==}{integer64}(e1,e2)
\method{<}{integer64}(e1,e2)
\method{<=}{integer64}(e1,e2)
\method{>}{integer64}(e1,e2)
\method{>=}{integer64}(e1,e2)
\method{+}{integer64}(e1,e2)
\method{-}{integer64}(e1,e2)
\method{*}{integer64}(e1,e2)
\method{^}{integer64}(e1,e2)
\method{/}{integer64}(e1,e2)
\method{\%/\%}{integer64}(e1,e2)
\method{\%\%}{integer64}(e1,e2)
binattr(e1,e2) # for internal use only
}
\arguments{
  \item{e1}{ an atomic vector of class 'integer64'}
  \item{e2}{ an atomic vector of class 'integer64'}
  \item{x}{ an atomic vector of class 'integer64'}
  \item{y}{ an atomic vector of class 'integer64'}
}
\value{
  \code{\link{&}}, \code{\link{|}}, \code{\link{xor}}, \code{\link{!=}}, \code{\link{==}}, 
  \code{\link{<}}, \code{\link{<=}}, \code{\link{>}}, \code{\link{>=}} return a logical vector \cr
  \code{\link{^}} and \code{\link{/}} return a double vector\cr
  \code{\link{+}}, \code{\link{-}}, \code{\link{*}}, \code{\link{\%/\%}}, \code{\link{\%\%}}
   return a vector of class 'integer64'
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\keyword{ classes }
\keyword{ manip }
\seealso{ \code{\link{format.integer64}} \code{\link{integer64}}  }
\examples{
  as.integer64(1:12) - 1
}
