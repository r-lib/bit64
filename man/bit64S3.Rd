\name{bit64S3}
\alias{bit64S3}
\alias{:}
\alias{:.default}
\alias{:.integer64}
\alias{is.double}
\alias{is.double.default}
\alias{is.double.integer64}
\alias{match}
\alias{match.default}
\alias{\%in\%}
\alias{\%in\%.default}
\alias{rank}
\alias{rank.default}
\alias{order}
\alias{order.default}
\title{
  Turning base R functions into S3 generics for bit64
}
\usage{
    from:to

 #--as-cran complains about \method{:}{default}(from, to)
 #--as-cran complains about \method{:}{integer64}(from, to)
    is.double(x)

 \method{is.double}{default}(x)

 \method{is.double}{integer64}(x)

    match(x, table, ...)

 \method{match}{default}(x, table, ...)

    x \%in\% table

 \method{\%in\%}{default}(x, table)

    rank(x, ...)

 \method{rank}{default}(x, ...)

    %table(...)

 %\method{table}{default}(...)

    order(...)

 \method{order}{default}(...)
}
\arguments{
  \item{x}{
    integer64 vector: the values to be matched, optionally carrying a cache created with code{link[=hashcache]{hashcache()}}
}

  \item{table}{
    integer64 vector: the values to be matched against, optionally carrying a cache created with code{link[=hashcache]{hashcache()}} or code{link[=sortordercache]{sortordercache()}}
}

  \item{...}{ ignored }

  \item{from}{ scalar denoting first element of sequence }

  \item{to}{ scalar denoting last element of sequence }
}
\value{
    code{link[=invisible]{invisible()}}
}
\description{
    Turn those base functions S3 generic which are used in bit64
}
\details{
   The following functions are turned into S3 gernerics in order to dispatch methods for \code{\link{integer64}}:
   \preformatted{
       \code{\link{:}}
       code{link[=is.double]{is.double()}}
       code{link[=match]{match()}}
       \code{\link{\%in\%}}
       %code{link[=table]{table()}}
       code{link[=rank]{rank()}}
       code{link[=order]{order()}}
   }
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\note{
    code{link[=is.double]{is.double()}} returns \code{FALSE} for \code{\link{integer64}} \cr
    \code{\link{:}} currently only dispatches at its first argument, thus \code{as.integer64(1):9} works but \code{1:as.integer64(9)} doesn't
    code{link[=match]{match()}} currently only dispatches at its first argument and expects its second argument also to be integer64, otherwise throws an error. Beware of something like \code{match(2, as.integer64(0:3))}
    \code{\link{\%in\%}} currently only dispatches at its first argument and expects its second argument also to be integer64, otherwise throws an error. Beware of something like \code{2 \%in\% as.integer64(0:3)}
    code{link[=order]{order()}} currently only orders a single argument, trying more than one raises an error
}
\examples{
 is.double(as.integer64(1))
    as.integer64(1):9
 match(as.integer64(2), as.integer64(0:3))
 as.integer64(2) \%in\% as.integer64(0:3)

 unique(as.integer64(c(1,1,2)))
 rank(as.integer64(c(1,1,2)))

 %table(as.integer64(c(1,1,2)))
 %table(as.integer64(c(1,1,2)),as.integer64(c(3,4,4)))
 %table(as.integer64(c(1,1,2)),c(3,4,4))
 %table(c(1,1,2),as.integer64(c(3,4,4)))

 order(as.integer64(c(1,NA,2)))

 \dontshow{
 stopifnot(identical(match(as.integer64(2), as.integer64(0:3)), match(2, 0:3)))
 stopifnot(identical(as.integer64(2) \%in\% as.integer64(0:3), 2 \%in\% 0:3))

 stopifnot(identical(unique(as.integer64(c(1,1,2))), as.integer64(unique(c(1,1,2)))))
 stopifnot(identical(rank(as.integer64(c(1,1,2))), rank(c(1,1,2))))

 %stopifnot(identical(table(as.integer64(c(1,1,2))), table(c(1,1,2))))
 %stopifnot(identical(table(as.integer64(c(1,1,2)),as.integer64(c(3,4,4))), table(c(1,1,2),c(3,4,4))))
 %stopifnot(identical(table(as.integer64(c(1,1,2)),c(3,4,4)), table(c(1,1,2),c(3,4,4))))
 %stopifnot(identical(table(c(1,1,2),as.integer64(c(3,4,4))), table(c(1,1,2),c(3,4,4))))

 stopifnot(identical(order(as.integer64(c(1,NA,2))), order(c(1,NA,2))))
 stopifnot(identical(order(as.integer64(c(1,NA,2)), decreasing=TRUE), order(c(1,NA,2), decreasing=TRUE)))
 }
}
\seealso{
    code{link[=bit64]{bit64()}}, \code{\link{S3}}
}
\keyword{ methods }
