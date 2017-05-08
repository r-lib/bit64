\name{match.integer64}
\alias{match.integer64}
\alias{\%in\%.integer64}
\title{
64-bit integer matching
}
\description{
\code{match} returns a vector of the positions of (first) matches of its first argument in its second. 

\code{\%in\%} is a more intuitive interface as a binary operator, which returns a logical vector indicating if there is a match or not for its left operand. 

}
\usage{
\method{match}{integer64}(x, table, nomatch = NA_integer_, nunique = NULL, method = NULL, ...)
\method{\%in\%}{integer64}(x, table, ...)
}
\arguments{
  \item{x}{
	integer64 vector: the values to be matched, optionally carrying a cache created with \code{\link{hashcache}}
}
  \item{table}{
	integer64 vector: the values to be matched against, optionally carrying a cache created with \code{\link{hashcache}} or \code{\link{sortordercache}}
}
  \item{nomatch}{
  the value to be returned in the case when no match is found. Note that it is coerced to integer.
}
  \item{nunique}{
	NULL or the number of unique values of table (including NA). Providing \code{nunique} can speed-up matching when \code{table} has no cache. Note that a wrong nunique can cause undefined behaviour up to a crash.
}
  \item{method}{
	NULL for automatic method selection or a suitable low-level method, see details
}
  \item{\dots}{
ignored
}
}
\details{
  These functions automatically choose from several low-level functions considering the size of \code{x} and \code{table} and the availability of caches. 


  Suitable methods for \code{\%in\%.integer64} are \code{\link{hashpos}} (hash table lookup), \code{\link{hashrev}} (reverse lookup), \code{\link{sortorderpos}} (fast ordering) and \code{\link{orderpos}} (memory saving ordering).
  Suitable methods for \code{match.integer64} are \code{\link{hashfin}} (hash table lookup), \code{\link{hashrin}} (reverse lookup), \code{\link{sortfin}} (fast sorting) and \code{\link{orderfin}} (memory saving ordering).
}
\value{
  A vector of the same length as \code{x}.

  \code{match}: An integer vector giving the position in \code{table} of
  the first match if there is a match, otherwise \code{nomatch}.

  If \code{x[i]} is found to equal \code{table[j]} then the value
  returned in the \code{i}-th position of the return value is \code{j},
  for the smallest possible \code{j}.  If no match is found, the value
  is \code{nomatch}.

  \code{\%in\%}: A logical vector, indicating if a match was located for
  each element of \code{x}: thus the values are \code{TRUE} or
  \code{FALSE} and never \code{NA}.
}
\author{
	Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\seealso{
	\code{\link{match}}
}
\examples{
x <- as.integer64(c(NA, 0:9), 32)
table <- as.integer64(c(1:9, NA))
match.integer64(x, table)
"\%in\%.integer64"(x, table)

x <- as.integer64(sample(c(rep(NA, 9), 0:9), 32, TRUE))
table <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
stopifnot(identical(match.integer64(x, table), match(as.integer(x), as.integer(table))))
stopifnot(identical("\%in\%.integer64"(x, table), as.integer(x) \%in\% as.integer(table)))

\dontrun{
	message("check when reverse hash-lookup beats standard hash-lookup")
	e <- 4:24
	timx <- timy <- matrix(NA, length(e), length(e), dimnames=list(e,e))
	for (iy in seq_along(e))
	for (ix in 1:iy){
		nx <- 2^e[ix]
		ny <- 2^e[iy]
		x <- as.integer64(sample(ny, nx, FALSE))
		y <- as.integer64(sample(ny, ny, FALSE))
		#hashfun(x, bits=as.integer(5))
		timx[ix,iy] <- repeat.time({
		hx <- hashmap(x)
		py <- hashrev(hx, y)
		})[3]
		timy[ix,iy] <- repeat.time({
		hy <- hashmap(y)
		px <- hashpos(hy, x)
		})[3]
		#identical(px, py)
		print(round(timx[1:iy,1:iy]/timy[1:iy,1:iy], 2), na.print="")
	}

	message("explore best low-level method given size of x and table")
	B1 <- 1:27
	B2 <- 1:27
	tim <- array(NA, dim=c(length(B1), length(B2), 5)
 , dimnames=list(B1, B2, c("hashpos","hashrev","sortpos1","sortpos2","sortpos3")))
	for (i1 in B1)
	for (i2 in B2)
	{
	  b1 <- B1[i1]
	  b2 <- B1[i2]
	  n1 <- 2^b1
	  n2 <- 2^b2
	  x1 <- as.integer64(c(sample(n2, n1-1, TRUE), NA))
	  x2 <- as.integer64(c(sample(n2, n2-1, TRUE), NA))
	  tim[i1,i2,1] <- repeat.time({h <- hashmap(x2);hashpos(h, x1);rm(h)})[3]
	  tim[i1,i2,2] <- repeat.time({h <- hashmap(x1);hashrev(h, x2);rm(h)})[3]
	  s <- clone(x2); o <- seq_along(s); ramsortorder(s, o)
	  tim[i1,i2,3] <- repeat.time(sortorderpos(s, o, x1, method=1))[3]
	  tim[i1,i2,4] <- repeat.time(sortorderpos(s, o, x1, method=2))[3]
	  tim[i1,i2,5] <- repeat.time(sortorderpos(s, o, x1, method=3))[3]
	  rm(s,o)
	  print(apply(tim, 1:2, function(ti)if(any(is.na(ti)))NA else which.min(ti)))
	}
}
}
\keyword{manip}
\keyword{logic}
