\name{hashmap}
\alias{hashfun}
\alias{hashfun.integer64}
\alias{hashmap}
\alias{hashmap.integer64}
\alias{hashpos}
\alias{hashpos.cache_integer64}
\alias{hashrev}
\alias{hashrev.cache_integer64}
\alias{hashfin}
\alias{hashfin.cache_integer64}
\alias{hashrin}
\alias{hashrin.cache_integer64}
\alias{hashdup}
\alias{hashdup.cache_integer64}
\alias{hashuni}
\alias{hashuni.cache_integer64}
\alias{hashmapuni}
\alias{hashmapuni.integer64}
\alias{hashupo}
\alias{hashupo.cache_integer64}
\alias{hashmapupo}
\alias{hashmapupo.integer64}
\alias{hashtab}
\alias{hashtab.cache_integer64}
\alias{hashmaptab}
\alias{hashmaptab.integer64}
\title{
   Hashing for 64bit integers
}
\description{
This is an explicit implementation of hash functionality that underlies 
matching and other functions in R. Explicit means that you can create, 
store and use hash functionality directly. One advantage is that you can
re-use hashmaps, which avoid re-building hashmaps again and again.
}
\usage{
hashfun(x, \dots)
\method{hashfun}{integer64}(x, minfac=1.41, hashbits=NULL, \dots)
hashmap(x, \dots)
\method{hashmap}{integer64}(x, nunique=NULL, minfac=1.41, hashbits=NULL, cache=NULL, \dots)
hashpos(cache, \dots)
\method{hashpos}{cache_integer64}(cache, x, nomatch = NA_integer_, \dots)
hashrev(cache, \dots)
\method{hashrev}{cache_integer64}(cache, x, nomatch = NA_integer_, \dots)
hashfin(cache, \dots)
\method{hashfin}{cache_integer64}(cache, x, \dots)
hashrin(cache, \dots)
\method{hashrin}{cache_integer64}(cache, x, \dots)
hashdup(cache, \dots)
\method{hashdup}{cache_integer64}(cache, \dots)
hashuni(cache, \dots)
\method{hashuni}{cache_integer64}(cache, keep.order=FALSE, \dots)
hashmapuni(x, \dots)
\method{hashmapuni}{integer64}(x, nunique=NULL, minfac=1.5, hashbits=NULL, \dots)
hashupo(cache, \dots)
\method{hashupo}{cache_integer64}(cache, keep.order=FALSE, \dots)
hashmapupo(x, \dots)
\method{hashmapupo}{integer64}(x, nunique=NULL, minfac=1.5, hashbits=NULL, \dots)
hashtab(cache, \dots)
\method{hashtab}{cache_integer64}(cache, \dots)
hashmaptab(x, \dots)
\method{hashmaptab}{integer64}(x, nunique=NULL, minfac=1.5, hashbits=NULL, \dots)
}
\arguments{
  \item{x}{ an integer64 vector }
  \item{hashmap}{ an object of class 'hashmap' i.e. here 'cache_integer64' }
  \item{minfac}{ minimum factor by which the hasmap has more elements compared to the data \code{x}, ignored if \code{hashbits} is given directly }
  \item{hashbits}{ length of hashmap is \code{2^hashbits} }
  \item{cache}{ an optional \code{\link{cache}} object into which to put the hashmap (by default a new cache is created)}
  \item{nunique}{ giving \emph{correct} number of unique elements can help reducing the size of the hashmap }
  \item{nomatch}{ the value to be returned if an element is not found in the hashmap }
  \item{keep.order}{ determines order of results and speed: \code{FALSE} (the default) is faster and returns in the (pseudo)random order of the hash function, \code{TRUE} returns in the order of first appearance in the original data, but this requires extra work } 
  \item{\dots}{ further arguments, passed from generics, ignored in methods }
}
\details{
\tabular{rrl}{
   \bold{function} \tab \bold{see also}          \tab \bold{description} \cr
   \code{hashfun} \tab \code{\link[digest]{digest}} \tab export of the hash function used in \code{hashmap} \cr
   \code{hashmap} \tab \code{\link[=match.integer64]{match}} \tab return hashmap \cr
   \code{hashpos} \tab \code{\link[=match.integer64]{match}} \tab return positions of \code{x} in \code{hashmap} \cr
   \code{hashrev} \tab \code{\link[=match.integer64]{match}} \tab return positions of \code{hashmap} in \code{x} \cr
   \code{hashfin} \tab \code{\link{\%in\%.integer64}} \tab return logical whether \code{x} is in \code{hashmap} \cr
   \code{hashrin} \tab \code{\link{\%in\%.integer64}} \tab return logical whether \code{hashmap} is in \code{x}  \cr
   \code{hashdup} \tab \code{\link[=duplicated.integer64]{duplicated}} \tab return logical whether hashdat is duplicated using hashmap\cr
   \code{hashuni} \tab \code{\link[=unique.integer64]{unique}} \tab return unique values of hashmap  \cr
   \code{hashmapuni} \tab \code{\link[=unique.integer64]{unique}} \tab return unique values of \code{x}  \cr
   \code{hashupo} \tab \code{\link[=unique.integer64]{unique}} \tab return positions of unique values in hashdat \cr
   \code{hashmapupo} \tab \code{\link[=unique.integer64]{unique}} \tab return positions of unique values in \code{x} \cr
   \code{hashtab} \tab \code{\link[=table.integer64]{table}} \tab tabulate values of hashdat using hashmap in \code{keep.order=FALSE} \cr
   \code{hashmaptab} \tab \code{\link[=table.integer64]{table}} \tab tabulate values of \code{x} building hasmap on the fly in \code{keep.order=FALSE}\cr
}
}
\value{
  see details
}
\author{
Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\keyword{ programming }
\keyword{ manip }
\seealso{ \code{\link[=match.integer64]{match}} }
\examples{
x <- as.integer64(sample(c(NA, 0:9)))
y <- as.integer64(sample(c(NA, 1:9), 10, TRUE))
hashfun(y)
hx <- hashmap(x)
hy <- hashmap(y)
ls(hy)
hashpos(hy, x)
hashrev(hx, y)
hashfin(hy, x)
hashrin(hx, y)
hashdup(hy)
hashuni(hy)
hashuni(hy, keep.order=TRUE)
hashmapuni(y)
hashupo(hy)
hashupo(hy, keep.order=TRUE)
hashmapupo(y)
hashtab(hy)
hashmaptab(y)

stopifnot(identical(match(as.integer(x),as.integer(y)),hashpos(hy, x)))
stopifnot(identical(match(as.integer(x),as.integer(y)),hashrev(hx, y)))
stopifnot(identical(as.integer(x) \%in\% as.integer(y), hashfin(hy, x)))
stopifnot(identical(as.integer(x) \%in\% as.integer(y), hashrin(hx, y)))
stopifnot(identical(duplicated(as.integer(y)), hashdup(hy)))
stopifnot(identical(as.integer64(unique(as.integer(y))), hashuni(hy, keep.order=TRUE)))
stopifnot(identical(sort(hashuni(hy, keep.order=FALSE)), sort(hashuni(hy, keep.order=TRUE))))
stopifnot(identical(y[hashupo(hy, keep.order=FALSE)], hashuni(hy, keep.order=FALSE)))
stopifnot(identical(y[hashupo(hy, keep.order=TRUE)], hashuni(hy, keep.order=TRUE)))
stopifnot(identical(hashpos(hy, hashuni(hy, keep.order=TRUE)), hashupo(hy, keep.order=TRUE)))
stopifnot(identical(hashpos(hy, hashuni(hy, keep.order=FALSE)), hashupo(hy, keep.order=FALSE)))
stopifnot(identical(hashuni(hy, keep.order=FALSE), hashtab(hy)$values))
stopifnot(identical(as.vector(table(as.integer(y), useNA="ifany"))
, hashtab(hy)$counts[order.integer64(hashtab(hy)$values)]))
stopifnot(identical(hashuni(hy, keep.order=TRUE), hashmapuni(y)))
stopifnot(identical(hashupo(hy, keep.order=TRUE), hashmapupo(y)))
stopifnot(identical(hashtab(hy), hashmaptab(y)))

	\dontrun{
	message("explore speed given size of the hasmap in 2^hashbits and size of the data")
	message("more hashbits means more random access and less collisions")
	message("i.e. more data means less random access and more collisions")
	bits <- 24
	b <- seq(-1, 0, 0.1)
	tim <- matrix(NA, length(b), 2, dimnames=list(b, c("bits","bits+1")))
    for (i in 1:length(b)){
	  n <- as.integer(2^(bits+b[i]))
	  x <- as.integer64(sample(n))
	  tim[i,1] <- repeat.time(hashmap(x, hashbits=bits))[3]
	  tim[i,2] <- repeat.time(hashmap(x, hashbits=bits+1))[3]
	  print(tim)
      matplot(b, tim)
	}
	message("we conclude that n*sqrt(2) is enough to avoid collisions")
	}
}
