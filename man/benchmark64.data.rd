\name{benchmark64.data}
\alias{benchmark64.data}
\docType{data}
\title{
 Results of performance measurement on a Core i7 Lenovo T410 8 GB RAM under Windows 7 64bit
}
\description{
  These are the results of calling \code{\link{benchmark64}}
}
\usage{data(benchmark64.data)}
\format{
  The format is:
 num [1:16, 1:6] 2.55e-05 2.37 2.39 1.28 1.39 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:16] "cache" "match(s,b)" "s \%in\% b" "match(b,s)" ...
  ..$ : chr [1:6] "32-bit" "64-bit" "hashcache" "sortordercache" ...
}
\examples{
data(benchmark64.data)
print(benchmark64.data)
matplot(log2(benchmark64.data[-1,1]/benchmark64.data[-1,])
, pch=c("3", "6", "h", "s", "o", "a")
, xlab="tasks [last=session]"
, ylab="log2(relative speed) [bigger is better]"
)
matplot(t(log2(benchmark64.data[-1,1]/benchmark64.data[-1,]))
, axes=FALSE
, type="b"
, lwd=c(rep(1, 14), 3)
, xlab="context"
, ylab="log2(relative speed) [bigger is better]"
)
axis(1
, labels=c("32-bit", "64-bit", "hash", "sortorder", "order", "hash+sortorder")
, at=1:6
)
axis(2)
}
\keyword{datasets}
