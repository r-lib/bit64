\name{benchmark64}
\alias{benchmark64}
\alias{optimizer64}
\title{
 Function for measuring algorithmic performance \cr 
 of high-level and low-level integer64 functions
}
\description{
 \code{benchmark64} compares high-level integer64 functions against the integer functions from Base R \cr
 \code{optimizer64} compares for each high-level integer64 function the Base R integer function with several low-level integer64 functions with and without caching \cr
}
\usage{
benchmark64(nsmall = 2^16, nbig = 2^25, timefun = repeat.time
)
optimizer64(nsmall = 2^16, nbig = 2^25, timefun = repeat.time
, what = c("match", "\%in\%", "duplicated", "unique", "unipos", "table", "rank", "quantile")
, uniorder = c("original", "values", "any")
, taborder = c("values", "counts")
, plot = TRUE
)
}
\arguments{
  \item{nsmall}{ size of smaller vector }
  \item{nbig}{ size of larger bigger vector }
  \item{timefun}{ a function for timing such as \code{\link[bit]{repeat.time}} or \code{\link{system.time}} }
  \item{what}{
 a vector of names of high-level functions
}
  \item{uniorder}{
 one of the order parameters that are allowed in \code{\link{unique.integer64}} and \code{\link{unipos.integer64}}
}
  \item{taborder}{
 one of the order parameters that are allowed in \code{\link{table.integer64}} 
}
  \item{plot}{
 set to FALSE to suppress plotting 
}
}
\details{
 \code{benchmark64} compares the following scenarios for the following use cases: 
 \tabular{rl}{
  \bold{scenario name} \tab \bold{explanation} \cr
  32-bit  \tab applying Base R function to 32-bit integer data \cr
  64-bit \tab applying bit64 function to 64-bit integer data (with no cache) \cr
  hashcache \tab dito when cache contains \code{\link{hashmap}}, see \code{\link{hashcache}} \cr
  sortordercache \tab dito when cache contains sorting and ordering, see \code{\link{sortordercache}} \cr
  ordercache \tab dito when cache contains ordering only, see \code{\link{ordercache}} \cr
  allcache \tab dito when cache contains sorting, ordering and hashing \cr
 }
 \tabular{rl}{
  \bold{use case name} \tab \bold{explanation} \cr
  cache         \tab filling the cache according to scenario \cr
  match(s,b)    \tab match small in big vector \cr
  s \%in\% b      \tab small \%in\% big vector \cr
  match(b,s)    \tab match big in small vector \cr
  b \%in\% s      \tab big \%in\% small vector \cr
  match(b,b)    \tab match big in (different) big vector \cr
  b \%in\% b      \tab big \%in\% (different) big vector \cr
  duplicated(b) \tab duplicated of big vector \cr
  unique(b)     \tab unique of big vector \cr
  table(b)      \tab table of big vector \cr
  sort(b)       \tab sorting of big vector \cr
  order(b)      \tab ordering of big vector \cr
  rank(b)       \tab ranking of big vector \cr
  quantile(b)   \tab quantiles of big vector \cr
  summary(b)    \tab summary of of big vector \cr
  SESSION       \tab exemplary session involving multiple calls (including cache filling costs) \cr
 }
 Note that the timings for the cached variants do \emph{not} contain the time costs of building the cache, except for the timing of the exemplary user session, where the cache costs are included in order to evaluate amortization. 
}
\value{
 \code{benchmark64} returns a matrix with elapsed seconds, different high-level tasks in rows and different scenarios to solve the task in columns. The last row named 'SESSION' contains the elapsed seconds of the exemplary sesssion.
 \cr
 \code{optimizer64} returns a dimensioned list with one row for each high-level function timed and two columns named after the values of the \code{nsmall} and \code{nbig} sample sizes. Each list cell contains a matrix with timings, low-level-methods in rows and three measurements \code{c("prep","both","use")} in columns. If it can be measured separately, \code{prep} contains the timing of preparatory work such as sorting and hashing, and \code{use} contains the timing of using the prepared work. If the function timed does both, preparation and use, the timing is in \code{both}.  
}
\author{
 Jens Oehlschlägel <Jens.Oehlschlaegel@truecluster.com>
}
\seealso{
 \code{\link{integer64}}
}
\examples{
message("this small example using system.time does not give serious timings\n
this we do this only to run regression tests")
benchmark64(nsmall=2^7, nbig=2^13, timefun=function(expr)system.time(expr, gcFirst=FALSE))
optimizer64(nsmall=2^7, nbig=2^13, timefun=function(expr)system.time(expr, gcFirst=FALSE)
, plot=FALSE
)
\dontrun{
message("for real measurement of sufficiently large datasets run this on your machine")
benchmark64()
optimizer64()
}
message("let's look at the performance results on Core i7 Lenovo T410 with 8 GB RAM")
data(benchmark64.data)
print(benchmark64.data)

matplot(log2(benchmark64.data[-1,1]/benchmark64.data[-1,])
, pch=c("3", "6", "h", "s", "o", "a") 
, xlab="tasks [last=session]"
, ylab="log2(relative speed) [bigger is better]"
)
matplot(t(log2(benchmark64.data[-1,1]/benchmark64.data[-1,]))
, type="b", axes=FALSE 
, lwd=c(rep(1, 14), 3)
, xlab="context"
, ylab="log2(relative speed) [bigger is better]"
)
axis(1
, labels=c("32-bit", "64-bit", "hash", "sortorder", "order", "hash+sortorder")
, at=1:6
)
axis(2)
data(optimizer64.data)
print(optimizer64.data)
oldpar <- par(no.readonly = TRUE)
par(mfrow=c(2,1))
par(cex=0.7)
for (i in 1:nrow(optimizer64.data)){
 for (j in 1:2){
   tim <- optimizer64.data[[i,j]]
  barplot(t(tim))
  if (rownames(optimizer64.data)[i]=="match")
   title(paste("match", colnames(optimizer64.data)[j], "in", colnames(optimizer64.data)[3-j]))
  else if (rownames(optimizer64.data)[i]=="\%in\%")
   title(paste(colnames(optimizer64.data)[j], "\%in\%", colnames(optimizer64.data)[3-j]))
  else
   title(paste(rownames(optimizer64.data)[i], colnames(optimizer64.data)[j]))
 }
}
par(mfrow=c(1,1))
}
\keyword{ misc }
