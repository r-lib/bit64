\name{table.integer64}
\title{Cross Tabulation and Table Creation for integer64}
\alias{table.integer64}

\concept{counts}
\concept{frequencies}
\concept{occurrences}
\concept{contingency table}

\description{
  \code{table.integer64} uses the cross-classifying integer64 vectors to build a contingency
  table of the counts at each combination of vector values.
}
\usage{
table.integer64(\dots
, return = c("table","data.frame","list")
, order = c("values","counts")
, nunique = NULL
, method = NULL
, dnn = list.names(...), deparse.level = 1
) 
}
\arguments{
  \item{\dots}{one or more objects which can be interpreted as factors
    (including character strings), or a list (or data frame) whose
    components can be so interpreted.  (For \code{as.table} and
    \code{as.data.frame}, arguments passed to specific methods.)}
  \item{nunique}{
	NULL or the number of unique values of table (including NA). Providing \code{nunique} can speed-up matching when \code{table} has no cache. Note that a wrong nunique can cause undefined behaviour up to a crash.
}
  \item{order}{
	By default results are created sorted by "values", or by "counts"
}
  \item{method}{
	NULL for automatic method selection or a suitable low-level method, see details
}
  \item{return}{
     choose the return format, see details
}
  \item{dnn}{the names to be given to the dimensions in the result (the
    \emph{dimnames names}).}
  \item{deparse.level}{controls how the default \code{dnn} is
    constructed.  See \sQuote{Details}.}
}
\details{
  This function automatically chooses from several low-level functions considering the size of \code{x} and the availability of a cache. 
  Suitable methods are \code{\link{hashmaptab}} (simultaneously creating and using a hashmap)
, \code{\link{hashtab}} (first creating a hashmap then using it)
, \code{\link{sortordertab}} (fast ordering) 
and \code{\link{ordertab}} (memory saving ordering).
\cr
  If the argument \code{dnn} is not supplied, the internal function
  \code{list.names} is called to compute the \sQuote{dimname names}.  If the
  arguments in \code{\dots} are named, those names are used.  For the
  remaining arguments, \code{deparse.level = 0} gives an empty name,
  \code{deparse.level = 1} uses the supplied argument if it is a symbol,
  and \code{deparse.level = 2} will deparse the argument.

  Arguments \code{exclude}, \code{useNA}, are not supported, i.e. \code{NA}s are always tabulated, and, different from \code{\link{table}} they are sorted first if \code{order="values"}. 
}
\value{
  By default (with \code{return="table"}) \code{\link{table}} returns a \emph{contingency table}, an object of
  class \code{"table"}, an array of integer values. Note that unlike S the result is always an array, a 1D array if one factor is given. Note also that for multidimensional arrays this is a \emph{dense} return structure which can dramatically increase RAM requirements (for large arrays with high mutual information, i.e. many possible input combinations of which only few occur) and that \code{\link{table}} is limited to \code{2^31} possible combinations (e.g. two input vectors with 46340 unique values only). Finally note that the tabulated values or value-combinations are represented as \code{dimnames} and that the implied conversion of values to strings can cause \emph{severe} performance problems since each string needs to be integrated into R's global string cache. 
  \cr
  You can use the other \code{return=} options to cope with these problems, the potential combination limit is increased from \code{2^31} to \code{2^63} with these options, RAM is only rewquired for observed combinations and string conversion is avoided. 
  \cr
  With \code{return="data.frame"} you get a \emph{dense} representation as a \code{\link{data.frame}} (like that resulting from \code{as.data.frame(table(...))}) where only observed combinations are listed (each as a data.frame row) with the corresponding frequency counts (the latter as component
  named by \code{responseName}).  This is the inverse of \code{\link{xtabs}}..
  \cr
  With \code{return="list"} you also get a \emph{dense} representation as a simple \code{\link{list}} with components 
  \item{values }{a integer64 vector of the technically tabulated values, for 1D this is the tabulated values themselves, for kD these are the values representing the potential combinations of input values}
  \item{counts}{the frequency counts}
  \item{dims}{only for kD: a list with the vectors of the unique values of the input dimensions}
}
\note{
  Note that by using \code{\link{as.integer64.factor}} we can also input 
  factors into \code{table.integer64} -- only the \code{\link{levels}} get lost.
 \cr
  Note that because of the existence of \code{\link{as.factor.integer64}} 
the standard \code{\link{table}} function -- within its limits -- can also be used 
for \code{\link{integer64}}, and especially for combining \code{\link{integer64}} input 
with other data types.
}
\seealso{
  \code{\link{table}} for more info on the standard version coping with Base R's data types, \code{\link{tabulate}} which can faster tabulate \code{\link{integer}s} with a limited range \code{[1L .. nL not too big]}, \code{\link{unique.integer64}} for the unique values without counting them and \code{\link{unipos.integer64}} for the positions of the unique values. 
}
\examples{
message("pure integer64 examples")
x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
y <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
z <- sample(c(rep(NA, 9), letters), 32, TRUE)
table.integer64(x)
table.integer64(x, order="counts")
table.integer64(x, y)
table.integer64(x, y, return="data.frame")

message("via as.integer64.factor we can use 'table.integer64' also for factors")
table.integer64(x, as.integer64(as.factor(z)))

message("via as.factor.integer64 we can also use 'table' for integer64")
table(x)
table(x, exclude=NULL)
table(x, z, exclude=NULL)

\dontshow{
 stopifnot(identical(table.integer64(as.integer64(c(1,1,2))), table(c(1,1,2))))
 stopifnot(identical(table.integer64(as.integer64(c(1,1,2)),as.integer64(c(3,4,4))), table(c(1,1,2),c(3,4,4))))
 message("the following works with three warnings due to coercion")
 stopifnot(identical(table.integer64(c(1,1,2)), table(c(1,1,2))))
 stopifnot(identical(table.integer64(as.integer64(c(1,1,2)),c(3,4,4)), table(c(1,1,2),c(3,4,4))))
 stopifnot(identical(table.integer64(c(1,1,2),as.integer64(c(3,4,4))), table(c(1,1,2),c(3,4,4))))
 message("the following works because of as.factor.integer64")
 stopifnot(identical(table(as.integer64(c(1,1,2))), table(c(1,1,2))))  
 stopifnot(identical(table(as.integer64(c(1,1,2)),as.integer64(c(3,4,4))), table(c(1,1,2),c(3,4,4))))
 stopifnot(identical(table(as.integer64(c(1,1,2)),c(3,4,4)), table(c(1,1,2),c(3,4,4))))
 stopifnot(identical(table(c(1,1,2),as.integer64(c(3,4,4))), table(c(1,1,2),c(3,4,4))))
}

}
\keyword{category}
