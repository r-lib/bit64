\name{sortnut}
\alias{sortnut}
\alias{sortnut.integer64}
\alias{ordernut}
\alias{ordernut.integer64}
\alias{sortfin}
\alias{sortfin.integer64}
\alias{orderpos}
\alias{orderpos.integer64}
\alias{orderfin}
\alias{orderfin.integer64}
\alias{sortorderpos}
\alias{sortorderpos.integer64}
\alias{orderdup}
\alias{orderdup.integer64}
\alias{sortorderdup}
\alias{sortorderdup.integer64}
\alias{sortuni}
\alias{sortuni.integer64}
\alias{orderuni}
\alias{orderuni.integer64}
\alias{sortorderuni}
\alias{sortorderuni.integer64}
\alias{orderupo}
\alias{orderupo.integer64}
\alias{sortorderupo}
\alias{sortorderupo.integer64}
\alias{ordertie}
\alias{ordertie.integer64}
\alias{sortordertie}
\alias{sortordertie.integer64}
\alias{sorttab}
\alias{sorttab.integer64}
\alias{ordertab}
\alias{ordertab.integer64}
\alias{sortordertab}
\alias{sortordertab.integer64}
\alias{orderkey}
\alias{orderkey.integer64}
\alias{sortorderkey}
\alias{sortorderkey.integer64}
\alias{orderrnk}
\alias{orderrnk.integer64}
\alias{sortorderrnk}
\alias{sortorderrnk.integer64}
\alias{sortqtl}
\alias{sortqtl.integer64}
\alias{orderqtl}
\alias{orderqtl.integer64}
\title{
   Searching and other uses of sorting for 64bit integers
}
\description{
  This is roughly an implementation of hash functionality but based on sorting instead on a hasmap.
  Since sorting is more informative than hashingwe can do some more interesting things.
}
\usage{
sortnut(sorted, \dots)
ordernut(table, order, \dots)
sortfin(sorted, x, \dots)
orderfin(table, order, x, \dots)
orderpos(table, order, x, \dots)
sortorderpos(sorted, order, x, \dots)
orderdup(table, order, \dots)
sortorderdup(sorted, order, \dots)
sortuni(sorted, nunique, \dots)
orderuni(table, order, nunique, \dots)
sortorderuni(table, sorted, order, nunique, \dots)
orderupo(table, order, nunique, \dots)
sortorderupo(sorted, order, nunique, keep.order = FALSE, \dots)
ordertie(table, order, nties, \dots)
sortordertie(sorted, order, nties, \dots)
sorttab(sorted, nunique, \dots)
ordertab(table, order, nunique, \dots)
sortordertab(sorted, order, \dots)
orderkey(table, order, na.skip.num = 0L, \dots)
sortorderkey(sorted, order, na.skip.num = 0L, \dots)
orderrnk(table, order, na.count, \dots)
sortorderrnk(sorted, order, na.count, \dots)
\method{sortnut}{integer64}(sorted, \dots)
\method{ordernut}{integer64}(table, order, \dots)
\method{sortfin}{integer64}(sorted, x, method=NULL, \dots)
\method{orderfin}{integer64}(table, order, x, method=NULL, \dots)
\method{orderpos}{integer64}(table, order, x, nomatch=NA, method=NULL, \dots)
\method{sortorderpos}{integer64}(sorted, order, x, nomatch=NA, method=NULL, \dots)
\method{orderdup}{integer64}(table, order, method=NULL, \dots)
\method{sortorderdup}{integer64}(sorted, order, method=NULL, \dots)
\method{sortuni}{integer64}(sorted, nunique, \dots)
\method{orderuni}{integer64}(table, order, nunique, keep.order=FALSE, \dots)
\method{sortorderuni}{integer64}(table, sorted, order, nunique, \dots)
\method{orderupo}{integer64}(table, order, nunique, keep.order=FALSE, \dots)
\method{sortorderupo}{integer64}(sorted, order, nunique, keep.order = FALSE, \dots)
\method{ordertie}{integer64}(table, order, nties, \dots)
\method{sortordertie}{integer64}(sorted, order, nties, \dots)
\method{sorttab}{integer64}(sorted, nunique, \dots)
\method{ordertab}{integer64}(table, order, nunique, denormalize=FALSE, keep.order=FALSE, \dots)
\method{sortordertab}{integer64}(sorted, order, denormalize=FALSE, \dots)
\method{orderkey}{integer64}(table, order, na.skip.num = 0L, \dots)
\method{sortorderkey}{integer64}(sorted, order, na.skip.num = 0L, \dots)
\method{orderrnk}{integer64}(table, order, na.count, \dots)
\method{sortorderrnk}{integer64}(sorted, order, na.count, \dots)
\method{sortqtl}{integer64}(sorted, na.count, probs, \dots)
\method{orderqtl}{integer64}(table, order, na.count, probs, \dots)
}
\arguments{
  \item{x}{ an \code{\link{integer64}} vector }
  \item{sorted}{ a sorted \code{\link{integer64}} vector }
  \item{table}{ the original data with original order under the sorted vector }
  \item{order}{ an \code{\link{integer}} order vector that turns 'table' into 'sorted' }
  \item{nunique}{ number of unique elements, usually we get this from cache or call \code{sortnut} or \code{ordernut} }
  \item{nties}{ number of tied values, usually we get this from cache or call \code{sortnut} or \code{ordernut} }
  \item{denormalize}{ FALSE returns counts of unique values, TRUE returns each value with its counts }
  \item{nomatch}{ the value to be returned if an element is not found in the hashmap }
  \item{keep.order}{ determines order of results and speed: \code{FALSE} (the default) is faster and returns in sorted order, \code{TRUE} returns in the order of first appearance in the original data, but this requires extra work } 
  \item{probs}{ vector of probabilities in [0..1] for which we seek quantiles }
  \item{na.skip.num}{ 0 or the number of \code{NA}s. With 0, \code{NA}s are coded with 1L, with the number of \code{NA}s, these are coded with \code{NA}, the latter needed for \code{\link{as.factor.integer64}} }
  \item{na.count}{ the number of \code{NA}s, needed for this low-level function algorithm }
  \item{method}{ see details }
  \item{\dots}{ further arguments, passed from generics, ignored in methods }
}
\details{
\tabular{rrrrl}{
   \bold{sortfun} \tab \bold{orderfun} \tab \bold{sortorderfun} \tab \bold{see also}          \tab \bold{description} \cr
   \code{sortnut} \tab \code{ordernut} \tab                     \tab  \tab return number of tied and of unique values \cr
   \code{sortfin} \tab \code{orderfin} \tab                     \tab \code{\link{\%in\%.integer64}} \tab return logical whether \code{x} is in \code{table} \cr
                  \tab \code{orderpos} \tab \code{sortorderpos} \tab \code{\link[=match.integer64]{match}} \tab return positions of \code{x} in \code{table} \cr
                  \tab \code{orderdup} \tab \code{sortorderdup} \tab \code{\link[=duplicated.integer64]{duplicated}} \tab return logical whether values are duplicated \cr
   \code{sortuni} \tab \code{orderuni} \tab \code{sortorderuni} \tab \code{\link[=unique.integer64]{unique}} \tab return unique values (=dimensiontable) \cr
                  \tab \code{orderupo} \tab \code{sortorderupo} \tab \code{\link[=unique.integer64]{unique}} \tab return positions of unique values \cr
                  \tab \code{ordertie} \tab \code{sortordertie} \tab  \tab return positions of tied values \cr
                  \tab \code{orderkey} \tab \code{sortorderkey} \tab  \tab positions of values in vector of unique values (match in dimensiontable) \cr
   \code{sorttab} \tab \code{ordertab} \tab \code{sortordertab} \tab \code{\link[=table.integer64]{table}} \tab tabulate frequency of values  \cr
                  \tab \code{orderrnk} \tab \code{sortorderrnk} \tab  \tab rank averaging ties \cr
   \code{sortqtl} \tab \code{orderqtl} \tab                     \tab  \tab return quantiles given probabilities \cr
}
The functions \code{sortfin}, \code{orderfin}, \code{orderpos} and \code{sortorderpos} each offer three algorithms for finding \code{x} in \code{table}.  \cr
With \code{method=1L} each value of \code{x} is searched independently using \emph{binary search}, this is fastest for small \code{table}s. \cr
With \code{method=2L} the values of \code{x} are first sorted and then searched using \emph{doubly exponential search}, this is the best allround method. \cr
With \code{method=3L} the values of \code{x} are first sorted and then searched using simple merging, this is the fastest method if \code{table} is huge and \code{x} has similar size and distribution of values. \cr
With \code{method=NULL} the functions use a heuristic to determine the fastest algorithm. \cr

The functions \code{orderdup} and \code{sortorderdup} each offer two algorithms for setting the truth values in the return vector.  \cr
With \code{method=1L} the return values are set directly which causes random write access on a possibly large return vector. \cr
With \code{method=2L} the return values are first set in a smaller bit-vector -- random access limited to a smaller memory region -- and finally written sequentially to the logical output  vector. \cr
With \code{method=NULL} the functions use a heuristic to determine the fastest algorithm. \cr
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
 message("check the code of 'optimizer64' for examples:")
 print(optimizer64)
}
