# Searching and other uses of sorting for 64bit integers

This is roughly an implementation of hash functionality but based on
sorting instead on a hashmap. Since sorting is more informative than
hashing we can do some more interesting things.

## Usage

``` r
sortnut(sorted, ...)

# S3 method for class 'integer64'
sortnut(sorted, ...)

ordernut(table, order, ...)

# S3 method for class 'integer64'
ordernut(table, order, ...)

sortfin(sorted, x, ...)

# S3 method for class 'integer64'
sortfin(sorted, x, method = NULL, ...)

orderfin(table, order, x, ...)

# S3 method for class 'integer64'
orderfin(table, order, x, method = NULL, ...)

orderpos(table, order, x, ...)

# S3 method for class 'integer64'
orderpos(table, order, x, nomatch = NA, method = NULL, ...)

sortorderpos(sorted, order, x, ...)

# S3 method for class 'integer64'
sortorderpos(sorted, order, x, nomatch = NA, method = NULL, ...)

orderdup(table, order, ...)

# S3 method for class 'integer64'
orderdup(table, order, method = NULL, ...)

sortorderdup(sorted, order, ...)

# S3 method for class 'integer64'
sortorderdup(sorted, order, method = NULL, ...)

sortuni(sorted, nunique, ...)

# S3 method for class 'integer64'
sortuni(sorted, nunique, ...)

orderuni(table, order, nunique, ...)

# S3 method for class 'integer64'
orderuni(table, order, nunique, keep.order = FALSE, ...)

sortorderuni(table, sorted, order, nunique, ...)

# S3 method for class 'integer64'
sortorderuni(table, sorted, order, nunique, ...)

orderupo(table, order, nunique, ...)

# S3 method for class 'integer64'
orderupo(table, order, nunique, keep.order = FALSE, ...)

sortorderupo(sorted, order, nunique, keep.order = FALSE, ...)

# S3 method for class 'integer64'
sortorderupo(sorted, order, nunique, keep.order = FALSE, ...)

ordertie(table, order, nties, ...)

# S3 method for class 'integer64'
ordertie(table, order, nties, ...)

sortordertie(sorted, order, nties, ...)

# S3 method for class 'integer64'
sortordertie(sorted, order, nties, ...)

sorttab(sorted, nunique, ...)

# S3 method for class 'integer64'
sorttab(sorted, nunique, ...)

ordertab(table, order, nunique, ...)

# S3 method for class 'integer64'
ordertab(table, order, nunique, denormalize = FALSE, keep.order = FALSE, ...)

sortordertab(sorted, order, ...)

# S3 method for class 'integer64'
sortordertab(sorted, order, denormalize = FALSE, ...)

orderkey(table, order, na.skip.num = 0L, ...)

# S3 method for class 'integer64'
orderkey(table, order, na.skip.num = 0L, ...)

sortorderkey(sorted, order, na.skip.num = 0L, ...)

# S3 method for class 'integer64'
sortorderkey(sorted, order, na.skip.num = 0L, ...)

orderrnk(table, order, na.count, ...)

# S3 method for class 'integer64'
orderrnk(table, order, na.count, ...)

sortorderrnk(sorted, order, na.count, ...)

# S3 method for class 'integer64'
sortorderrnk(sorted, order, na.count, ...)

sortqtl(sorted, na.count, probs, ...)

# S3 method for class 'integer64'
sortqtl(sorted, na.count, probs, ...)

orderqtl(table, order, na.count, probs, ...)

# S3 method for class 'integer64'
orderqtl(table, order, na.count, probs, ...)
```

## Arguments

- sorted:

  a sorted
  [`integer64`](https://bit64.r-lib.org/reference/bit64-package.md)
  vector

- ...:

  further arguments, passed from generics, ignored in methods

- table:

  the original data with original order under the sorted vector

- order:

  an [`integer`](https://rdrr.io/r/base/integer.html) order vector that
  turns 'table' into 'sorted'

- x:

  an [`integer64`](https://bit64.r-lib.org/reference/bit64-package.md)
  vector

- method:

  see Details

- nomatch:

  the value to be returned if an element is not found in the hashmap

- nunique:

  number of unique elements, usually we get this from cache or call
  `sortnut` or `ordernut`

- keep.order:

  determines order of results and speed: `FALSE` (the default) is faster
  and returns in sorted order, `TRUE` returns in the order of first
  appearance in the original data, but this requires extra work

- nties:

  number of tied values, usually we get this from cache or call
  `sortnut` or `ordernut`

- denormalize:

  FALSE returns counts of unique values, TRUE returns each value with
  its counts

- na.skip.num:

  0 or the number of `NA`s. With 0, `NA`s are coded with 1L, with the
  number of `NA`s, these are coded with `NA`

- na.count:

  the number of `NA`s, needed for this low-level function algorithm

- probs:

  vector of probabilities in `[0..1]` for which we seek quantiles

## Value

see details

## Details

|             |              |                  |                                                                             |                                                                          |
|-------------|--------------|------------------|-----------------------------------------------------------------------------|--------------------------------------------------------------------------|
| **sortfun** | **orderfun** | **sortorderfun** | **see also**                                                                | **description**                                                          |
| `sortnut`   | `ordernut`   |                  |                                                                             | return number of tied and of unique values                               |
| `sortfin`   | `orderfin`   |                  | [`%in%.integer64`](https://bit64.r-lib.org/reference/match.integer64.md)    | return logical whether `x` is in `table`                                 |
|             | `orderpos`   | `sortorderpos`   | [`match()`](https://bit64.r-lib.org/reference/match.integer64.md)           | return positions of `x` in `table`                                       |
|             | `orderdup`   | `sortorderdup`   | [`duplicated()`](https://bit64.r-lib.org/reference/duplicated.integer64.md) | return logical whether values are duplicated                             |
| `sortuni`   | `orderuni`   | `sortorderuni`   | [`unique()`](https://bit64.r-lib.org/reference/unique.integer64.md)         | return unique values (=dimensiontable)                                   |
|             | `orderupo`   | `sortorderupo`   | [`unique()`](https://bit64.r-lib.org/reference/unique.integer64.md)         | return positions of unique values                                        |
|             | `ordertie`   | `sortordertie`   |                                                                             | return positions of tied values                                          |
|             | `orderkey`   | `sortorderkey`   |                                                                             | positions of values in vector of unique values (match in dimensiontable) |
| `sorttab`   | `ordertab`   | `sortordertab`   | [`table()`](https://bit64.r-lib.org/reference/table.integer64.md)           | tabulate frequency of values                                             |
|             | `orderrnk`   | `sortorderrnk`   |                                                                             | rank averaging ties                                                      |
| `sortqtl`   | `orderqtl`   |                  |                                                                             | return quantiles given probabilities                                     |

The functions `sortfin`, `orderfin`, `orderpos` and `sortorderpos` each
offer three algorithms for finding `x` in `table`.

With `method=1L` each value of `x` is searched independently using
*binary search*, this is fastest for small `table`s.

With `method=2L` the values of `x` are first sorted and then searched
using *doubly exponential search*, this is the best all-around method.

With `method=3L` the values of `x` are first sorted and then searched
using simple merging, this is the fastest method if `table` is huge and
`x` has similar size and distribution of values.

With `method=NULL` the functions use a heuristic to determine the
fastest algorithm.

The functions `orderdup` and `sortorderdup` each offer two algorithms
for setting the truth values in the return vector.

With `method=1L` the return values are set directly which causes random
write access on a possibly large return vector.

With `method=2L` the return values are first set in a smaller bit-vector
– random access limited to a smaller memory region – and finally written
sequentially to the logical output vector.

With `method=NULL` the functions use a heuristic to determine the
fastest algorithm.

## See also

[`match()`](https://bit64.r-lib.org/reference/match.integer64.md)

## Examples

``` r
 message("check the code of 'optimizer64' for examples:")
#> check the code of 'optimizer64' for examples:
 print(optimizer64)
#> function (nsmall = 2L^16L, nbig = 2L^25L, timefun = repeat.time, 
#>     what = c("match", "%in%", "duplicated", "unique", "unipos", 
#>         "table", "rank", "quantile"), uniorder = c("original", 
#>         "values", "any"), taborder = c("values", "counts"), plot = TRUE) 
#> {
#>     uniorder <- match.arg(uniorder)
#>     taborder <- match.arg(taborder)
#>     ret <- vector("list", 2L * length(what))
#>     dim(ret) <- c(length(what), 2L)
#>     dimnames(ret) <- list(what, c(nsmall, nbig))
#>     if (plot) {
#>         oldpar <- par(no.readonly = TRUE)
#>         on.exit(par(oldpar))
#>         par(mfrow = c(2L, 1L))
#>     }
#>     if ("match" %in% what) {
#>         message("match: timings of different methods")
#>         N1 <- c(nsmall, nbig)
#>         N2 <- c(nbig, nsmall)
#>         for (i in seq_along(N1)) {
#>             n1 <- N1[i]
#>             n2 <- N2[i]
#>             x1 <- c(sample(n2, n1 - 1L, TRUE), NA)
#>             x2 <- c(sample(n2, n2 - 1L, TRUE), NA)
#>             tim <- matrix(0, 9L, 3L)
#>             dimnames(tim) <- list(c("match", "match.64", "hashpos", 
#>                 "hashrev", "sortorderpos", "orderpos", "hashcache", 
#>                 "sortorder.cache", "order.cache"), c("prep", 
#>                 "both", "use"))
#>             tim["match", "both"] <- timefun({
#>                 p <- match(x1, x2)
#>             })[3L]
#>             x1 <- as.integer64(x1)
#>             x2 <- as.integer64(x2)
#>             tim["match.64", "both"] <- timefun({
#>                 p2 <- match.integer64(x1, x2)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             tim["hashpos", "prep"] <- timefun({
#>                 h2 <- hashmap(x2)
#>             })[3L]
#>             tim["hashpos", "use"] <- timefun({
#>                 p2 <- hashpos(h2, x1)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             tim["hashrev", "prep"] <- timefun({
#>                 h1 <- hashmap(x1)
#>             })[3L]
#>             tim["hashrev", "use"] <- timefun({
#>                 p1 <- hashrev(h1, x2)
#>             })[3L]
#>             stopifnot(identical(p1, p))
#>             tim["sortorderpos", "prep"] <- system.time({
#>                 s2 <- clone(x2)
#>                 o2 <- seq_along(x2)
#>                 ramsortorder(s2, o2, na.last = FALSE)
#>             })[3L]
#>             tim["sortorderpos", "use"] <- timefun({
#>                 p2 <- sortorderpos(s2, o2, x1)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             tim["orderpos", "prep"] <- timefun({
#>                 o2 <- seq_along(x2)
#>                 ramorder(x2, o2, na.last = FALSE)
#>             })[3L]
#>             tim["orderpos", "use"] <- timefun({
#>                 p2 <- orderpos(x2, o2, x1, method = 2L)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             hashcache(x2)
#>             tim["hashcache", "use"] <- timefun({
#>                 p2 <- match.integer64(x1, x2)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             remcache(x2)
#>             sortordercache(x2)
#>             tim["sortorder.cache", "use"] <- timefun({
#>                 p2 <- match.integer64(x1, x2)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             remcache(x2)
#>             ordercache(x2)
#>             tim["order.cache", "use"] <- timefun({
#>                 p2 <- match.integer64(x1, x2)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             remcache(x2)
#>             if (plot) {
#>                 barplot(t(tim))
#>                 n <- format(c(n1, n2))
#>                 title(paste("match", n[1L], "in", n[2L]))
#>             }
#>             ret[["match", as.character(n1)]] <- tim
#>         }
#>     }
#>     if ("%in%" %in% what) {
#>         message("%in%: timings of different methods")
#>         N1 <- c(nsmall, nbig)
#>         N2 <- c(nbig, nsmall)
#>         for (i in seq_along(N1)) {
#>             n1 <- N1[i]
#>             n2 <- N2[i]
#>             x1 <- c(sample(n2, n1 - 1L, TRUE), NA)
#>             x2 <- c(sample(n2, n2 - 1L, TRUE), NA)
#>             tim <- matrix(0, 10L, 3L)
#>             dimnames(tim) <- list(c("%in%", "match.64", "%in%.64", 
#>                 "hashfin", "hashrin", "sortfin", "orderfin", 
#>                 "hash.cache", "sortorder.cache", "order.cache"), 
#>                 c("prep", "both", "use"))
#>             tim["%in%", "both"] <- timefun({
#>                 p <- x1 %in% x2
#>             })[3L]
#>             x1 <- as.integer64(x1)
#>             x2 <- as.integer64(x2)
#>             tim["match.64", "both"] <- timefun({
#>                 p2 <- match.integer64(x1, x2, nomatch = 0L) > 
#>                   0L
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             tim["%in%.64", "both"] <- timefun({
#>                 p2 <- `%in%.integer64`(x1, x2)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             tim["hashfin", "prep"] <- timefun({
#>                 h2 <- hashmap(x2)
#>             })[3L]
#>             tim["hashfin", "use"] <- timefun({
#>                 p2 <- hashfin(h2, x1)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             tim["hashrin", "prep"] <- timefun({
#>                 h1 <- hashmap(x1)
#>             })[3L]
#>             tim["hashrin", "use"] <- timefun({
#>                 p1 <- hashrin(h1, x2)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             tim["sortfin", "prep"] <- timefun({
#>                 s2 <- clone(x2)
#>                 ramsort(s2, na.last = FALSE)
#>             })[3L]
#>             tim["sortfin", "use"] <- timefun({
#>                 p2 <- sortfin(s2, x1)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             tim["orderfin", "prep"] <- timefun({
#>                 o2 <- seq_along(x2)
#>                 ramorder(x2, o2, na.last = FALSE)
#>             })[3L]
#>             tim["orderfin", "use"] <- timefun({
#>                 p2 <- orderfin(x2, o2, x1)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             hashcache(x2)
#>             tim["hash.cache", "use"] <- timefun({
#>                 p2 <- `%in%.integer64`(x1, x2)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             remcache(x2)
#>             sortordercache(x2)
#>             tim["sortorder.cache", "use"] <- timefun({
#>                 p2 <- `%in%.integer64`(x1, x2)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             remcache(x2)
#>             ordercache(x2)
#>             tim["order.cache", "use"] <- timefun({
#>                 p2 <- `%in%.integer64`(x1, x2)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             remcache(x2)
#>             if (plot) {
#>                 barplot(t(tim))
#>                 n <- format(c(n1, n2))
#>                 title(paste(n[1L], "%in%", n[2L]))
#>             }
#>             ret[["%in%", as.character(n1)]] <- tim
#>         }
#>     }
#>     if ("duplicated" %in% what) {
#>         message("duplicated: timings of different methods")
#>         N <- c(nsmall, nbig)
#>         for (i in seq_along(N)) {
#>             n <- N[i]
#>             x <- c(sample(n, n - 1L, TRUE), NA)
#>             tim <- matrix(0, 10L, 3L)
#>             dimnames(tim) <- list(c("duplicated", "duplicated.64", 
#>                 "hashdup", "sortorderdup1", "sortorderdup2", 
#>                 "orderdup1", "orderdup2", "hash.cache", "sortorder.cache", 
#>                 "order.cache"), c("prep", "both", "use"))
#>             tim["duplicated", "both"] <- timefun({
#>                 p <- duplicated(x)
#>             })[3L]
#>             x <- as.integer64(x)
#>             tim["duplicated.64", "both"] <- timefun({
#>                 p2 <- duplicated(x)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             tim["hashdup", "prep"] <- timefun({
#>                 h <- hashmap(x)
#>             })[3L]
#>             tim["hashdup", "use"] <- timefun({
#>                 p2 <- hashdup(h)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             tim["sortorderdup1", "prep"] <- timefun({
#>                 s <- clone(x)
#>                 o <- seq_along(x)
#>                 ramsortorder(s, o, na.last = FALSE)
#>                 nunique <- sortnut(s)[1L]
#>             })[3L]
#>             tim["sortorderdup1", "use"] <- timefun({
#>                 p2 <- sortorderdup(s, o, method = 1L)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             tim["sortorderdup2", "prep"] <- tim["sortorderdup1", 
#>                 "prep"]
#>             tim["sortorderdup2", "use"] <- timefun({
#>                 p2 <- sortorderdup(s, o, method = 2L)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             tim["orderdup1", "prep"] <- timefun({
#>                 o <- seq_along(x)
#>                 ramorder(x, o, na.last = FALSE)
#>                 nunique <- ordernut(x, o)[1L]
#>             })[3L]
#>             tim["orderdup1", "use"] <- timefun({
#>                 p2 <- orderdup(x, o, method = 1L)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             tim["orderdup2", "prep"] <- tim["orderdup1", "prep"]
#>             tim["orderdup2", "use"] <- timefun({
#>                 p2 <- orderdup(x, o, method = 2L)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             hashcache(x)
#>             tim["hash.cache", "use"] <- timefun({
#>                 p2 <- duplicated(x)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             remcache(x)
#>             sortordercache(x)
#>             tim["sortorder.cache", "use"] <- timefun({
#>                 p2 <- duplicated(x)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             remcache(x)
#>             ordercache(x)
#>             tim["order.cache", "use"] <- timefun({
#>                 p2 <- duplicated(x)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             remcache(x)
#>             if (plot) {
#>                 barplot(t(tim), cex.names = 0.7)
#>                 title(paste0("duplicated(", n, ")"))
#>             }
#>             ret[["duplicated", as.character(n)]] <- tim
#>         }
#>     }
#>     if ("unique" %in% what) {
#>         message("unique: timings of different methods")
#>         N <- c(nsmall, nbig)
#>         for (i in seq_along(N)) {
#>             n <- N[i]
#>             x <- c(sample(n, n - 1L, TRUE), NA)
#>             tim <- matrix(0, 15L, 3L)
#>             dimnames(tim) <- list(c("unique", "unique.64", "hashmapuni", 
#>                 "hashuni", "hashunikeep", "sortuni", "sortunikeep", 
#>                 "orderuni", "orderunikeep", "hashdup", "sortorderdup", 
#>                 "hash.cache", "sort.cache", "sortorder.cache", 
#>                 "order.cache"), c("prep", "both", "use"))
#>             tim["unique", "both"] <- timefun({
#>                 p <- unique(x)
#>             })[3L]
#>             x <- as.integer64(x)
#>             p <- as.integer64(p)
#>             if (uniorder == "values") 
#>                 ramsort(p, na.last = FALSE)
#>             tim["unique.64", "both"] <- timefun({
#>                 p2 <- unique(x, order = uniorder)
#>             })[3L]
#>             if (uniorder != "any") 
#>                 stopifnot(identical.integer64(p2, p))
#>             tim["hashmapuni", "both"] <- timefun({
#>                 p2 <- hashmapuni(x)
#>             })[3L]
#>             if (uniorder == "original") 
#>                 stopifnot(identical.integer64(p2, p))
#>             tim["hashuni", "prep"] <- timefun({
#>                 h <- hashmap(x)
#>             })[3L]
#>             tim["hashuni", "use"] <- timefun({
#>                 p2 <- hashuni(h)
#>             })[3L]
#>             if (uniorder == "values") 
#>                 stopifnot(identical.integer64(sort(p2, na.last = FALSE), 
#>                   p))
#>             tim["hashunikeep", "prep"] <- tim["hashuni", "prep"]
#>             tim["hashunikeep", "use"] <- timefun({
#>                 p2 <- hashuni(h, keep.order = TRUE)
#>             })[3L]
#>             if (uniorder == "original") 
#>                 stopifnot(identical.integer64(p2, p))
#>             tim["sortuni", "prep"] <- timefun({
#>                 s <- clone(x)
#>                 ramsort(s, na.last = FALSE)
#>                 nunique <- sortnut(s)[1L]
#>             })[3L]
#>             tim["sortuni", "use"] <- timefun({
#>                 p2 <- sortuni(s, nunique)
#>             })[3L]
#>             if (uniorder == "values") 
#>                 stopifnot(identical.integer64(sort(p2, na.last = FALSE), 
#>                   p))
#>             tim["sortunikeep", "prep"] <- timefun({
#>                 s <- clone(x)
#>                 o <- seq_along(x)
#>                 ramsortorder(s, o, na.last = FALSE)
#>                 nunique <- sortnut(s)[1L]
#>             })[3L]
#>             tim["sortunikeep", "use"] <- timefun({
#>                 p2 <- sortorderuni(x, s, o, nunique)
#>             })[3L]
#>             if (uniorder == "original") 
#>                 stopifnot(identical.integer64(p2, p))
#>             tim["orderuni", "prep"] <- timefun({
#>                 o <- seq_along(x)
#>                 ramorder(x, o, na.last = FALSE)
#>                 nunique <- ordernut(x, o)[1L]
#>             })[3L]
#>             tim["orderuni", "use"] <- timefun({
#>                 p2 <- orderuni(x, o, nunique)
#>             })[3L]
#>             if (uniorder == "values") 
#>                 stopifnot(identical.integer64(sort(p2, na.last = FALSE), 
#>                   p))
#>             tim["orderunikeep", "prep"] <- tim["orderuni", "prep"]
#>             tim["orderunikeep", "use"] <- timefun({
#>                 p2 <- orderuni(x, o, nunique, keep.order = TRUE)
#>                 nunique <- ordernut(x, o)[1L]
#>             })[3L]
#>             if (uniorder == "original") 
#>                 stopifnot(identical.integer64(p2, p))
#>             tim["hashdup", "prep"] <- tim["hashuni", "prep"]
#>             tim["hashdup", "use"] <- timefun({
#>                 p2 <- x[!hashdup(h)]
#>             })[3L]
#>             if (uniorder == "original") 
#>                 stopifnot(identical.integer64(p2, p))
#>             tim["sortorderdup", "prep"] <- tim["sortunikeep", 
#>                 "prep"]
#>             tim["sortorderdup", "use"] <- timefun({
#>                 p2 <- x[!sortorderdup(s, o)]
#>             })[3L]
#>             if (uniorder == "original") 
#>                 stopifnot(identical.integer64(p2, p))
#>             hashcache(x)
#>             tim["hash.cache", "use"] <- timefun({
#>                 p2 <- unique(x, order = uniorder)
#>             })[3L]
#>             if (uniorder != "any") 
#>                 stopifnot(identical.integer64(p2, p))
#>             remcache(x)
#>             sortcache(x)
#>             tim["sort.cache", "use"] <- timefun({
#>                 p2 <- unique(x, order = uniorder)
#>             })[3L]
#>             if (uniorder != "any") 
#>                 stopifnot(identical.integer64(p2, p))
#>             remcache(x)
#>             sortordercache(x)
#>             tim["sortorder.cache", "use"] <- timefun({
#>                 p2 <- unique(x, order = uniorder)
#>             })[3L]
#>             if (uniorder != "any") 
#>                 stopifnot(identical.integer64(p2, p))
#>             remcache(x)
#>             ordercache(x)
#>             tim["order.cache", "use"] <- timefun({
#>                 p2 <- unique(x, order = uniorder)
#>             })[3L]
#>             if (uniorder != "any") 
#>                 stopifnot(identical.integer64(p2, p))
#>             remcache(x)
#>             if (plot) {
#>                 barplot(t(tim), cex.names = 0.7)
#>                 title(paste0("unique(", n, ", order=", uniorder, 
#>                   ")"))
#>             }
#>             ret[["unique", as.character(n)]] <- tim
#>         }
#>     }
#>     if ("unipos" %in% what) {
#>         message("unipos: timings of different methods")
#>         N <- c(nsmall, nbig)
#>         for (i in seq_along(N)) {
#>             n <- N[i]
#>             x <- c(sample(n, n - 1L, TRUE), NA)
#>             tim <- matrix(0, 14L, 3L)
#>             dimnames(tim) <- list(c("unique", "unipos.64", "hashmapupo", 
#>                 "hashupo", "hashupokeep", "sortorderupo", "sortorderupokeep", 
#>                 "orderupo", "orderupokeep", "hashdup", "sortorderdup", 
#>                 "hash.cache", "sortorder.cache", "order.cache"), 
#>                 c("prep", "both", "use"))
#>             tim["unique", "both"] <- timefun({
#>                 unique(x)
#>             })[3L]
#>             x <- as.integer64(x)
#>             tim["unipos.64", "both"] <- timefun({
#>                 p <- unipos(x, order = uniorder)
#>             })[3L]
#>             tim["hashmapupo", "both"] <- timefun({
#>                 p2 <- hashmapupo(x)
#>             })[3L]
#>             if (uniorder == "original") 
#>                 stopifnot(identical(p2, p))
#>             tim["hashupo", "prep"] <- timefun({
#>                 h <- hashmap(x)
#>             })[3L]
#>             tim["hashupo", "use"] <- timefun({
#>                 p2 <- hashupo(h)
#>             })[3L]
#>             if (uniorder == "values") 
#>                 stopifnot(identical(sort(p2, na.last = FALSE), 
#>                   sort(p, na.last = FALSE)))
#>             tim["hashupokeep", "prep"] <- tim["hashupo", "prep"]
#>             tim["hashupokeep", "use"] <- timefun({
#>                 p2 <- hashupo(h, keep.order = TRUE)
#>             })[3L]
#>             if (uniorder == "original") 
#>                 stopifnot(identical(p2, p))
#>             tim["sortorderupo", "prep"] <- timefun({
#>                 s <- clone(x)
#>                 o <- seq_along(x)
#>                 ramsortorder(s, o, na.last = FALSE)
#>                 nunique <- sortnut(s)[1L]
#>             })[3L]
#>             tim["sortorderupo", "use"] <- timefun({
#>                 p2 <- sortorderupo(s, o, nunique)
#>             })[3L]
#>             if (uniorder == "values") 
#>                 stopifnot(identical(p2, p))
#>             tim["sortorderupokeep", "prep"] <- timefun({
#>                 s <- clone(x)
#>                 o <- seq_along(x)
#>                 ramsortorder(s, o, na.last = FALSE)
#>                 nunique <- sortnut(s)[1L]
#>             })[3L]
#>             tim["sortorderupokeep", "use"] <- timefun({
#>                 p2 <- sortorderupo(s, o, nunique, keep.order = TRUE)
#>             })[3L]
#>             if (uniorder == "original") 
#>                 stopifnot(identical(p2, p))
#>             tim["orderupo", "prep"] <- timefun({
#>                 o <- seq_along(x)
#>                 ramorder(x, o, na.last = FALSE)
#>                 nunique <- ordernut(x, o)[1L]
#>             })[3L]
#>             tim["orderupo", "use"] <- timefun({
#>                 p2 <- orderupo(x, o, nunique)
#>             })[3L]
#>             if (uniorder == "values") 
#>                 stopifnot(identical(p2, p))
#>             tim["orderupokeep", "prep"] <- tim["orderupo", "prep"]
#>             tim["orderupokeep", "use"] <- timefun({
#>                 p2 <- orderupo(x, o, nunique, keep.order = TRUE)
#>                 nunique <- ordernut(x, o)[1L]
#>             })[3L]
#>             if (uniorder == "original") 
#>                 stopifnot(identical(p2, p))
#>             tim["hashdup", "prep"] <- tim["hashupo", "prep"]
#>             tim["hashdup", "use"] <- timefun({
#>                 p2 <- (1:n)[!hashdup(h)]
#>             })[3L]
#>             if (uniorder == "original") 
#>                 stopifnot(identical(p2, p))
#>             tim["sortorderdup", "prep"] <- tim["sortorderupokeep", 
#>                 "prep"]
#>             tim["sortorderdup", "use"] <- timefun({
#>                 p2 <- (1:n)[!sortorderdup(s, o)]
#>             })[3L]
#>             if (uniorder == "original") 
#>                 stopifnot(identical(p2, p))
#>             hashcache(x)
#>             tim["hash.cache", "use"] <- timefun({
#>                 p2 <- unipos(x, order = uniorder)
#>             })[3L]
#>             if (uniorder != "any") 
#>                 stopifnot(identical(p2, p))
#>             remcache(x)
#>             sortordercache(x)
#>             tim["sortorder.cache", "use"] <- timefun({
#>                 p2 <- unipos(x, order = uniorder)
#>             })[3L]
#>             if (uniorder != "any") 
#>                 stopifnot(identical(p2, p))
#>             remcache(x)
#>             ordercache(x)
#>             tim["order.cache", "use"] <- timefun({
#>                 p2 <- unipos(x, order = uniorder)
#>             })[3L]
#>             if (uniorder != "any") 
#>                 stopifnot(identical(p2, p))
#>             remcache(x)
#>             if (plot) {
#>                 barplot(t(tim), cex.names = 0.7)
#>                 title(paste0("unipos(", n, ", order=", uniorder, 
#>                   ")"))
#>             }
#>             ret[["unipos", as.character(n)]] <- tim
#>         }
#>     }
#>     if ("table" %in% what) {
#>         message("table: timings of different methods")
#>         N <- c(nsmall, nbig)
#>         for (i in seq_along(N)) {
#>             n <- N[i]
#>             x <- c(sample.int(1024L, n - 1L, replace = TRUE), 
#>                 NA)
#>             tim <- matrix(0, 13L, 3L)
#>             dimnames(tim) <- list(c("tabulate", "table", "table.64", 
#>                 "hashmaptab", "hashtab", "hashtab2", "sorttab", 
#>                 "sortordertab", "ordertab", "ordertabkeep", "hash.cache", 
#>                 "sort.cache", "order.cache"), c("prep", "both", 
#>                 "use"))
#>             tim["tabulate", "both"] <- timefun({
#>                 tabulate(x)
#>             })[3L]
#>             tim["table", "both"] <- timefun({
#>                 p <- table(x, exclude = NULL)
#>             })[3L]
#>             p <- p[-length(p)]
#>             x <- as.integer64(x)
#>             tim["table.64", "both"] <- timefun({
#>                 p2 <- table.integer64(x, order = taborder)
#>             })[3L]
#>             p2 <- p2[-1L]
#>             stopifnot(identical(p2, p))
#>             tim["hashmaptab", "both"] <- timefun({
#>                 p <- hashmaptab(x)
#>             })[3L]
#>             tim["hashtab", "prep"] <- timefun({
#>                 h <- hashmap(x)
#>             })[3L]
#>             tim["hashtab", "use"] <- timefun({
#>                 p2 <- hashtab(h)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             tim["hashtab2", "prep"] <- tim["hashtab", "prep"] + 
#>                 timefun({
#>                   h <- hashmap(x, nunique = h$nunique)
#>                 })[3L]
#>             tim["hashtab2", "use"] <- timefun({
#>                 p2 <- hashtab(h)
#>             })[3L]
#>             sortp <- function(p) {
#>                 s <- p$values
#>                 o <- seq_along(s)
#>                 ramsortorder(s, o, na.last = FALSE)
#>                 list(values = s, counts = p$counts[o])
#>             }
#>             p <- sortp(p)
#>             p2 <- sortp(p2)
#>             stopifnot(identical(p2, p))
#>             tim["sorttab", "prep"] <- timefun({
#>                 s <- clone(x)
#>                 ramsort(s, na.last = FALSE)
#>                 nunique <- sortnut(s)[1L]
#>             })[3L]
#>             tim["sorttab", "use"] <- timefun({
#>                 p2 <- list(values = sortuni(s, nunique), counts = sorttab(s, 
#>                   nunique))
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             tim["sortordertab", "prep"] <- timefun({
#>                 s <- clone(x)
#>                 o <- seq_along(x)
#>                 ramsortorder(s, o, na.last = FALSE)
#>                 nunique <- sortnut(s)[1L]
#>             })[3L]
#>             tim["sortordertab", "use"] <- timefun({
#>                 p2 <- list(values = sortorderuni(x, s, o, nunique), 
#>                   counts = sortordertab(s, o))
#>             })[3L]
#>             p2 <- sortp(p2)
#>             stopifnot(identical(p2, p))
#>             tim["ordertab", "prep"] <- timefun({
#>                 o <- seq_along(x)
#>                 ramorder(x, o, na.last = FALSE)
#>                 nunique <- ordernut(x, o)[1L]
#>             })[3L]
#>             tim["ordertab", "use"] <- timefun({
#>                 p2 <- list(values = orderuni(x, o, nunique), 
#>                   counts = ordertab(x, o, nunique))
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             tim["ordertabkeep", "prep"] <- tim["ordertab", "prep"]
#>             tim["ordertabkeep", "use"] <- timefun({
#>                 p2 <- list(values = orderuni(x, o, nunique, keep.order = TRUE), 
#>                   counts = ordertab(x, o, nunique, keep.order = TRUE))
#>             })[3L]
#>             p2 <- sortp(p2)
#>             stopifnot(identical(p2, p))
#>             hashcache(x)
#>             tim["hash.cache", "use"] <- timefun({
#>                 p <- table.integer64(x, order = taborder)
#>             })[3L]
#>             remcache(x)
#>             sortordercache(x)
#>             tim["sort.cache", "use"] <- timefun({
#>                 p2 <- table.integer64(x, order = taborder)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             remcache(x)
#>             ordercache(x)
#>             tim["order.cache", "use"] <- timefun({
#>                 p2 <- table.integer64(x, order = taborder)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             remcache(x)
#>             if (plot) {
#>                 barplot(t(tim), cex.names = 0.7)
#>                 title(paste0("table.integer64(", n, ", order=", 
#>                   taborder, ")"))
#>             }
#>             ret[["table", as.character(n)]] <- tim
#>         }
#>     }
#>     if ("rank" %in% what) {
#>         message("rank: timings of different methods")
#>         N <- c(nsmall, nbig)
#>         for (i in seq_along(N)) {
#>             n <- N[i]
#>             x <- c(sample(n, n - 1L, TRUE), NA)
#>             tim <- matrix(0, 7L, 3L)
#>             dimnames(tim) <- list(c("rank", "rank.keep", "rank.64", 
#>                 "sortorderrnk", "orderrnk", "sort.cache", "order.cache"), 
#>                 c("prep", "both", "use"))
#>             tim["rank", "both"] <- timefun({
#>                 rank(x)
#>             })[3L]
#>             tim["rank.keep", "both"] <- timefun({
#>                 p <- rank(x, na.last = "keep")
#>             })[3L]
#>             x <- as.integer64(x)
#>             tim["rank.64", "both"] <- timefun({
#>                 p2 <- rank.integer64(x)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             tim["sortorderrnk", "prep"] <- timefun({
#>                 s <- clone(x)
#>                 o <- seq_along(x)
#>                 na.count <- ramsortorder(s, o, na.last = FALSE)
#>             })[3L]
#>             tim["sortorderrnk", "use"] <- timefun({
#>                 p2 <- sortorderrnk(s, o, na.count)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             tim["orderrnk", "prep"] <- timefun({
#>                 o <- seq_along(x)
#>                 na.count <- ramorder(x, o, na.last = FALSE)
#>             })[3L]
#>             tim["orderrnk", "use"] <- timefun({
#>                 p2 <- orderrnk(x, o, na.count)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             sortordercache(x)
#>             tim["sort.cache", "use"] <- timefun({
#>                 p2 <- rank.integer64(x)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             remcache(x)
#>             ordercache(x)
#>             tim["order.cache", "use"] <- timefun({
#>                 p2 <- rank.integer64(x)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             remcache(x)
#>             if (plot) {
#>                 barplot(t(tim), cex.names = 0.7)
#>                 title(paste0("rank.integer64(", n, ")"))
#>             }
#>             ret[["rank", as.character(n)]] <- tim
#>         }
#>     }
#>     if ("quantile" %in% what) {
#>         message("quantile: timings of different methods")
#>         N <- c(nsmall, nbig)
#>         for (i in seq_along(N)) {
#>             n <- N[i]
#>             x <- c(sample(n, n - 1L, TRUE), NA)
#>             tim <- matrix(0, 6L, 3L)
#>             dimnames(tim) <- list(c("quantile", "quantile.64", 
#>                 "sortqtl", "orderqtl", "sort.cache", "order.cache"), 
#>                 c("prep", "both", "use"))
#>             tim["quantile", "both"] <- timefun({
#>                 p <- quantile(x, type = 1L, na.rm = TRUE)
#>             })[3L]
#>             p2 <- p
#>             p <- as.integer64(p2)
#>             names(p) <- names(p2)
#>             x <- as.integer64(x)
#>             tim["quantile.64", "both"] <- timefun({
#>                 p2 <- quantile(x, na.rm = TRUE)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             tim["sortqtl", "prep"] <- timefun({
#>                 s <- clone(x)
#>                 na.count <- ramsort(s, na.last = FALSE)
#>             })[3L]
#>             tim["sortqtl", "use"] <- timefun({
#>                 p2 <- sortqtl(s, na.count, seq(0, 1, 0.25))
#>             })[3L]
#>             stopifnot(identical(unname(p2), unname(p)))
#>             tim["orderqtl", "prep"] <- timefun({
#>                 o <- seq_along(x)
#>                 na.count <- ramorder(x, o, na.last = FALSE)
#>             })[3L]
#>             tim["orderqtl", "use"] <- timefun({
#>                 p2 <- orderqtl(x, o, na.count, seq(0, 1, 0.25))
#>             })[3L]
#>             stopifnot(identical(unname(p2), unname(p)))
#>             sortordercache(x)
#>             tim["sort.cache", "use"] <- timefun({
#>                 p2 <- quantile(x, na.rm = TRUE)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             remcache(x)
#>             ordercache(x)
#>             tim["order.cache", "use"] <- timefun({
#>                 p2 <- quantile(x, na.rm = TRUE)
#>             })[3L]
#>             stopifnot(identical(p2, p))
#>             remcache(x)
#>             if (plot) {
#>                 barplot(t(tim), cex.names = 0.7)
#>                 title(paste0("quantile(", n, ")"))
#>             }
#>             ret[["quantile", as.character(n)]] <- tim
#>         }
#>     }
#>     ret
#> }
#> <bytecode: 0x55e27db7bda0>
#> <environment: namespace:bit64>
```
