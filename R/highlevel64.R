# /*
# R-Code for matching and other functions based on hashing
# S3 atomic 64bit integers for R
# (c) 2012 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2011-12-11
# Last changed:  2011-12-11
# */

#' Function for measuring algorithmic performance of high-level and low-level integer64 functions
#'
#' @param nsmall size of smaller vector
#' @param nbig size of larger bigger vector
#' @param timefun a function for timing such as [bit::repeat.time()] or [system.time()]
#' @param what a vector of names of high-level functions
#' @param uniorder one of the order parameters that are allowed in [unique.integer64()] and [unipos.integer64()]
#' @param taborder one of the order parameters that are allowed in [table.integer64()]
#' @param plot set to FALSE to suppress plotting
#'
#' @details
#' `benchmark64` compares the following scenarios for the following use cases:
#'
#' | **scenario name** | **explanation**                                 |
#' |------------------:|:------------------------------------------------|
#' |            32-bit | applying Base R function to 32-bit integer data |
#' |            64-bit | applying bit64 function to 64-bit integer data (with no cache) |
#' |         hashcache | ditto when cache contains [hashmap()], see [hashcache()] |
#' |    sortordercache | ditto when cache contains sorting and ordering, see [sortordercache()] |
#' |        ordercache | ditto when cache contains ordering only, see [ordercache()] |
#' |          allcache | ditto when cache contains sorting, ordering and hashing |
#'
#' | **use case name** | **explanation**                         |
#' |------------------:|:----------------------------------------|
#' |             cache | filling the cache according to scenario |
#' |       match(s, b) | match small in big vector               |
#' |          s %in% b | small %in% big vector                   |
#' |       match(b, s) | match big in small vector               |
#' |          b %in% s | big %in% small vector                   |
#' |       match(b, b) | match big in (different) big vector     |
#' |          b %in% b | big %in% (different) big vector         |
#' |     duplicated(b) | duplicated of big vector                |
#' |         unique(b) | unique of big vector                    |
#' |          table(b) | table of big vector                     |
#' |           sort(b) | sorting of big vector                   |
#' |          order(b) | ordering of big vector                  |
#' |           rank(b) | ranking of big vector                   |
#' |       quantile(b) | quantiles of big vector                 |
#' |        summary(b) | summary of of big vector                |
#' |           SESSION | exemplary session involving multiple calls (including cache filling costs) |
#'
#' Note that the timings for the cached variants do _not_ contain the
#'   time costs of building the cache, except for the timing of the exemplary
#'   user session, where the cache costs are included in order to evaluate amortization.
#'
#' @return
#' `benchmark64` returns a matrix with elapsed seconds, different high-level tasks
#'   in rows and different scenarios to solve the task in columns. The last row
#'   named 'SESSION' contains the elapsed seconds of the exemplary sesssion.
#'
#' `optimizer64` returns a dimensioned list with one row for each high-level
#'   function timed and two columns named after the values of the `nsmall` and
#'   `nbig` sample sizes. Each list cell contains a matrix with timings,
#'   low-level-methods in rows and three measurements `c("prep", "both", "use")`
#'   in columns. If it can be measured separately, `prep` contains the timing
#'   of preparatory work such as sorting and hashing, and `use` contains the
#'   timing of using the prepared work. If the function timed does both,
#'   preparation and use, the timing is in `both`.
#'
#' @seealso [integer64()]
#' @examples
#' message("this small example using system.time does not give serious timings\n
#' this we do this only to run regression tests")
#' benchmark64(nsmall=2^7, nbig=2^13, timefun=function(expr)system.time(expr, gcFirst=FALSE))
#' optimizer64(nsmall=2^7, nbig=2^13, timefun=function(expr)system.time(expr, gcFirst=FALSE)
#' , plot=FALSE
#' )
#'\dontrun{
#' message("for real measurement of sufficiently large datasets run this on your machine")
#' benchmark64()
#' optimizer64()
#'}
#' message("let's look at the performance results on Core i7 Lenovo T410 with 8 GB RAM")
#' data(benchmark64.data)
#' print(benchmark64.data)
#'
#' matplot(log2(benchmark64.data[-1, 1]/benchmark64.data[-1, ])
#' , pch=c("3", "6", "h", "s", "o", "a")
#' , xlab="tasks [last=session]"
#' , ylab="log2(relative speed) [bigger is better]"
#' )
#' matplot(t(log2(benchmark64.data[-1, 1]/benchmark64.data[-1, ]))
#' , type="b", axes=FALSE
#' , lwd=c(rep(1, 14), 3)
#' , xlab="context"
#' , ylab="log2(relative speed) [bigger is better]"
#' )
#' axis(1
#' , labels=c("32-bit", "64-bit", "hash", "sortorder", "order", "hash+sortorder")
#' , at=1:6
#' )
#' axis(2)
#' data(optimizer64.data)
#' print(optimizer64.data)
#' oldpar <- par(no.readonly = TRUE)
#' par(mfrow=c(2, 1))
#' par(cex=0.7)
#' for (i in 1:nrow(optimizer64.data)) {
#'  for (j in 1:2) {
#'    tim <- optimizer64.data[[i, j]]
#'   barplot(t(tim))
#'   if (rownames(optimizer64.data)[i]=="match")
#'    title(paste("match", colnames(optimizer64.data)[j], "in", colnames(optimizer64.data)[3-j]))
#'   else if (rownames(optimizer64.data)[i]=="%in%")
#'    title(paste(colnames(optimizer64.data)[j], "%in%", colnames(optimizer64.data)[3-j]))
#'   else
#'    title(paste(rownames(optimizer64.data)[i], colnames(optimizer64.data)[j]))
#'  }
#' }
#' par(mfrow=c(1, 1))
#' @keywords misc
#' @name benchmark64
NULL

#' @describeIn benchmark64 compares high-level integer64 functions against the
#'   integer functions from Base R
#' @export
# nocov start
# nolint start: brace_linter, line_length_linter.
benchmark64 <- function(nsmall=2L^16L, nbig=2L^25L, timefun=repeat.time) {

 message('\ncompare performance for a complete sessions of calls')
 s <- sample(nbig, nsmall, TRUE)
 b <- sample(nbig, nbig, TRUE)
 b2 <- sample(nbig, nbig, TRUE)

 tim1 <- double(6L)
 names(tim1) <- c("32-bit", "64-bit", "hashcache", "sortordercache", "ordercache", "allcache")

 s <- as.integer(s)
 b <- as.integer(b)
 b2 <- as.integer(b2)

 for (i in 1:6) {
  message("\n=== ", names(tim1)[i], " ===")

  if (i==2L) {
   s <- as.integer64(s)
   b <- as.integer64(b)
   b2 <- as.integer64(b2)
  }

  tim1[i] <- 0L

  tim1[i] <- tim1[i] + timefun({
   switch(i,
     NULL, # i=1
     NULL, # i=2
     { hashcache(s); hashcache(b); hashcache(b2) },
     { sortordercache(s); sortordercache(b); sortordercache(b2) },
     { ordercache(s); ordercache(b); ordercache(b2) },
     { hashcache(s); hashcache(b); hashcache(b2);sortordercache(s); sortordercache(b); sortordercache(b2) }
   )
  })[3L]

  message('check data range, mean etc.')
  tim1[i] <- tim1[i] + timefun({
   summary(b)
  })[3L]
  message('get all percentiles for plotting distribution shape')
  tim1[i] <- tim1[i] + timefun({
   quantile(b, probs=seq(0.0, 1.0, 0.01))
  })[3L]
  message('list the upper and lower permille of values')
  tim1[i] <- tim1[i] + timefun({
   quantile(b, probs=c(0.001, 0.999))
   sort(b, na.last=NA)
  })[3L]
  message('OK, for some of these values I want to see the complete ROW, so I need their positions in the data.frame')
  tim1[i] <- tim1[i] + timefun({
   if (i==1L) order(b) else order.integer64(b)
  })[3L]
  message('check if any values are duplicated')
  tim1[i] <- tim1[i] + timefun({
   anyDuplicated(b)
  })[3L]
  message('since not unique, then check distribution of frequencies')
  tim1[i] <- tim1[i] + timefun({
   if (i==1L) tabulate(table(b, exclude=NULL)) else tabulate(table.integer64(b, return='list')$counts)
  })[3L]
  message("OK, let's plot the percentiles of unique values versus the percentiles allowing for duplicates")
  tim1[i] <- tim1[i] + timefun({
   quantile(b, probs=seq(0.0, 1.0, 0.01))
   quantile(unique(b), probs=seq(0.0, 1.0, 0.01))
  })[3L]
  message('check whether we find a match for each fact in the dimension table')
  tim1[i] <- tim1[i] + timefun({
   all(if (i==1L) b %in% s else "%in%.integer64"(b, s))
  })[3L]
  message('check whether there are any dimension table entries not in the fact table')
  tim1[i] <- tim1[i] + timefun({
   all(if (i==1L) s %in% b else "%in%.integer64"(s, b))
  })[3L]
  message('check whether we find a match for each fact in a parallel fact table')
  tim1[i] <- tim1[i] + timefun({
   all(if (i==1L) b %in% b2 else "%in%.integer64"(b, b2))
  })[3L]
  message('find positions of facts in dimension table for joining')
  tim1[i] <- tim1[i] + timefun({
   if (i==1L) match(b, s) else match.integer64(b, s)
  })[3L]
  message('find positions of facts in parallel fact table for joining')
  tim1[i] <- tim1[i] + timefun({
   if (i==1L) match(b, b2) else match.integer64(b, b2)
  })[3L]
  message('out of curiosity: how well rank-correlated are fact and parallel fact table?')
  tim1[i] <- tim1[i] + timefun({
   if (i==1L) {
    cor(rank(b, na.last="keep"), rank(b2, na.last="keep"), use="na.or.complete")
   } else {
    cor(rank.integer64(b), rank.integer64(b2), use="na.or.complete")
   }
  })[3L]

  remcache(s)
  remcache(b)
  remcache(b2)

  print(round(rbind(seconds=tim1, factor=tim1[1L]/tim1), 3L))

 }

        # 32-bit         64-bit      hashcache sortordercache     ordercache       allcache
       # 196.510          8.963          8.242          5.183         12.325          6.043
        # 32-bit         64-bit      hashcache sortordercache     ordercache       allcache
         # 1.000         21.924         23.842         37.913         15.944         32.519


 message("\nnow let's look more systematically at the components involved")
 s <- sample(nbig, nsmall, TRUE)
 b <- sample(nbig, nbig, TRUE)
 b2 <- sample(nbig, nbig, TRUE)

  tim2 <- matrix(0.0, 15L, 6L)
  dimnames(tim2) <- list(
    c("cache", "match(s, b)", "s %in% b", "match(b, s)", "b %in% s", "match(b, b)", "b %in% b", "duplicated(b)", "unique(b)", "table(b)", "sort(b)", "order(b)", "rank(b)", "quantile(b)", "summary(b)"), # nolint: line_length_linter.
    c("32-bit", "64-bit", "hashcache", "sortordercache", "ordercache", "allcache")
  )

 s <- as.integer(s)
 b <- as.integer(b)
 b2 <- as.integer(b2)

 i <- 1L
 for (i in 1:6) {
  if (i==2L) {
   s <- as.integer64(s)
   b <- as.integer64(b)
   b2 <- as.integer64(b2)
  }

  if (i>2L) message(colnames(tim2)[i], " cache")
  tim2["cache", i] <- timefun({
   switch(i,
     NULL, # i=1
     NULL, # i=2
     { hashcache(s); hashcache(b); hashcache(b2) },
     { sortordercache(s); sortordercache(b); sortordercache(b2) },
     { ordercache(s); ordercache(b); ordercache(b2) },
     { hashcache(s); hashcache(b); hashcache(b2);sortordercache(s); sortordercache(b); sortordercache(b2) }
   )
  })[3L]

  message(colnames(tim2)[i], " match(s, b)")
  tim2["match(s, b)", i] <- timefun({
   if (i==1L) match(s, b) else match.integer64(s, b)
  })[3L]

  message(colnames(tim2)[i], " s %in% b")
  tim2["s %in% b", i] <- timefun({
   if (i==1L) s %in% b else "%in%.integer64"(s, b)
  })[3L]

  message(colnames(tim2)[i], " match(b, s)")
  tim2["match(b, s)", i] <- timefun({
   if (i==1L) match(b, s) else match.integer64(b, s)
  })[3L]

  message(colnames(tim2)[i], " b %in% s")
  tim2["b %in% s", i] <- timefun({
   if (i==1L) b %in% s else "%in%.integer64"(b, s)
  })[3L]

  message(colnames(tim2)[i], " match(b, b)")
  tim2["match(b, b)", i] <- timefun({
   if (i==1L) match(b, b2) else match.integer64(b, b2)
  })[3L]

  message(colnames(tim2)[i], " b %in% b")
  tim2["b %in% b", i] <- timefun({
   if (i==1L) b %in% b2 else "%in%.integer64"(b, b2)
  })[3L]

  message(colnames(tim2)[i], " duplicated(b)")
  tim2["duplicated(b)", i] <- timefun({
   duplicated(b)
  })[3L]

  message(colnames(tim2)[i], " unique(b)")
  tim2["unique(b)", i] <- timefun({
   unique(b)
  })[3L]

  message(colnames(tim2)[i], " table(b)")
  tim2["table(b)", i] <- timefun({
   if (i==1L) table(b) else table.integer64(b, return='list')
  })[3L]

  message(colnames(tim2)[i], " sort(b)")
  tim2["sort(b)", i] <- timefun({
   sort(b)
  })[3L]

  message(colnames(tim2)[i], " order(b)")
  tim2["order(b)", i] <- timefun({
   if (i==1L) order(b) else order.integer64(b)
  })[3L]

  message(colnames(tim2)[i], " rank(b)")
  tim2["rank(b)", i] <- timefun({
   if (i==1L) rank(b) else rank.integer64(b)
  })[3L]

  message(colnames(tim2)[i], " quantile(b)")
  tim2["quantile(b)", i] <- timefun({
   quantile(b)
  })[3L]

  message(colnames(tim2)[i], " summary(b)")
  tim2["summary(b)", i] <- timefun({
   summary(b)
  })[3L]

  remcache(s)
  remcache(b)
  remcache(b2)

  tim3 <- rbind(tim2, SESSION=tim1)
  #tim2 <- tim2[, 1]/tim2

  cat("seconds")
  print(round(tim3, 3L))
  cat("factor")
  print(round(tim3[, 1L]/tim3, 3L))

 }



               # 32-bit 64-bit hashcache sortordercache ordercache allcache
# cache           0.000  0.000     0.775          1.330      6.500    2.660
# match(s, b)     0.820  0.218     0.004          0.025      0.093    0.004
# s %in% b        0.810  0.234     0.003          0.022      0.093    0.003
# match(b, s)     0.450  0.228     0.232          0.224      0.224    0.226
# b %in% s        0.510  0.226     0.224          0.222      0.218    0.222
# match(b, b)     2.370  0.870     0.505          0.890      0.880    0.505
# b %in% b        2.350  0.850     0.480          0.865      0.870    0.483
# duplicated(b)   0.875  0.510     0.141          0.116      0.383    0.117
# unique(b)       0.930  0.555     0.447          0.156      0.427    0.450
# table(b)      110.340  0.725     0.680          0.234      0.575    0.202
# sort(b)         2.440  0.400     0.433          0.072      0.460    0.069
# order(b)       12.780  0.680     0.615          0.036      0.036    0.035
# rank(b)        13.480  0.860     0.915          0.240      0.545    0.246
# quantile(b)     0.373  0.400     0.410          0.000      0.000    0.000
# summary(b)      0.645  0.423     0.427          0.016      0.016    0.016
# TOTAL         149.173  7.179     6.291          4.448     11.320    5.239
              # 32-bit  64-bit hashcache sortordercache ordercache allcache
# cache              1   1.062     0.000          0.000      0.000    0.000
# match(s, b)        1   3.761   230.420         32.475      8.843  217.300
# s %in% b           1   3.462   234.090         36.450      8.735  237.386
# match(b, s)        1   1.974     1.940          2.009      2.009    1.991
# b %in% s           1   2.257     2.277          2.297      2.339    2.297
# match(b, b)        1   2.724     4.693          2.663      2.693    4.693
# b %in% b           1   2.765     4.896          2.717      2.701    4.862
# duplicated(b)      1   1.716     6.195          7.572      2.283    7.500
# unique(b)          1   1.676     2.082          5.972      2.180    2.067
# table(b)           1 152.193   162.265        471.538    191.896  546.238
# sort(b)            1   6.100     5.631         33.822      5.304   35.534
# order(b)           1  18.794    20.780        357.840    354.297  366.950
# rank(b)            1  15.674    14.732         56.167     24.734   54.797
# quantile(b)        1   0.933     0.911        804.907    806.027  810.133
# summary(b)         1   1.524     1.512         39.345     39.345   39.345
# TOTAL              1  20.778    23.712         33.534     13.177   28.476

  tim3
}

#' @describeIn benchmark64 compares for each high-level integer64 function the Base
#'   R integer function with several low-level integer64 functions with and
#'   without caching
#' @export
optimizer64 <- function(nsmall=2L^16L,
                        nbig=2L^25L,
                        timefun=repeat.time,
                        what=c("match", "%in%", "duplicated", "unique", "unipos", "table", "rank", "quantile"),
                        uniorder=c("original", "values", "any"),
                        taborder=c("values", "counts"),
                        plot=TRUE) {
 uniorder <- match.arg(uniorder)
 taborder <- match.arg(taborder)
 ret <- vector("list", 2L*length(what))
 dim(ret) <- c(length(what), 2L)
 dimnames(ret) <- list(what, c(nsmall, nbig))

 if (plot) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow=c(2L, 1L))
 }

 if ("match" %in% what) {
  message("match: timings of different methods")
  N1 <- c(nsmall, nbig)
  N2 <- c(nbig, nsmall)
  for (i in seq_along(N1)) {
   n1 <- N1[i]
   n2 <- N2[i]
   x1 <- c(sample(n2, n1-1L, TRUE), NA)
   x2 <- c(sample(n2, n2-1L, TRUE), NA)
   tim <- matrix(0.0, 9L, 3L)
   dimnames(tim) <- list(
    c("match", "match.64", "hashpos", "hashrev", "sortorderpos", "orderpos", "hashcache", "sortorder.cache", "order.cache"), # nolint: line_length_linter.
    c("prep", "both", "use")
  )

   tim["match", "both"] <- timefun({
    p <- match(x1, x2)
   })[3L]
   x1 <- as.integer64(x1)
   x2 <- as.integer64(x2)

   tim["match.64", "both"] <- timefun({
    p2 <- match.integer64(x1, x2)
   })[3L]
   stopifnot(identical(p2, p))

   tim["hashpos", "prep"] <- timefun({
    h2 <- hashmap(x2)
   })[3L]
   tim["hashpos", "use"] <- timefun({
    p2 <- hashpos(h2, x1)
   })[3L]
   stopifnot(identical(p2, p))

   tim["hashrev", "prep"] <- timefun({
    h1 <- hashmap(x1)
   })[3L]
   tim["hashrev", "use"] <- timefun({
    p1 <- hashrev(h1, x2)
   })[3L]
   stopifnot(identical(p1, p))

   tim["sortorderpos", "prep"] <- system.time({
    s2 <- clone(x2)
    o2 <- seq_along(x2)
    ramsortorder(s2, o2, na.last=FALSE)
   })[3L]
   tim["sortorderpos", "use"] <- timefun({
    p2 <- sortorderpos(s2, o2, x1)
   })[3L]
   stopifnot(identical(p2, p))

   tim["orderpos", "prep"] <- timefun({
    o2 <- seq_along(x2)
    ramorder(x2, o2, na.last=FALSE)
   })[3L]
   tim["orderpos", "use"] <- timefun({
    p2 <- orderpos(x2, o2, x1, method=2L)
   })[3L]
   stopifnot(identical(p2, p))

   hashcache(x2)
   tim["hashcache", "use"] <- timefun({
    p2 <- match.integer64(x1, x2)
   })[3L]
   stopifnot(identical(p2, p))
   remcache(x2)

   sortordercache(x2)
   tim["sortorder.cache", "use"] <- timefun({
    p2 <- match.integer64(x1, x2)
   })[3L]
   stopifnot(identical(p2, p))
   remcache(x2)

   ordercache(x2)
   tim["order.cache", "use"] <- timefun({
    p2 <- match.integer64(x1, x2)
   })[3L]
   stopifnot(identical(p2, p))
   remcache(x2)

   if (plot) {
    barplot(t(tim))
    n <- format(c(n1, n2))
    title(paste("match", n[1L], "in", n[2L]))
   }

   ret[["match", as.character(n1)]] <- tim
  }
 }

 if ("%in%" %in% what) {
  message("%in%: timings of different methods")
  N1 <- c(nsmall, nbig)
  N2 <- c(nbig, nsmall)
  for (i in seq_along(N1)) {
   n1 <- N1[i]
   n2 <- N2[i]
   x1 <- c(sample(n2, n1-1L, TRUE), NA)
   x2 <- c(sample(n2, n2-1L, TRUE), NA)
   tim <- matrix(0.0, 10L, 3L)
   dimnames(tim) <- list(
    c("%in%", "match.64", "%in%.64", "hashfin", "hashrin", "sortfin", "orderfin", "hash.cache", "sortorder.cache", "order.cache"), # nolint: line_length_linter.
    c("prep", "both", "use")
  )

   tim["%in%", "both"] <- timefun({
    p <- x1 %in% x2
   })[3L]
   x1 <- as.integer64(x1)
   x2 <- as.integer64(x2)

   tim["match.64", "both"] <- timefun({
    p2 <- match.integer64(x1, x2, nomatch = 0L) > 0L
   })[3L]
   stopifnot(identical(p2, p))

   tim["%in%.64", "both"] <- timefun({
    p2 <- "%in%.integer64"(x1, x2) # this is using the custom version
   })[3L]
   stopifnot(identical(p2, p))

   tim["hashfin", "prep"] <- timefun({
    h2 <- hashmap(x2)
   })[3L]
   tim["hashfin", "use"] <- timefun({
    p2 <- hashfin(h2, x1)
   })[3L]
   stopifnot(identical(p2, p))

   tim["hashrin", "prep"] <- timefun({
    h1 <- hashmap(x1)
   })[3L]
   tim["hashrin", "use"] <- timefun({
    p1 <- hashrin(h1, x2)
   })[3L]
   stopifnot(identical(p2, p))

   tim["sortfin", "prep"] <- timefun({
    s2 <- clone(x2)
    ramsort(s2, na.last=FALSE)
   })[3L]
   tim["sortfin", "use"] <- timefun({
    p2 <- sortfin(s2, x1)
   })[3L]
   stopifnot(identical(p2, p))

   tim["orderfin", "prep"] <- timefun({
    o2 <- seq_along(x2)
    ramorder(x2, o2, na.last=FALSE)
   })[3L]
   tim["orderfin", "use"] <- timefun({
    p2 <- orderfin(x2, o2, x1)
   })[3L]
   stopifnot(identical(p2, p))

   hashcache(x2)
   tim["hash.cache", "use"] <- timefun({
    p2 <- "%in%.integer64"(x1, x2)
   })[3L]
   stopifnot(identical(p2, p))
   remcache(x2)

   sortordercache(x2)
   tim["sortorder.cache", "use"] <- timefun({
    p2 <- "%in%.integer64"(x1, x2)
   })[3L]
   stopifnot(identical(p2, p))
   remcache(x2)

   ordercache(x2)
   tim["order.cache", "use"] <- timefun({
    p2 <- "%in%.integer64"(x1, x2)
   })[3L]
   stopifnot(identical(p2, p))
   remcache(x2)

   if (plot) {
    barplot(t(tim))
    n <- format(c(n1, n2))
    title(paste(n[1L], "%in%", n[2L]))
   }

   ret[["%in%", as.character(n1)]] <- tim
  }
 }
 if ("duplicated" %in% what) {
  message("duplicated: timings of different methods")
  N <- c(nsmall, nbig)
  for (i in seq_along(N)) {
   n <- N[i]
   x <- c(sample(n, n-1L, TRUE), NA)
   tim <- matrix(0.0, 10L, 3L)
   dimnames(tim) <- list(
    c("duplicated", "duplicated.64", "hashdup", "sortorderdup1", "sortorderdup2", "orderdup1", "orderdup2", "hash.cache", "sortorder.cache", "order.cache"), # nolint: line_length_linter.
    c("prep", "both", "use")
  )

   tim["duplicated", "both"] <- timefun({
    p <- duplicated(x)
   })[3L]
   x <- as.integer64(x)

   tim["duplicated.64", "both"] <- timefun({
    p2 <- duplicated(x)
   })[3L]
   stopifnot(identical(p2, p))

   tim["hashdup", "prep"] <- timefun({
    h <- hashmap(x)
   })[3L]
   tim["hashdup", "use"] <- timefun({
    p2 <- hashdup(h)
   })[3L]
   stopifnot(identical(p2, p))

   tim["sortorderdup1", "prep"] <- timefun({
    s <- clone(x)
    o <- seq_along(x)
    ramsortorder(s, o, na.last=FALSE)
    nunique <- sortnut(s)[1L]
   })[3L]
   tim["sortorderdup1", "use"] <- timefun({
    p2 <- sortorderdup(s, o, method=1L)
   })[3L]
   stopifnot(identical(p2, p))

   tim["sortorderdup2", "prep"] <- tim["sortorderdup1", "prep"]
   tim["sortorderdup2", "use"] <- timefun({
    p2 <- sortorderdup(s, o, method=2L)
   })[3L]
   stopifnot(identical(p2, p))

   tim["orderdup1", "prep"] <- timefun({
    o <- seq_along(x)
    ramorder(x, o, na.last=FALSE)
    nunique <- ordernut(x, o)[1L]
   })[3L]
   tim["orderdup1", "use"] <- timefun({
    p2 <- orderdup(x, o, method=1L)
   })[3L]
   stopifnot(identical(p2, p))

   tim["orderdup2", "prep"] <- tim["orderdup1", "prep"]
   tim["orderdup2", "use"] <- timefun({
    p2 <- orderdup(x, o, method=2L)
   })[3L]
   stopifnot(identical(p2, p))

   hashcache(x)
   tim["hash.cache", "use"] <- timefun({
    p2 <- duplicated(x)
   })[3L]
   stopifnot(identical(p2, p))
   remcache(x)

   sortordercache(x)
   tim["sortorder.cache", "use"] <- timefun({
    p2 <- duplicated(x)
   })[3L]
   stopifnot(identical(p2, p))
   remcache(x)

   ordercache(x)
   tim["order.cache", "use"] <- timefun({
    p2 <- duplicated(x)
   })[3L]
   stopifnot(identical(p2, p))
   remcache(x)

   if (plot) {
    barplot(t(tim), cex.names=0.7)
    title(paste0("duplicated(", n, ")"))
   }

   ret[["duplicated", as.character(n)]] <- tim
  }
 }
 if ("unique" %in% what) {
  message("unique: timings of different methods")
  N <- c(nsmall, nbig)
  for (i in seq_along(N)) {
    n <- N[i]
    x <- c(sample(n, n-1L, TRUE), NA)
    tim <- matrix(0.0, 15L, 3L)
    dimnames(tim) <- list(
      c("unique", "unique.64", "hashmapuni", "hashuni", "hashunikeep", "sortuni", "sortunikeep", "orderuni", "orderunikeep", "hashdup", "sortorderdup", "hash.cache", "sort.cache", "sortorder.cache", "order.cache"), # nolint: line_length_linter.
      c("prep", "both", "use")
    )

   tim["unique", "both"] <- timefun({
    p <- unique(x)
   })[3L]
   x <- as.integer64(x)
   p <- as.integer64(p)
   if (uniorder=="values")
    ramsort(p, na.last=FALSE)

   tim["unique.64", "both"] <- timefun({
    p2 <- unique(x, order=uniorder)
   })[3L]
   if (uniorder!="any")
    stopifnot(identical.integer64(p2, p))

   tim["hashmapuni", "both"] <- timefun({
    p2 <- hashmapuni(x)
   })[3L]
   if (uniorder=="original")
    stopifnot(identical.integer64(p2, p))

   tim["hashuni", "prep"] <- timefun({
    h <- hashmap(x)
    # for(r in 1:r)h <- hashmap(x, nunique=h$nunique)
   })[3L]
   tim["hashuni", "use"] <- timefun({
    p2 <- hashuni(h)
   })[3L]
   if (uniorder=="values")
    stopifnot(identical.integer64(sort(p2, na.last=FALSE), p))

   tim["hashunikeep", "prep"] <- tim["hashuni", "prep"]
   tim["hashunikeep", "use"] <- timefun({
    p2 <- hashuni(h, keep.order=TRUE)
   })[3L]
   if (uniorder=="original")
    stopifnot(identical.integer64(p2, p))

   tim["sortuni", "prep"] <- timefun({
    s <- clone(x)
    ramsort(s, na.last=FALSE)
    nunique <- sortnut(s)[1L]
   })[3L]
   tim["sortuni", "use"] <- timefun({
    p2 <- sortuni(s, nunique)
   })[3L]
   if (uniorder=="values")
    stopifnot(identical.integer64(sort(p2, na.last=FALSE), p))

   tim["sortunikeep", "prep"] <- timefun({
    s <- clone(x)
    o <- seq_along(x)
    ramsortorder(s, o, na.last=FALSE)
    nunique <- sortnut(s)[1L]
   })[3L]
   tim["sortunikeep", "use"] <- timefun({
    p2 <- sortorderuni(x, s, o, nunique)
   })[3L]
   if (uniorder=="original")
    stopifnot(identical.integer64(p2, p))

   tim["orderuni", "prep"] <- timefun({
    o <- seq_along(x)
    ramorder(x, o, na.last=FALSE)
    nunique <- ordernut(x, o)[1L]
   })[3L]
   tim["orderuni", "use"] <- timefun({
    p2 <- orderuni(x, o, nunique)
   })[3L]
   if (uniorder=="values")
    stopifnot(identical.integer64(sort(p2, na.last=FALSE), p))

   tim["orderunikeep", "prep"] <- tim["orderuni", "prep"]
   tim["orderunikeep", "use"] <- timefun({
    p2 <- orderuni(x, o, nunique, keep.order=TRUE)
    nunique <- ordernut(x, o)[1L]
   })[3L]
   if (uniorder=="original")
    stopifnot(identical.integer64(p2, p))

   tim["hashdup", "prep"] <- tim["hashuni", "prep"]
   tim["hashdup", "use"] <- timefun({
    p2 <- x[!hashdup(h)]
   })[3L]
   if (uniorder=="original")
    stopifnot(identical.integer64(p2, p))

   tim["sortorderdup", "prep"] <- tim["sortunikeep", "prep"]
   tim["sortorderdup", "use"] <- timefun({
    p2 <- x[!sortorderdup(s, o)]
   })[3L]
   if (uniorder=="original")
    stopifnot(identical.integer64(p2, p))


   hashcache(x)
   tim["hash.cache", "use"] <- timefun({
    p2 <- unique(x, order=uniorder)
   })[3L]
   if (uniorder!="any")
    stopifnot(identical.integer64(p2, p))
   remcache(x)

   sortcache(x)
   tim["sort.cache", "use"] <- timefun({
    p2 <- unique(x, order=uniorder)
   })[3L]
   if (uniorder!="any")
    stopifnot(identical.integer64(p2, p))
   remcache(x)

   sortordercache(x)
   tim["sortorder.cache", "use"] <- timefun({
    p2 <- unique(x, order=uniorder)
   })[3L]
   if (uniorder!="any")
    stopifnot(identical.integer64(p2, p))
   remcache(x)

   ordercache(x)
   tim["order.cache", "use"] <- timefun({
    p2 <- unique(x, order=uniorder)
   })[3L]
   if (uniorder!="any")
    stopifnot(identical.integer64(p2, p))
   remcache(x)

   if (plot) {
    barplot(t(tim), cex.names=0.7)
    title(paste0("unique(", n, ", order=", uniorder, ")"))
   }

   ret[["unique", as.character(n)]] <- tim
  }
 }
 if ("unipos" %in% what) {
  message("unipos: timings of different methods")
  N <- c(nsmall, nbig)
  for (i in seq_along(N)) {
    n <- N[i]
    x <- c(sample(n, n-1L, TRUE), NA)
    tim <- matrix(0.0, 14L, 3L)
    dimnames(tim) <- list(
      c("unique", "unipos.64", "hashmapupo", "hashupo", "hashupokeep", "sortorderupo", "sortorderupokeep", "orderupo", "orderupokeep", "hashdup", "sortorderdup", "hash.cache", "sortorder.cache", "order.cache"), # nolint: line_length_linter.
      c("prep", "both", "use")
    )

   tim["unique", "both"] <- timefun({
    unique(x)
   })[3L]
   x <- as.integer64(x)

   tim["unipos.64", "both"] <- timefun({
    p <- unipos(x, order=uniorder)
   })[3L]

   tim["hashmapupo", "both"] <- timefun({
    p2 <- hashmapupo(x)
   })[3L]
   if (uniorder=="original")
    stopifnot(identical(p2, p))

   tim["hashupo", "prep"] <- timefun({
    h <- hashmap(x)
    # if nunique is small we could re-build the hashmap at a smaller size
    # h <- hashmap(x, nunique=h$nunique)
   })[3L]
   tim["hashupo", "use"] <- timefun({
    p2 <- hashupo(h)
   })[3L]
   if (uniorder=="values")
    stopifnot(identical(sort(p2, na.last=FALSE), sort(p, na.last=FALSE)))

   tim["hashupokeep", "prep"] <- tim["hashupo", "prep"]
   tim["hashupokeep", "use"] <- timefun({
    p2 <- hashupo(h, keep.order=TRUE)
   })[3L]
   if (uniorder=="original")
    stopifnot(identical(p2, p))


   tim["sortorderupo", "prep"] <- timefun({
    s <- clone(x)
    o <- seq_along(x)
    ramsortorder(s, o, na.last=FALSE)
    nunique <- sortnut(s)[1L]
   })[3L]
   tim["sortorderupo", "use"] <- timefun({
    p2 <- sortorderupo(s, o, nunique)
   })[3L]
   if (uniorder=="values")
    stopifnot(identical(p2, p))

   tim["sortorderupokeep", "prep"] <- timefun({
    s <- clone(x)
    o <- seq_along(x)
    ramsortorder(s, o, na.last=FALSE)
    nunique <- sortnut(s)[1L]
   })[3L]
   tim["sortorderupokeep", "use"] <- timefun({
    p2 <- sortorderupo(s, o, nunique, keep.order=TRUE)
   })[3L]
   if (uniorder=="original")
    stopifnot(identical(p2, p))

   tim["orderupo", "prep"] <- timefun({
    o <- seq_along(x)
    ramorder(x, o, na.last=FALSE)
    nunique <- ordernut(x, o)[1L]
   })[3L]
   tim["orderupo", "use"] <- timefun({
    p2 <- orderupo(x, o, nunique)
   })[3L]
   if (uniorder=="values")
    stopifnot(identical(p2, p))

   tim["orderupokeep", "prep"] <- tim["orderupo", "prep"]
   tim["orderupokeep", "use"] <- timefun({
    p2 <- orderupo(x, o, nunique, keep.order=TRUE)
    nunique <- ordernut(x, o)[1L]
   })[3L]
   if (uniorder=="original")
    stopifnot(identical(p2, p))

   tim["hashdup", "prep"] <- tim["hashupo", "prep"]
   tim["hashdup", "use"] <- timefun({
    p2 <- (1:n)[!hashdup(h)]
   })[3L]
   if (uniorder=="original")
    stopifnot(identical(p2, p))

   tim["sortorderdup", "prep"] <- tim["sortorderupokeep", "prep"]
   tim["sortorderdup", "use"] <- timefun({
    p2 <- (1:n)[!sortorderdup(s, o)]
   })[3L]
   if (uniorder=="original")
    stopifnot(identical(p2, p))

   hashcache(x)
   tim["hash.cache", "use"] <- timefun({
    p2 <- unipos(x, order=uniorder)
   })[3L]
   if (uniorder!="any")
    stopifnot(identical(p2, p))
   remcache(x)

   sortordercache(x)
   tim["sortorder.cache", "use"] <- timefun({
    p2 <- unipos(x, order=uniorder)
   })[3L]
   if (uniorder!="any")
    stopifnot(identical(p2, p))
   remcache(x)

   ordercache(x)
   tim["order.cache", "use"] <- timefun({
    p2 <- unipos(x, order=uniorder)
   })[3L]
   if (uniorder!="any")
    stopifnot(identical(p2, p))
   remcache(x)

   if (plot) {
    barplot(t(tim), cex.names=0.7)
    title(paste0("unipos(", n, ", order=", uniorder, ")"))
   }

   ret[["unipos", as.character(n)]] <- tim
  }
 }
 if ("table" %in% what) {
  message("table: timings of different methods")
  N <- c(nsmall, nbig)
  for (i in seq_along(N)) {
   n <- N[i]
   x <- c(sample.int(1024L, n-1L, replace=TRUE), NA)
   tim <- matrix(0.0, 13L, 3L)
   dimnames(tim) <- list(c("tabulate", "table", "table.64", "hashmaptab", "hashtab", "hashtab2", "sorttab", "sortordertab", "ordertab", "ordertabkeep"
    , "hash.cache", "sort.cache", "order.cache")
   , c("prep", "both", "use"))

   tim["tabulate", "both"] <- timefun({
    tabulate(x)
   })[3L]

   tim["table", "both"] <- timefun({
    p <- table(x, exclude=NULL)
   })[3L]
   p <- p[-length(p)]

   x <- as.integer64(x)

   tim["table.64", "both"] <- timefun({
    p2 <- table.integer64(x, order=taborder)
   })[3L]
   p2 <- p2[-1L]
   stopifnot(identical(p2, p))

   tim["hashmaptab", "both"] <- timefun({
    p <- hashmaptab(x)
   })[3L]

   tim["hashtab", "prep"] <- timefun({
    h <- hashmap(x)
   })[3L]
   tim["hashtab", "use"] <- timefun({
    p2 <- hashtab(h)
   })[3L]
   stopifnot(identical(p2, p))

   tim["hashtab2", "prep"] <- tim["hashtab", "prep"] + timefun({
    h <- hashmap(x, nunique=h$nunique)
   })[3L]
   tim["hashtab2", "use"] <- timefun({
    p2 <- hashtab(h)
   })[3L]

   sortp <- function(p) {
    s <- p$values
    o <- seq_along(s)
    ramsortorder(s, o, na.last=FALSE)
    list(values=s, counts=p$counts[o])
   }
   p <- sortp(p)
   p2 <- sortp(p2)
   stopifnot(identical(p2, p))

   tim["sorttab", "prep"] <- timefun({
    s <- clone(x)
    ramsort(s, na.last=FALSE)
    nunique <- sortnut(s)[1L]
   })[3L]
   tim["sorttab", "use"] <- timefun({
    p2 <- list(values=sortuni(s, nunique), counts=sorttab(s, nunique))
   })[3L]
   stopifnot(identical(p2, p))

   tim["sortordertab", "prep"] <- timefun({
    s <- clone(x)
    o <- seq_along(x)
    ramsortorder(s, o, na.last=FALSE)
    nunique <- sortnut(s)[1L]
      })[3L]
            tim["sortordertab", "use"] <- timefun({
                p2 <- list(values=sortorderuni(x, s, o, nunique), counts=sortordertab(s, o))
            })[3L]
            p2 <- sortp(p2)
            stopifnot(identical(p2, p))

            tim["ordertab", "prep"] <- timefun({
                o <- seq_along(x)
                ramorder(x, o, na.last=FALSE)
                nunique <- ordernut(x, o)[1L]
            })[3L]
            tim["ordertab", "use"] <- timefun({
                p2 <- list(values=orderuni(x, o, nunique), counts=ordertab(x, o, nunique))
            })[3L]
            stopifnot(identical(p2, p))

            tim["ordertabkeep", "prep"] <- tim["ordertab", "prep"]
            tim["ordertabkeep", "use"] <- timefun({
                p2 <- list(values=orderuni(x, o, nunique, keep.order=TRUE), counts=ordertab(x, o, nunique, keep.order=TRUE))
            })[3L]
            p2 <- sortp(p2)
            stopifnot(identical(p2, p))

            hashcache(x)
            tim["hash.cache", "use"] <- timefun({
                p <- table.integer64(x, order=taborder)
            })[3L]
            remcache(x)

            sortordercache(x)
            tim["sort.cache", "use"] <- timefun({
                p2 <- table.integer64(x, order=taborder)
            })[3L]
            stopifnot(identical(p2, p))
            remcache(x)

            ordercache(x)
            tim["order.cache", "use"] <- timefun({
                p2 <- table.integer64(x, order=taborder)
            })[3L]
            stopifnot(identical(p2, p))
            remcache(x)

            if (plot) {
                barplot(t(tim), cex.names=0.7)
                title(paste0("table.integer64(", n, ", order=", taborder, ")"))
            }

            ret[["table", as.character(n)]] <- tim
        }
    }
    if ("rank" %in% what) {
        message("rank: timings of different methods")
        N <- c(nsmall, nbig)
        for (i in seq_along(N)) {
            n <- N[i]
            x <- c(sample(n, n-1L, TRUE), NA)
            tim <- matrix(0.0, 7L, 3L)
            dimnames(tim) <- list(c("rank", "rank.keep", "rank.64", "sortorderrnk", "orderrnk"
                , "sort.cache", "order.cache")
            , c("prep", "both", "use"))

            tim["rank", "both"] <- timefun({
                rank(x)
            })[3L]

            tim["rank.keep", "both"] <- timefun({
                p <- rank(x, na.last="keep")
            })[3L]

            x <- as.integer64(x)

            tim["rank.64", "both"] <- timefun({
                p2 <- rank.integer64(x)
            })[3L]
            stopifnot(identical(p2, p))

            tim["sortorderrnk", "prep"] <- timefun({
                s <- clone(x)
                o <- seq_along(x)
                na.count <- ramsortorder(s, o, na.last=FALSE)
            })[3L]
            tim["sortorderrnk", "use"] <- timefun({
                p2 <- sortorderrnk(s, o, na.count)
            })[3L]
            stopifnot(identical(p2, p))

            tim["orderrnk", "prep"] <- timefun({
                o <- seq_along(x)
                na.count <- ramorder(x, o, na.last=FALSE)
            })[3L]
            tim["orderrnk", "use"] <- timefun({
                p2 <- orderrnk(x, o, na.count)
            })[3L]
            stopifnot(identical(p2, p))

            sortordercache(x)
            tim["sort.cache", "use"] <- timefun({
                p2 <- rank.integer64(x)
            })[3L]
            stopifnot(identical(p2, p))
            remcache(x)

            ordercache(x)
            tim["order.cache", "use"] <- timefun({
                p2 <- rank.integer64(x)
            })[3L]
            stopifnot(identical(p2, p))
            remcache(x)

            if (plot) {
                barplot(t(tim), cex.names=0.7)
                title(paste0("rank.integer64(", n, ")"))
            }

            ret[["rank", as.character(n)]] <- tim
        }
    }
    if ("quantile" %in% what) {
        message("quantile: timings of different methods")
        N <- c(nsmall, nbig)
        for (i in seq_along(N)) {
            n <- N[i]
            x <- c(sample(n, n-1L, TRUE), NA)
            tim <- matrix(0.0, 6L, 3L)
            dimnames(tim) <- list(c("quantile", "quantile.64", "sortqtl", "orderqtl"
                , "sort.cache", "order.cache")
            , c("prep", "both", "use"))

            tim["quantile", "both"] <- timefun({
                p <- quantile(x, type=1L, na.rm=TRUE)
            })[3L]
            p2 <- p
            p <- as.integer64(p2)
            names(p) <- names(p2)

            x <- as.integer64(x)

            tim["quantile.64", "both"] <- timefun({
                p2 <- quantile(x, na.rm=TRUE)
            })[3L]
            stopifnot(identical(p2, p))

            tim["sortqtl", "prep"] <- timefun({
                s <- clone(x)
                na.count <- ramsort(s, na.last=FALSE)
            })[3L]
            tim["sortqtl", "use"] <- timefun({
                p2 <- sortqtl(s, na.count, seq(0.0, 1.0, 0.25))
            })[3L]
            stopifnot(identical(unname(p2), unname(p)))

            tim["orderqtl", "prep"] <- timefun({
                o <- seq_along(x)
                na.count <- ramorder(x, o, na.last=FALSE)
            })[3L]
            tim["orderqtl", "use"] <- timefun({
                p2 <- orderqtl(x, o, na.count, seq(0.0, 1.0, 0.25))
            })[3L]
            stopifnot(identical(unname(p2), unname(p)))

            sortordercache(x)
            tim["sort.cache", "use"] <- timefun({
                p2 <- quantile(x, na.rm=TRUE)
            })[3L]
            stopifnot(identical(p2, p))
            remcache(x)

            ordercache(x)
            tim["order.cache", "use"] <- timefun({
                p2 <- quantile(x, na.rm=TRUE)
            })[3L]
            stopifnot(identical(p2, p))
            remcache(x)

            if (plot) {
                barplot(t(tim), cex.names=0.7)
                title(paste0("quantile(", n, ")"))
            }

            ret[["quantile", as.character(n)]] <- tim
        }
    }

    ret

}
# nolint end: brace_linter, line_length_linter.
# nocov end

#' 64-bit integer matching
#'
#' `match` returns a vector of the positions of (first) matches of its first
#'   argument in its second.
#' `%in%` is a more intuitive interface as a binary operator, which returns a
#'   logical vector indicating if there is a match or not for its left operand.
#'
#' @param x integer64 vector: the values to be matched, optionally carrying a
#'   cache created with [hashcache()]
#' @param table integer64 vector: the values to be matched against, optionally
#'   carrying a cache created with [hashcache()] or [sortordercache()]
#' @param nomatch the value to be returned in the case when no match is found.
#'   Note that it is coerced to integer.
#' @param nunique NULL or the number of unique values of table (including NA).
#'   Providing `nunique` can speed-up matching when `table` has no cache. Note
#'   that a wrong nunique can cause undefined behaviour up to a crash.
#' @param method NULL for automatic method selection or a suitable low-level
#'   method, see details
#' @param ... ignored
#'
#' @details
#' These functions automatically choose from several low-level functions
#'   considering the size of `x` and `table` and the availability of caches.
#'
#' Suitable methods for `%in%.integer64` are
#'  - [`hashpos`] (hash table lookup)
#'  - [`hashrev`] (reverse lookup)
#'  - [`sortorderpos`] (fast ordering)
#'  - [`orderpos`] (memory saving ordering).
#'
#' Suitable methods for `match.integer64` are
#'  - [`hashfin`] (hash table lookup)
#'  - [`hashrin`] (reverse lookup)
#'  - [`sortfin`] (fast sorting)
#'  - [`orderfin`] (memory saving ordering).
#'
#' @return
#' A vector of the same length as `x`.
#'
#' `match`: An integer vector giving the position in `table` of
#'   the first match if there is a match, otherwise `nomatch`.
#'
#' If `x[i]` is found to equal `table[j]` then the value
#'   returned in the `i`-th position of the return value is `j`,
#'   for the smallest possible `j`.  If no match is found, the value
#'   is `nomatch`.
#'
#' `%in%`: A logical vector, indicating if a match was located for
#'   each element of `x`: thus the values are `TRUE` or
#'   `FALSE` and never `NA`.
#'
#' @seealso [match()]
#' @examples
#' x <- as.integer64(c(NA, 0:9), 32)
#' table <- as.integer64(c(1:9, NA))
#' match.integer64(x, table)
#' "%in%.integer64"(x, table)
#'
#' x <- as.integer64(sample(c(rep(NA, 9), 0:9), 32, TRUE))
#' table <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
#' stopifnot(identical(match.integer64(x, table), match(as.integer(x), as.integer(table))))
#' stopifnot(identical("%in%.integer64"(x, table), as.integer(x) %in% as.integer(table)))
#'
#' \dontrun{
#'     library(bit)
#'     message("check when reverse hash-lookup beats standard hash-lookup")
#'     e <- 4:24
#'     timx <- timy <- matrix(NA, length(e), length(e), dimnames=list(e, e))
#'     for (iy in seq_along(e))
#'     for (ix in 1:iy) {
#'         nx <- 2^e[ix]
#'         ny <- 2^e[iy]
#'         x <- as.integer64(sample(ny, nx, FALSE))
#'         y <- as.integer64(sample(ny, ny, FALSE))
#'         #hashfun(x, bits=as.integer(5))
#'         timx[ix, iy] <- repeat.time({
#'         hx <- hashmap(x)
#'         py <- hashrev(hx, y)
#'         })[3]
#'         timy[ix, iy] <- repeat.time({
#'         hy <- hashmap(y)
#'         px <- hashpos(hy, x)
#'         })[3]
#'         #identical(px, py)
#'         print(round(timx[1:iy, 1:iy]/timy[1:iy, 1:iy], 2), na.print="")
#'     }
#'
#'     message("explore best low-level method given size of x and table")
#'     B1 <- 1:27
#'     B2 <- 1:27
#'     tim <- array(NA, dim=c(length(B1), length(B2), 5)
#'  , dimnames=list(B1, B2, c("hashpos", "hashrev", "sortpos1", "sortpos2", "sortpos3")))
#'     for (i1 in B1)
#'     for (i2 in B2)
#'     {
#'       b1 <- B1[i1]
#'       b2 <- B1[i2]
#'       n1 <- 2^b1
#'       n2 <- 2^b2
#'       x1 <- as.integer64(c(sample(n2, n1-1, TRUE), NA))
#'       x2 <- as.integer64(c(sample(n2, n2-1, TRUE), NA))
#'       tim[i1, i2, 1] <- repeat.time({h <- hashmap(x2);hashpos(h, x1);rm(h)})[3]
#'       tim[i1, i2, 2] <- repeat.time({h <- hashmap(x1);hashrev(h, x2);rm(h)})[3]
#'       s <- clone(x2); o <- seq_along(s); ramsortorder(s, o)
#'       tim[i1, i2, 3] <- repeat.time(sortorderpos(s, o, x1, method=1))[3]
#'       tim[i1, i2, 4] <- repeat.time(sortorderpos(s, o, x1, method=2))[3]
#'       tim[i1, i2, 5] <- repeat.time(sortorderpos(s, o, x1, method=3))[3]
#'       rm(s, o)
#'       print(apply(tim, 1:2, function(ti)if(any(is.na(ti)))NA else which.min(ti)))
#'     }
#' }
#' @keywords manip logic
#' @export
match.integer64 <- function(x, table, nomatch = NA_integer_, nunique=NULL, method=NULL, ...) {
  # trivial cases for zero length input
  if (!length(x)) return(integer())
  if (!length(table)) return(rep(as.integer(c(nomatch[[1L]], NA_integer_)[1L]), length(x)))
  stopifnot(is.integer64(x))
  table <- as.integer64(table)
  cache_env <- cache(table)
  if (is.null(method)) {
    if (is.null(cache_env)) {
            nx <- length(x)
            if (is.null(nunique))
                nunique <- length(table)
            btable <- as.integer(ceiling(log2(nunique*1.5)))
            bx <- as.integer(ceiling(log2(nx*1.5)))
            if (bx<=17L && btable>=16L) {
                method <- "hashrev"
            } else {
                method <- "hashpos"
            }
    } else if (!is.null(cache_env$hashmap)) {
        method <- "hashpos"
    } else if (!is.null(cache_env$sort) && !is.null(cache_env$order) && (length(table)>length(x) || length(x)<4096L)) {
        method <- "sortorderpos"
    } else if (!is.null(cache_env$order) && (length(table)>length(x) || length(x)<4096L)) {
        method <- "orderpos"
    } else {
        nx <- length(x)
        if (is.null(nunique)) {
            if (!is.null(cache_env$nunique))
                nunique <- cache_env$nunique
            else
                nunique <- length(table)
        }
        btable <- as.integer(ceiling(log2(nunique*1.5)))
        bx <- as.integer(ceiling(log2(nx*1.5)))
        if (bx<=17L && btable>=16L) {
            method <- "hashrev"
        } else {
            method <- "hashpos"
        }
    }
  }
  method <- match.arg(method, c("hashpos", "hashrev", "sortorderpos", "orderpos"))
  switch(method,
    hashpos={
      if (is.null(cache_env) || is.null(cache_env$hashmap)) {
        if (exists("btable", inherits=FALSE)) {
          h <- hashmap(table, hashbits=btable)
        } else {
          if (is.null(nunique))
            nunique <- cache_env$nunique
          h <- hashmap(table, nunique=nunique)
        }
      } else {
        h <- cache_env
      }
      p <- hashpos(h, x, nomatch=nomatch)
    },
    hashrev={
      cache_env <- cache(x)
      if (is.null(cache_env) || is.null(cache_env$hashmap)) {
        if (exists("bx", inherits=FALSE)) {
          h <- hashmap(x, bits=bx)
        } else {
          if (is.null(nunique))
            nunique <- cache_env$nunique
          h <- hashmap(x, nunique=nunique)
        }
      } else {
        h <- cache_env
      }
      p <- hashrev(h, table, nomatch=nomatch)
    },
    sortorderpos={
      if (is.null(cache_env) || !exists("sort", cache_env) || !exists("order", cache_env)) {
        s <- clone(table)
        o <- seq_along(s)
        ramsortorder(s, o, na.last=FALSE)
      } else {
        s <- get("sort", cache_env)
        o <- get("order", cache_env)
      }
      p <- sortorderpos(s, o, x, nomatch=nomatch)
    },
    orderpos={
      if (is.null(cache_env) || !exists("order", cache_env)) {
        o <- seq_along(s)
        ramorder(table, o, na.last=FALSE)
      } else {
        o <- get("order", cache_env)
      }
      p <- orderpos(table, o, x, nomatch=nomatch)
    }
  )
  p
}

#' @rdname match.integer64
#' @export
`%in%.integer64` <- function(x, table, ...) {
  stopifnot(is.integer64(x))
  table <- as.integer64(table)
  nunique <- NULL
  cache_env <- cache(table)
  if (is.null(cache_env)) {
    nx <- length(x)
    if (is.null(nunique))
      nunique <- length(table)
    btable <- as.integer(ceiling(log2(nunique*1.5)))
    bx <- as.integer(ceiling(log2(nx*1.5)))
    if (bx<=17L && btable>=16L) {
      method <- "hashrin"
    } else {
      method <- "hashfin"
    }
  } else if (!is.null(cache_env$hashmap)) {
    method <- "hashfin"
  } else if (!is.null(cache_env$sort) && (length(table)>length(x) || length(x)<4096L)) {
    method <- "sortfin"
  } else if (!is.null(cache_env$order) && (length(table)>length(x) || length(x)<4096L)) {
    method <- "orderfin"
  } else {
    nx <- length(x)
    if (is.null(nunique)) {
      if (!is.null(cache_env$nunique))
        nunique <- cache_env$nunique
      else
        nunique <- length(table)
    }
    btable <- as.integer(ceiling(log2(nunique*1.5)))
    bx <- as.integer(ceiling(log2(nx*1.5)))
    if (bx<=17L && btable>=16L) {
      method <- "hashrin"
    } else {
      method <- "hashfin"
    }
  }
  method <- match.arg(method, c("hashfin", "hashrin", "sortfin", "orderfin"))
  switch(method,
    hashfin={
      if (is.null(cache_env) || is.null(cache_env$hashmap)) {
        if (exists("btable", inherits=FALSE)) {
          h <- hashmap(table, hashbits=btable)
        } else {
          if (is.null(nunique))
            nunique <- cache_env$nunique
          h <- hashmap(table, nunique=nunique)
        }
      } else {
        h <- cache_env
      }
      p <- hashfin(h, x)
    },
    hashrin={
      cache_env <- cache(x)
      if (is.null(cache_env) || is.null(cache_env$hashmap)) {
        if (exists("bx", inherits=FALSE)) {
          h <- hashmap(x, bits=bx)
        } else {
          if (is.null(nunique))
            nunique <- cache_env$nunique
          h <- hashmap(x, nunique=nunique)
        }
      } else {
        h <- cache_env
      }
      p <- hashrin(h, table)
    },
    sortfin={
      if (is.null(cache_env) || !exists("sort", cache_env)) {
        s <- clone(table)
        ramsort(s, na.last=FALSE)
      } else {
        s <- get("sort", cache_env)
      }
      p <- sortfin(s, x)
    },
    orderfin={
      if (is.null(cache_env) || !exists("order", cache_env)) {
        o <- seq_along(s)
        ramorder(table, o, na.last=FALSE)
      } else {
        o <- get("order", cache_env)
      }
      p <- orderfin(table, o, x)
    }
  )
  p
}

#' Determine Duplicate Elements of integer64
#'
#' `duplicated()` determines which elements of a vector or data frame are duplicates
#'   of elements with smaller subscripts, and returns a logical vector
#'   indicating which elements (rows) are duplicates.
#'
#' @param x a vector or a data frame or an array or `NULL`.
#' @param incomparables ignored
#' @param nunique NULL or the number of unique values (including NA). Providing
#'   `nunique` can speed-up matching when `x` has no cache. Note that a wrong
#'   `nunique` can cause undefined behaviour up to a crash.
#' @param method NULL for automatic method selection or a suitable low-level
#'   method, see details
#' @param ... ignored
#'
#' @details
#' This function automatically chooses from several low-level functions
#'   considering the size of `x` and the availability of a cache.
#'
#' Suitable methods are
#'  - [`hashdup`] (hashing)
#'  - [`sortorderdup`] (fast ordering)
#'  - [`orderdup`] (memory saving ordering).
#'
#' @return `duplicated()`: a logical vector of the same length as `x`.
#' @seealso [duplicated()], [unique.integer64()]
#' @examples
#' x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
#' duplicated(x)
#'
#' stopifnot(identical(duplicated(x),  duplicated(as.integer(x))))
#' @keywords logic manip
#' @export
duplicated.integer64 <- function(x, incomparables = FALSE, nunique = NULL, method = NULL, ...) {
  stopifnot(identical(incomparables, FALSE))
  cache_env <- cache(x)
  if (is.null(nunique) && !is.null(cache_env))
    nunique <- cache_env$nunique
  if (is.null(method)) {
    if (is.null(cache_env)) {
      if (length(x)>50000000L)
        method <- "sortorderdup" # nocov. Too large for practical unit tests.
      else
        method <- "hashdup"
    } else if (!is.null(cache_env$sort) && !is.null(cache_env$order)) {
      method <- "sortorderdup"
    } else if (!is.null(cache_env$hashmap)) {
      method <- "hashdup"
    } else if (!is.null(cache_env$order)) {
      method <- "orderdup"
    } else if (length(x) > 50000000L) {
      method <- "sortorderdup"
    } else {
      method <- "hashdup"
    }
  }
  method <- match.arg(method, c("hashdup", "sortorderdup", "orderdup"))
  switch(method,
    hashdup={
      if (is.null(cache_env) || is.null(cache_env$hashmap))
        h <- hashmap(x, nunique=nunique)
      else
        h <- cache_env
      p <- hashdup(h)
    },
    sortorderdup={
      if (is.null(cache_env) || is.null(cache_env$sort) || is.null(cache_env$order)) {
        s <- clone(x)
        o <- seq_along(s)
        ramsortorder(s, o, na.last=FALSE)
      } else {
        s <- get("sort", cache_env, inherits=FALSE)
        o <- get("order", cache_env, inherits=FALSE)
      }
      p <- sortorderdup(s, o)
    },
    orderdup={
      if (is.null(cache_env) || is.null(cache_env$order)) {
        o <- seq_along(s)
        ramorder(x, o, na.last=FALSE)
      } else {
        o <- get("order", cache_env, inherits=FALSE)
      }
      p <- orderdup(x, o)
    }
  )
  p
}

#' Extract Unique Elements from integer64
#'
#' `unique` returns a vector like `x` but with duplicate elements/rows removed.
#'
#' @param x a vector or a data frame or an array or `NULL`.
#' @param incomparables ignored
#' @param order The order in which unique values will be returned, see details
#' @param nunique NULL or the number of unique values (including NA). Providing
#'   `nunique` can speed-up matching when `x` has no cache. Note that a wrong
#'   `nunique`` can cause undefined behaviour up to a crash.
#' @param method NULL for automatic method selection or a suitable low-level
#'   method, see details
#' @param ... ignored
#'
#' @details
#' This function automatically chooses from several low-level functions
#'   considering the size of `x` and the availability of a cache.
#'
#' Suitable methods are
#'  - [`hashmapuni`] (simultaneously creating and using a hashmap)
#'  - [`hashuni`] (first creating a hashmap then using it)
#'  - [`sortuni`] (fast sorting for sorted order only)
#'  - [`sortorderuni`] (fast ordering for original order only)
#'  - [`orderuni`] (memory saving ordering).
#'
#' The default `order="original"` returns unique values in the order of the
#'   first appearance in `x` like in [unique()], this costs extra processing.
#'   `order="values"` returns unique values in sorted order like in [table()],
#'   this costs extra processing with the hash methods but comes for free.
#'   `order="any"` returns unique values in undefined order, possibly faster.
#'   For hash methods this will be a quasi random order, for sort methods this
#'   will be sorted order.
#'
#' @return For a vector, an object of the same type of `x`, but with only
#'   one copy of each duplicated element.  No attributes are copied (so
#'   the result has no names).
#'
#' @seealso [unique()] for the generic, [unipos()] which gives the indices
#'   of the unique elements and [table.integer64()] which gives frequencies
#'   of the unique elements.
#'
#' @examples
#' x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
#' unique(x)
#' unique(x, order="values")
#'
#' stopifnot(identical(unique(x),  x[!duplicated(x)]))
#' stopifnot(identical(unique(x),  as.integer64(unique(as.integer(x)))))
#' stopifnot(identical(unique(x, order="values")
#' ,  as.integer64(sort(unique(as.integer(x)), na.last=FALSE))))
#'
#' @keywords manip logic
#' @export
unique.integer64 <- function(x,
                             incomparables=FALSE,
                             order=c("original", "values", "any"),
                             nunique=NULL,
                             method=NULL,
                             ...) {
  stopifnot(identical(incomparables, FALSE))
  order <- match.arg(order)
  cache_env <- cache(x)
  keep.order <- order == "original"
  if (is.null(nunique) && !is.null(cache_env))
    nunique <- cache_env$nunique
  if (is.null(method)) {
    if (is.null(cache_env)) {
        if (order=="values")
            method <- "sortuni"
        else
            method <- "hashmapuni"
    } else {
      switch(order,
        original = {
          if (!is.null(cache_env$hashmap))
            method <- "hashuni"
          else if (!is.null(cache_env$order)) {
            if (!is.null(cache_env$sort))
              method <- "sortorderuni"
            else
              method <- "orderuni"
          } else {
            method <- "hashmapuni"
          }
        },
        values = {
          if (!is.null(cache_env$sort))
            method <- "sortuni"
          else if (!is.null(cache_env$order))
            method <- "orderuni"
          else if (!is.null(cache_env$hashmap) && cache_env$nunique<length(x)/2L)
            method <- "hashuni"
          else
            method <- "sortuni"
        },
        any = {
          if (!is.null(cache_env$sort))
            method <- "sortuni"
          else if (!is.null(cache_env$hashmap))
            method <- "hashuni"
          else if (!is.null(cache_env$order))
            method <- "orderuni"
          else
            method <- "sortuni"
        }
      )
    }
  }
  method <- match.arg(method, c("hashmapuni", "hashuni", "sortuni", "sortorderuni", "orderuni"))
  switch(method,
    hashmapuni={
      p <- hashmapuni(x, nunique=nunique)
    },
    hashuni={
      if (is.null(cache_env) || is.null(cache_env$hashmap))
        h <- hashmap(x, nunique=nunique)
      else
        h <- cache_env
      p <- hashuni(h, keep.order=keep.order)
      if (order=="values")
        ramsort(p, na.last=FALSE)
    },
    sortuni={
      if (is.null(cache_env) || is.null(cache_env$sort)) {
        s <- clone(x)
        ramsort(s, na.last=FALSE)
      } else {
        s <- get("sort", cache_env, inherits=FALSE)
      }
      if (is.null(nunique))
        nunique <- sortnut(s)[1L]
      p <- sortuni(s, nunique)
    },
    sortorderuni={
      if (is.null(cache_env) || is.null(cache_env$sort) || is.null(cache_env$order)) {
        s <- clone(x)
        o <- seq_along(x)
        ramsortorder(s, o, na.last=FALSE)
      } else {
        s <- get("sort", cache_env, inherits=FALSE)
        o <- get("order", cache_env, inherits=FALSE)
      }
      if (is.null(nunique))
        nunique <- sortnut(s)[1L]
      p <- sortorderuni(x, s, o, nunique)
    },
    orderuni={
      if (is.null(cache_env) || is.null(cache_env$order)) {
        o <- seq_along(x)
        ramorder(x, o, na.last=FALSE)
      } else {
        o <- get("order", cache_env, inherits=FALSE)
      }
      if (is.null(nunique))
        nunique <- ordernut(x, o)[1L]
      p <- orderuni(x, o, nunique, keep.order=keep.order)
    }
  )
  p
}

#' Extract Positions of Unique Elements
#'
#' `unipos` returns the positions of those elements returned by [unique()].
#'
#' @param x a vector or a data frame or an array or `NULL`.
#' @param incomparables ignored
#' @param order The order in which positions of unique values will be returned,
#'   see details
#' @param nunique NULL or the number of unique values (including NA). Providing
#'   `nunique` can speed-up when `x` has no cache. Note that a wrong `nunique`
#'   can cause undefined behaviour up to a crash.
#' @param method NULL for automatic method selection or a suitable low-level
#'   method, see details
#' @param ... ignored
#'
#' @details
#' This function automatically chooses from several low-level functions
#'   considering the size of `x` and the availability of a cache.
#'
#' Suitable methods are
#'  - [`hashmapupo`] (simultaneously creating and using a hashmap)
#'  - [`hashupo`] (first creating a hashmap then using it)
#'  - [`sortorderupo`] (fast ordering)
#'  - [`orderupo`] (memory saving ordering).
#'
#' The default `order="original"` collects unique values in the order of
#'   the first appearance in `x` like in [unique()], this costs extra processing.
#'   `order="values"` collects unique values in sorted order like in [table()],
#'   this costs extra processing with the hash methods but comes for free.
#'   `order="any"` collects unique values in undefined order, possibly faster.
#'   For hash methods this will be a quasi random order, for sort methods this
#'   will be sorted order.
#'
#' @return an integer vector of positions
#' @seealso [unique.integer64()] for unique values and [match.integer64()]
#'   for general matching.
#' @examples
#' x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
#' unipos(x)
#' unipos(x, order="values")
#'
#' stopifnot(identical(unipos(x),  (1:length(x))[!duplicated(x)]))
#' stopifnot(identical(unipos(x),  match.integer64(unique(x), x)))
#' stopifnot(identical(unipos(x, order="values"),  match.integer64(unique(x, order="values"), x)))
#' stopifnot(identical(unique(x),  x[unipos(x)]))
#' stopifnot(identical(unique(x, order="values"),  x[unipos(x, order="values")]))
#'
#' @keywords manip logic
#' @export
unipos <- function(x, incomparables = FALSE, order = c("original", "values", "any"), ...) UseMethod("unipos")

#' @rdname unipos
#' @export
unipos.integer64 <- function(x,
                             incomparables=FALSE,
                             order=c("original", "values", "any"),
                             nunique=NULL,
                             method=NULL,
                             ...) {
  stopifnot(identical(incomparables, FALSE))
  order <- match.arg(order)
  cache_env <- cache(x)
  keep.order <- order == "original"
  if (is.null(nunique) && !is.null(cache_env))
    nunique <- cache_env$nunique
  if (is.null(method)) {
    if (is.null(cache_env)) {
      if (order=="values")
        method <- "sortorderupo"
      else
        method <- "hashmapupo"
    } else {
      switch(order,
        original = {
          if (!is.null(cache_env$hashmap))
            method <- "hashupo"
          else if (!is.null(cache_env$order)) {
            if (!is.null(cache_env$sort))
              method <- "sortorderupo"
            else
              method <- "orderupo"
          } else {
            method <- "hashmapupo"
          }
        },
        values = {
          if (!is.null(cache_env$order)) {
            if (!is.null(cache_env$sort))
              method <- "sortorderupo"
            else
              method <- "orderupo"
          } else if (!is.null(cache_env$hashmap) && cache_env$nunique<length(x)/2L) {
            method <- "hashupo"
          } else {
            method <- "sortorderupo"
          }
        },
        any = {
          if (!is.null(cache_env$sort) && !is.null(cache_env$order))
            method <- "sortorderupo"
          else if (!is.null(cache_env$hashmap))
            method <- "hashupo"
          else if (!is.null(cache_env$order))
            method <- "orderupo"
          else
            method <- "sortorderupo"
        }
      )
    }
  }
  method <- match.arg(method, c("hashmapupo", "hashupo", "sortorderupo", "orderupo"))
  switch(method,
    hashmapupo={
      p <- hashmapupo(x, nunique=nunique)
    },
    hashupo={
      if (is.null(cache_env) || is.null(cache_env$hashmap))
        h <- hashmap(x, nunique=nunique)
      else
        h <- cache_env
      p <- hashupo(h, keep.order=keep.order)
      if (order == "values") {
        s <- x[p]
        ramsortorder(s, p, na.last=FALSE)
      }
    },
    sortorderupo={
      if (is.null(cache_env) || is.null(cache_env$sort) || is.null(cache_env$order)) {
        s <- clone(x)
        o <- seq_along(x)
        ramsortorder(s, o, na.last=FALSE)
      } else {
        s <- get("sort", cache_env, inherits=FALSE)
        o <- get("order", cache_env, inherits=FALSE)
      }
      if (is.null(nunique))
        nunique <- sortnut(s)[1L]
      p <- sortorderupo(s, o, nunique, keep.order=keep.order)
    },
    orderupo={
      if (is.null(cache_env) || is.null(cache_env$order)) {
        o <- seq_along(x)
        ramorder(x, o, na.last=FALSE)
      } else {
        o <- get("order", cache_env, inherits=FALSE)
      }
      if (is.null(nunique))
        nunique <- ordernut(x, o)[1L]
      p <- orderupo(x, o, nunique, keep.order=keep.order)
    }
  )
  p
}

#' Cross Tabulation and Table Creation for integer64
#'
#' `table.integer64` uses the cross-classifying integer64 vectors to build a
#'   contingency table of the counts at each combination of vector values.
#'
#' @param ... one or more objects which can be interpreted as factors
#'   (including character strings), or a list (or data frame) whose
#'   components can be so interpreted.  (For `as.table` and `as.data.frame`,
#'   arguments passed to specific methods.)
#' @param nunique NULL or the number of unique values of table (including NA).
#'   Providing `nunique` can speed-up matching when `table` has no cache. Note
#'   that a wrong `nunique` can cause undefined behaviour up to a crash.
#' @param order By default results are created sorted by "values", or by "counts"
#' @param method NULL for automatic method selection or a suitable low-level
#'   method, see details
#' @param return choose the return format, see details
#' @param dnn the names to be given to the dimensions in the result
#'   (the _dimnames names_).
#' @param deparse.level controls how the default `dnn` is constructed. See Details.
#'
#' @details
#' This function automatically chooses from several low-level functions considering
#'   the size of `x` and the availability of a cache.
#'
#' Suitable methods are
#'  - [`hashmaptab`] (simultaneously creating and using a hashmap)
#'  - [`hashtab`] (first creating a hashmap then using it)
#'  - [`sortordertab`] (fast ordering)
#'  - [`ordertab`] (memory saving ordering).
#'
#' If the argument `dnn` is not supplied, the internal function
#'   `list.names` is called to compute the 'dimname names'.  If the
#'   arguments in `...` are named, those names are used.  For the
#'   remaining arguments, `deparse.level = 0` gives an empty name,
#'   `deparse.level = 1` uses the supplied argument if it is a symbol,
#'   and `deparse.level = 2` will deparse the argument.
#'
#' Arguments `exclude`, `useNA`, are not supported, i.e. `NA`s are always tabulated,
#'   and, different from [table()] they are sorted first if `order="values"`.
#'
#' @return By default (with `return="table"`) [table()] returns a
#'   _contingency table_, an object of class `"table"`, an array of integer values.
#'   Note that unlike S the result is always an array, a 1D array if one factor is
#'   given. Note also that for multidimensional arrays this is a _dense_ return
#'   structure which can dramatically increase RAM requirements (for large arrays
#'   with high mutual information, i.e. many possible input combinations of which
#'   only few occur) and that [table()] is limited to `2^31` possible combinations
#'   (e.g. two input vectors with 46340 unique values only). Finally note that the
#'   tabulated values or value-combinations are represented as `dimnames` and that
#'   the implied conversion of values to strings can cause _severe_ performance
#'   problems since each string needs to be integrated into R's global string cache.
#'
#' You can use the other `return=` options to cope with these problems, the potential
#'   combination limit is increased from `2^31` to `2^63` with these options, RAM is
#'   only required for observed combinations and string conversion is avoided.
#'
#' With `return="data.frame"` you get a _dense_ representation as a [data.frame()]
#'   (like that resulting from `as.data.frame(table(...))`) where only observed
#'   combinations are listed (each as a data.frame row) with the corresponding
#'   frequency counts (the latter as component named by `responseName`). This is
#'   the inverse of [xtabs()].
#'
#' With `return="list"` you also get a _dense_ representation as a simple
#'   [list()] with components
#'  - `values` a integer64 vector of the technically tabulated values, for 1D this
#'    is the tabulated values themselves, for kD these are the values representing
#'    the potential combinations of input values
#'  - `counts` the frequency counts
#'  - `dims` only for kD: a list with the vectors of the unique values of the
#'    input dimensions
#'
#' @note Note that by using [as.integer64.factor()] we can also input
#'   factors into `table.integer64` -- only the [levels()] get lost.
#'
#' @seealso [table()] for more info on the standard version coping with Base R's
#'   data types, [tabulate()] which can faster tabulate [`integer`]s with a limited
#'   range `[1L .. nL not too big]`, [unique.integer64()] for the unique values
#'   without counting them and [unipos.integer64()] for the positions of the unique values.
#'
#' @examples
#' message("pure integer64 examples")
#' x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
#' y <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
#' z <- sample(c(rep(NA, 9), letters), 32, TRUE)
#' table.integer64(x)
#' table.integer64(x, order="counts")
#' table.integer64(x, y)
#' table.integer64(x, y, return="data.frame")
#'
#' message("via as.integer64.factor we can use 'table.integer64' also for factors")
#' table.integer64(x, as.integer64(as.factor(z)))
#' @keywords category
#' @concept counts
#' @concept frequencies
#' @concept occurrences
#' @concept contingency table
#' @export
table.integer64 <- function(...,
                            return = c("table", "data.frame", "list"),
                            order = c("values", "counts"),
                            nunique = NULL,
                            method = NULL,
                            dnn = list.names(...),
                            deparse.level = 1L) {
  order <- match.arg(order)
  return <- match.arg(return)
  # this is taken from 'table'
  list.names <- function(...) {
    l <- as.list(substitute(list(...)))[-1L]
    nm <- names(l)
    fixup <- if (is.null(nm))
      seq_along(l)
    else nm == ""
    dep <- vapply(
      l[fixup],
      function(x) switch(deparse.level + 1L, "", if (is.symbol(x)) as.character(x) else "", deparse(x, nlines=1L)[1L]),
      ""
    )
    if (is.null(nm)) {
      dep
    } else {
      nm[fixup] <- dep
      nm
    }
  }

  # COPY ON MODIFY is broken for reading from list(...)
  # because list(...) creates a copy of all ... and this invalidates our caches
  # therefore we go this sick workaround
  argsymbols <- as.list(substitute(list(...)))[-1L]
  argframe <- parent.frame()
  A <- function(i) eval(argsymbols[[i]], argframe)
  N <- length(argsymbols)
  if (!N)
      stop("nothing to tabulate")
  if (N == 1L && is.list(A(1L))) {
    args <- A(1L) # nolint: object_overwrite_linter. This code should probably be refactored anyway.
    if (length(dnn) != length(args))
      # TODO(R>=4.4.0): names(args) %||% paste(dnn[1L], seq_along(args), sep=".")
      dnn <- if (!is.null(argn <- names(args))) argn else paste(dnn[1L], seq_along(args), sep=".")
    N <- length(args)
    A <- function(i) args[[i]]
  }
  force(dnn)

  if (N==1L) {
    x <- A(1L)
    if (!is.integer64(x)) {
      warning("coercing first argument to integer64")
      x <- as.integer64(x)
    }
  } else {
    a <- A(1L)
    n <- length(a)
    nu <- integer(N)
    d <- integer64(N+1L); d[[1L]] <- 1L
    dims <- vector("list", N)
    names(dims) <- dnn
    for (i in 1:N) {
      a <- A(i)
      if (length(a) != n)
        stop("all input vectors must have the same length")
      if (!is.integer64(a)) {
        warning("coercing argument ", i, " to integer64")
        a <- as.integer64(a)
      }
      cache_env <- cache(a)
      if (is.null(cache_env$order)) {
        s <- clone(a)
        o <- seq_along(s)
        ramsortorder(s, o)
        nu[[i]] <- sortnut(s)[["nunique"]]
      } else if (is.null(cache_env$sort)) {
        o <- cache_env$order
        s <- a[o]
        nu[[i]] <- cache_env$nunique
      } else {
        o <- cache_env$order
        s <- cache_env$sort
        nu[[i]] <- cache_env$nunique
      }
      d[[i+1L]] <- d[[i]] * nu[[i]]
      if (is.na(d[[i+1L]]))
        stop("attempt to make a table from more than >= 2^63 hypothetical combinations")
      dims[[i]] <- sortuni(s, nu[[i]])
      if (i==1L)
        x <- sortorderkey(s, o) - 1L
      else
        x <- x + d[[i]] * (sortorderkey(s, o) - 1L)
    }
  }
  cache_env <- cache(x)
  if (is.null(nunique) && !is.null(cache_env))
    nunique <- cache_env$nunique
  if (is.null(method)) {
    if (is.null(cache_env)) {
      if (order=="values" && (is.null(nunique) || nunique>65536L))
        method <- "sorttab"
      else
        method <- "hashmaptab"
    } else {
      # nolint next: unnecessary_nesting_linter. Good parallelism.
      if (order=="values") {
        if (!is.null(cache_env$sort))
          method <- "sorttab"
        else if (!is.null(cache_env$hashmap) && cache_env$nunique<sqrt(length(x)))
          method <- "hashtab"
        else if (!is.null(cache_env$order))
          method <- "ordertab"
        else
          method <- "sorttab"
      } else { # order = "counts"
        # nolint next: unnecessary_nesting_linter. Good parallelism.
        if (!is.null(cache_env$hashmap))
          method <- "hashtab"
        else if (!is.null(cache_env$sort))
          method <- "sorttab"
        else if (!is.null(cache_env$order))
          method <- "ordertab"
        else
          method <- "hashmaptab"
      }
    }
  }
  method <- match.arg(method, c("hashmaptab", "hashtab", "sorttab", "ordertab"))
  switch(method,
    hashmaptab={
      tmp <- hashmaptab(x, nunique=nunique)
      cnt <- tmp$counts
      val <- tmp$values
      rm(tmp)
    },
    hashtab={
      if (is.null(cache_env) || is.null(cache_env$hashmap))
        h <- hashmap(x, nunique=nunique)
      else
        h <- cache_env
      tmp <- hashtab(h, keep.order=FALSE)
      cnt <- tmp$counts
      val <- tmp$values
      rm(tmp)
    },
    sorttab={
      if (is.null(cache_env) || is.null(cache_env$sort)) {
        s <- clone(x)
        ramsort(s, na.last=FALSE)
      } else {
        s <- get("sort", cache_env, inherits=FALSE)
      }
      if (is.null(nunique))
        nunique <- sortnut(s)[1L]
      val <- sortuni(s, nunique)
      cnt <- sorttab(s, nunique)
    },
    ordertab={
      if (is.null(cache_env) || is.null(cache_env$order)) {
        o <- seq_along(x)
        ramorder(x, o, na.last=FALSE)
      } else {
        o <- get("order", cache_env, inherits=FALSE)
      }
      if (is.null(nunique))
        nunique <- ordernut(x, o)[1L]
      val <- orderuni(x, o, nunique, keep.order=FALSE)
      cnt <- ordertab(x, o, nunique, keep.order=FALSE)
      rm(o)
    }
  )
  if (order=="values") {
    if (startsWith(method, "hash")) {
      o <- seq_along(val)
      ramsortorder(val, o, na.last=FALSE)
      cnt <- cnt[o]
    }
  } else {
    # xx workaround until we have implemented ramsort.integer
    o <- sort.list(cnt, na.last=NA, method="quick")
    cnt <- cnt[o]
    # o <- seq_along(cnt)
    # ramsortorder(cnt, o, na.last=FALSE)
    val <- val[o]
  }

  ## attaching names is extremely expensive with many unique values, doing this only for compatibility with 'table' here
  switch(return,
    table = {
      if (N == 1L) {
        attr(cnt, "dim") <- length(cnt)
        dn <- list(as.character(val))
        names(dn) <- dnn[1L]
        attr(cnt, "dimnames") <- dn
      } else {
        a <- array(0L, dim=nu, dimnames=lapply(dims, as.character))
        a[as.integer(val)+1L] <- as.integer(cnt)
        cnt <- a
      }
      oldClass(cnt) <- "table"
    },
    data.frame = {
      if (N==1L) {
        cnt <- data.frame(values=val, Freq=cnt)
        names(cnt)[[1L]] <- dnn[1L]
      } else {
        for (i in N:1) {
          w <- val %/% d[[i]]
          val <- val - d[[i]]*w
          dims[[i]] <- dims[[i]][as.integer(w)+1L]
        }
        cnt <- data.frame(dims, Freq=cnt)
      }
    },
    list = {
      if (N == 1L)
        cnt <- list(values=val, counts=cnt)
      else
        cnt <- list(values=val, counts=cnt, dims=dims)
    }
  )
  cnt
}

as.integer64.factor <- function(x, ...) as.integer64(unclass(x))

#' Extract Positions in redundant dimension table
#'
#' `keypos` returns the positions of the (fact table) elements that participate
#'   in their sorted unique subset (dimension table)
#'
#' @param x a vector or a data frame or an array or `NULL`.
#' @param method NULL for automatic method selection or a suitable low-level
#'   method, see details
#' @param ... ignored
#'
#' @details
#' NAs are sorted first in the dimension table, see [ramorder.integer64()].
#'
#' This function automatically chooses from several low-level functions
#'   considering the size of `x` and the availability of a cache.
#'
#' Suitable methods are
#'  - [`sortorderkey`] (fast ordering)
#'  - [`orderkey`] (memory saving ordering).
#'
#' @return an integer vector of the same length as `x` containing positions
#'   relative to `sort(unique(x), na.last=FALSE)`
#' @seealso [unique.integer64()] for the unique subset and [match.integer64()]
#'   for finding positions in a different vector.
#'
#' @examples
#' x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
#' keypos(x)
#'
#' stopifnot(identical(keypos(x),  match.integer64(x, sort(unique(x), na.last=FALSE))))
#' @keywords manip univar
#' @export
keypos <- function(x, ...) UseMethod("keypos")

#' @rdname keypos
#' @export
keypos.integer64 <- function(x, method = NULL, ...) {
  cache_env <- cache(x)
  if (is.null(method)) {
    if (is.null(cache_env)) {
      method <- "sortorderkey"
    } else if (!is.null(cache_env$order)) {
      if (!is.null(cache_env$sort))
        method <- "sortorderkey"
      else
        method <- "orderkey"
    } else {
      method <- "sortorderkey"
    }
  }
  method <- match.arg(method, c("sortorderkey", "orderkey"))
  switch(method,
    sortorderkey={
      if (is.null(cache_env) || is.null(cache_env$sort) || is.null(cache_env$order)) {
        s <- clone(x)
        o <- seq_along(x)
        ramsortorder(s, o, na.last=FALSE)
      } else {
        s <- get("sort", cache_env, inherits=FALSE)
        o <- get("order", cache_env, inherits=FALSE)
      }
      p <- sortorderkey(s, o)
    },
    orderkey={
      if (is.null(cache_env) || is.null(cache_env$order)) {
        o <- seq_along(x)
        ramorder(x, o, na.last=FALSE)
      } else {
        o <- get("order", cache_env, inherits=FALSE)
      }
      p <- orderkey(x, o)
    }
  )
  p
}

#' Extract Positions of Tied Elements
#'
#' `tiepos` returns the positions of those elements that participate in ties.
#'
#' @param x a vector or a data frame or an array or `NULL`.
#' @param nties NULL or the number of tied values (including NA). Providing
#'   `nties` can speed-up when `x` has no cache. Note that a wrong nties can
#'   cause undefined behaviour up to a crash.
#' @param method NULL for automatic method selection or a suitable low-level
#'   method, see details
#' @param ... ignored
#'
#' @details
#' This function automatically chooses from several low-level functions
#'   considering the size of `x` and the availability of a cache.
#'
#' Suitable methods are
#'  - [`sortordertie`] (fast ordering)
#'  - [`ordertie`] (memory saving ordering).
#'
#' @return an integer vector of positions
#' @seealso [rank.integer64()] for possibly tied ranks and [unipos.integer64()]
#'   for positions of unique values.
#'
#' @examples
#' x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
#' tiepos(x)
#'
#' stopifnot(identical(tiepos(x),  (1:length(x))[duplicated(x) | rev(duplicated(rev(x)))]))
#' @keywords manip univar
#' @export
tiepos <- function(x, ...) UseMethod("tiepos")

#' @rdname tiepos
#' @export
tiepos.integer64 <- function(x, nties = NULL, method = NULL, ...) {
  cache_env <- cache(x)
  if (is.null(nties) && !is.null(cache_env))
    nties <- cache_env$nties
  if (is.null(method)) {
    if (is.null(cache_env)) {
      method <- "sortordertie"
    } else if (!is.null(cache_env$order)) {
      if (!is.null(cache_env$sort))
        method <- "sortordertie"
      else
        method <- "ordertie"
    } else {
      method <- "sortordertie"
    }
  }
  method <- match.arg(method, c("sortordertie", "ordertie"))
  switch(method,
    sortordertie={
      if (is.null(cache_env) || is.null(cache_env$sort) || is.null(cache_env$order)) {
        s <- clone(x)
        o <- seq_along(x)
        ramsortorder(s, o, na.last=FALSE)
      } else {
        s <- get("sort", cache_env, inherits=FALSE)
        o <- get("order", cache_env, inherits=FALSE)
      }
      if (is.null(nties))
        nties <- sortnut(s)[2L]
      p <- sortordertie(s, o, nties)
    },
    ordertie={
      if (is.null(cache_env) || is.null(cache_env$order)) {
        o <- seq_along(x)
        ramorder(x, o, na.last=FALSE)
      } else {
        o <- get("order", cache_env, inherits=FALSE)
      }
      if (is.null(nties))
        nties <- ordernut(x, o)[2L]
      p <- ordertie(x, o, nties)
    }
  )
  p
}

#' Sample Ranks from integer64
#'
#' Returns the sample ranks of the values in a vector.  Ties (i.e., equal
#'   values) are averaged and missing values propagated.
#'
#' @param x a integer64 vector
#' @param method NULL for automatic method selection or a suitable low-level
#'   method, see details
#' @param ... ignored
#'
#' @details
#' This function automatically chooses from several low-level functions
#'   considering the size of `x` and the availability of a cache.
#' Suitable methods are
#'  - [sortorderrnk()] (fast ordering)
#'  - [orderrnk()] (memory saving ordering).
#'
#' @return A numeric vector of the same length as `x`.
#' @seealso [order.integer64()], [rank()] and [prank()] for percent rank.
#' @examples
#' x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
#' rank.integer64(x)
#'
#' stopifnot(identical(rank.integer64(x),  rank(as.integer(x)
#' , na.last="keep", ties.method = "average")))
#'
#' @keywords univar
#' @export
rank.integer64 <- function(x, method = NULL, ...) {
  cache_env <- cache(x)
  if (is.null(method)) {
    if (is.null(cache_env)) {
      method <- "sortorderrnk"
    } else if (!is.null(cache_env$order)) {
      if (!is.null(cache_env$sort))
        method <- "sortorderrnk"
      else
        method <- "orderrnk"
    } else {
      method <- "sortorderrnk"
    }
  }
  method <- match.arg(method, c("sortorderrnk", "orderrnk"))
  switch(method,
    sortorderrnk={
      if (is.null(cache_env) || is.null(cache_env$sort) || is.null(cache_env$order)) {
        s <- clone(x)
        o <- seq_along(x)
        na.count <- ramsortorder(s, o, na.last=FALSE)
      } else {
        s <- get("sort", cache_env, inherits=FALSE)
        o <- get("order", cache_env, inherits=FALSE)
        na.count <- get("na.count", cache_env, inherits=FALSE)
      }
      p <- sortorderrnk(s, o, na.count)
    },
    orderrnk={
      if (is.null(cache_env) || is.null(cache_env$order)) {
        o <- seq_along(x)
        na.count <- ramorder(x, o, na.last=FALSE)
      } else {
        o <- get("order", cache_env, inherits=FALSE)
        na.count <- get("na.count", cache_env, inherits=FALSE)
      }
      p <- orderrnk(x, o, na.count)
    }
  )
  p
}

#' (P)ercent (Rank)s
#'
#' Function `prank.integer64`  projects the values `[min..max]` via ranks
#'   `[1..n]` to `[0..1]`.
#' [qtile.integer64()] is the inverse function of 'prank.integer64' and
#'   projects `[0..1]` to `[min..max]`.
#'
#' @param x a integer64 vector
#' @param method NULL for automatic method selection or a suitable low-level
#'   method, see details
#' @param ... ignored
#'
#' @details Function `prank.integer64` is based on [rank.integer64()].
#' @return `prank` returns a numeric vector of the same length as `x`.
#' @seealso [rank.integer64()] for simple ranks and [qtile()] for the
#'   inverse function quantiles.
#' @examples
#' x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
#' prank(x)
#'
#' x <- x[!is.na(x)]
#' stopifnot(identical(x,  unname(qtile(x, probs=prank(x)))))
#' @keywords univar
#' @export
prank <- function(x, ...) UseMethod("prank")
#' @rdname prank
#' @export
prank.integer64 <- function(x, method = NULL, ...) {
  n <- nvalid(x)
  if (n<2L) return(rep(as.integer64(NA), length(x)))
  (rank.integer64(x, method=method, ...)-1L) / (n-1L)
}

#' (Q)uan(Tile)s
#'
#' Function [prank.integer64()]  projects the values `[min..max]` via ranks
#'   `[1..n]` to `[0..1]`.
#'
#' `qtile.integer64` is the inverse function of 'prank.integer64' and projects
#'   `[0..1]` to `[min..max]`.
#'
#' @param x a integer64 vector
#' @param probs numeric vector of probabilities with values in `[0, 1]` - possibly containing `NA`s
#' @param ... ignored
#'
#' @details
#'
#' Functions `quantile.integer64` with `type=0` and `median.integer64` are
#'   convenience wrappers to `qtile`.
#'
#' Function `qtile` behaves very similar to `quantile.default` with `type=1`
#'   in that it only returns existing values, it is mostly symmetric but it is
#'   using 'round' rather than 'floor'.
#'
#' Note that this implies that `median.integer64` does not interpolate for even
#'   number of values (interpolation would create values that could not be
#'   represented as 64-bit integers).
#'
#' This function automatically chooses from several low-level functions
#'   considering the size of `x` and the availability of a cache.
#'
#' Suitable methods are
#'  - [`sortqtl`] (fast sorting)
#'  - [`orderqtl`] (memory saving ordering).
#'
#' @return
#' `prank` returns a numeric vector of the same length as `x`.
#'
#' `qtile` returns a vector with elements from `x`
#'   at the relative positions specified by `probs`.
#' @seealso [rank.integer64()] for simple ranks and [quantile()] for quantiles.
#' @examples
#' x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
#' qtile(x, probs=seq(0, 1, 0.25))
#' quantile(x, probs=seq(0, 1, 0.25), na.rm=TRUE)
#' median(x, na.rm=TRUE)
#' summary(x)
#'
#' x <- x[!is.na(x)]
#' stopifnot(identical(x,  unname(qtile(x, probs=prank(x)))))
#' @keywords univar
#' @export
qtile <- function(x, probs = seq(0.0, 1.0, 0.25), ...) UseMethod("qtile")

#' @rdname qtile
#' @param names logical; if `TRUE`, the result has a `names` attribute. Set to `FALSE` for speedup with many probs.
#' @param method NULL for automatic method selection or a suitable low-level method, see details
#' @export
qtile.integer64 <- function(x, probs = seq(0.0, 1.0, 0.25), names = TRUE, method = NULL, ...) {
  if (any(is.na(probs) | probs<0.0 | probs>1.0))
    stop("p outside [0, 1]")
  cache_env <- cache(x)
  if (is.null(method)) {
    if (is.null(cache_env))
      method <- "sortqtl"
    else if (!is.null(cache_env$sort))
      method <- "sortqtl"
    else if (!is.null(cache_env$order))
      method <- "orderqtl"
    else
      method <- "sortqtl"
  }
  method <- match.arg(method, c("sortqtl", "orderqtl"))
  switch(method,
    sortqtl={
      if (is.null(cache_env) || is.null(cache_env$sort)) {
        s <- clone(x)
        na.count <- ramsort(s, na.last=FALSE)
      } else {
        s <- get("sort", cache_env, inherits=FALSE)
        na.count <- get("na.count", cache_env, inherits=FALSE)
      }
      qs <- sortqtl(s, na.count, probs)
    },
    orderqtl={
      if (is.null(cache_env) || is.null(cache_env$order)) {
        o <- seq_along(x)
        na.count <- ramorder(x, o, na.last=FALSE)
      } else {
        o <- get("order", cache_env, inherits=FALSE)
        na.count <- get("na.count", cache_env, inherits=FALSE)
      }
      qs <- orderqtl(x, o, na.count, probs)
    }
  )
  if (names) {
    np <- length(probs)
    dig <- max(2L, getOption("digits"))
    names(qs) <- paste0(
      if (np < 100L)
        formatC(100.0 * probs, format = "fg", width = 1L, digits = dig)
      else
        format(100.0 * probs, trim = TRUE, digits = dig),
      "%"
    )
  }
  qs
}

#' @rdname qtile
#' @param type an integer selecting the quantile algorithm, currently only
#'   0 is supported, see details
#' @param na.rm logical; if `TRUE`, any `NA` and `NaN`'s are removed from
#'   `x` before the quantiles are computed.
#' @export
quantile.integer64 <- function(x, probs = seq(0.0, 1.0, 0.25), na.rm = FALSE, names = TRUE, type=0L, ...) {
    if (type[[1L]]!=0L)
        stop("only type==0 ('qtile') supported")
    if (!na.rm && na.count(x)>0L)
        stop("missing values not allowed with 'na.rm='==FALSE")
    qtile.integer64(x, probs = probs, na.rm = na.rm, names = names, ...)
}

#' @rdname qtile
#' @export
median.integer64 <- function(x, na.rm=FALSE, ...) {
  if (!na.rm && na.count(x)>0L) return(NA_integer64_)
  if (!length(x)) return(NA_integer64_)
  qtile.integer64(x, probs = 0.5, na.rm = na.rm, names = FALSE)
}

#' @rdname qtile
#' @export
mean.integer64 <- function(x, na.rm=FALSE, ...) {
    ret <- .Call(C_mean_integer64, x, as.logical(na.rm), double(1L))
    oldClass(ret) <- "integer64"
    ret
}

#' @rdname qtile
#' @param object a integer64 vector
#' @export
summary.integer64 <- function(object, ...) {
    nas <- na.count(object)
    qq <- quantile(object, na.rm=TRUE)
    qq <- c(qq[1L:3L], mean(object, na.rm=TRUE), qq[4L:5L])
    names(qq) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
    if (any(nas))
        c(qq, "NA's" = nas)
    else qq
}
