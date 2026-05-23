# High-level intger64 methods for sorting and ordering

Fast high-level methods for sorting and ordering. These are wrappers to
[`ramsort.integer64()`](https://bit64.r-lib.org/reference/ramsort.integer64.md)
and friends and do not modify their arguments.

## Usage

``` r
# S3 method for class 'integer64'
sort(
  x,
  decreasing = FALSE,
  has.na = TRUE,
  na.last = TRUE,
  stable = TRUE,
  optimize = c("time", "memory"),
  VERBOSE = FALSE,
  ...
)

# S3 method for class 'integer64'
order(
  ...,
  na.last = TRUE,
  decreasing = FALSE,
  has.na = TRUE,
  stable = TRUE,
  optimize = c("time", "memory"),
  VERBOSE = FALSE
)
```

## Arguments

- x:

  a vector to be sorted by
  [`ramsort.integer64()`](https://bit64.r-lib.org/reference/ramsort.integer64.md)
  and
  [`ramsortorder.integer64()`](https://bit64.r-lib.org/reference/ramsort.integer64.md),
  i.e. the output of `sort.integer64()`

- decreasing:

  boolean scalar telling ramsort whether to sort increasing or
  decreasing

- has.na:

  boolean scalar defining whether the input vector might contain `NA`s.
  If we know we don't have NAs, this may speed-up. *Note* that you risk
  a crash if there are unexpected `NA`s with `has.na=FALSE`

- na.last:

  boolean scalar telling ramsort whether to sort `NA`s last or first.
  *Note* that 'boolean' means that there is no third option `NA` as in
  [`sort()`](https://rdrr.io/r/base/sort.html)

- stable:

  boolean scalar defining whether stable sorting is needed. Allowing
  non-stable may speed-up.

- optimize:

  by default ramsort optimizes for 'time' which requires more RAM, set
  to 'memory' to minimize RAM requirements and sacrifice speed

- VERBOSE:

  cat some info about chosen method

- ...:

  further arguments, passed from generics, ignored in methods

## Value

`sort` returns the sorted vector and `vector` returns the order
positions.

## Details

see [`sort()`](https://rdrr.io/r/base/sort.html) and
[`order()`](https://bit64.r-lib.org/reference/bit64S3.md)

## See also

[`sort()`](https://rdrr.io/r/base/sort.html),
[`sortcache()`](https://bit64.r-lib.org/reference/hashcache.md)

## Examples

``` r
  x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
  x
#> integer64
#>  [1] 6    2    9    8    1    <NA> <NA> 5    9    6    1    3    2   
#> [14] 4    <NA> 8    <NA> <NA> <NA> <NA> 7    7    1    1    <NA> <NA>
#> [27] <NA> 2    <NA> 4    9    2   
  sort(x)
#> integer64
#>  [1] 1    1    1    1    2    2    2    2    3    4    4    5    6   
#> [14] 6    7    7    8    8    9    9    9    <NA> <NA> <NA> <NA> <NA>
#> [27] <NA> <NA> <NA> <NA> <NA> <NA>
  message(
    "the following has default optimize='time' which is faster ",
    "but requires more RAM, this calls 'ramorder'"
  )
#> the following has default optimize='time' which is faster but requires more RAM, this calls 'ramorder'
  order(x)
#>  [1]  5 11 23 24  2 13 28 32 12 14 30  8  1 10 21 22  4 16  3  9 31  6
#> [23]  7 15 17 18 19 20 25 26 27 29
  message("slower with less RAM, this calls 'ramsortorder'")
#> slower with less RAM, this calls 'ramsortorder'
  order(x, optimize="memory")
#>  [1]  5 11 23 24  2 13 28 32 12 14 30  8  1 10 21 22  4 16  3  9 31  6
#> [23]  7 15 17 18 19 20 25 26 27 29
```
