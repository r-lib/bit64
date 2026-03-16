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
#>  [1] <NA> 7    7    1    1    <NA> <NA> <NA> 2    <NA> 4    9    2   
#> [14] <NA> 3    <NA> 6    <NA> 5    3    6    <NA> <NA> <NA> <NA> 1   
#> [27] <NA> 1    5    1    <NA> 2   
  sort(x)
#> integer64
#>  [1] 1    1    1    1    1    2    2    2    3    3    4    5    5   
#> [14] 6    6    7    7    9    <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>
#> [27] <NA> <NA> <NA> <NA> <NA> <NA>
  message(
    "the following has default optimize='time' which is faster ",
    "but requires more RAM, this calls 'ramorder'"
  )
#> the following has default optimize='time' which is faster but requires more RAM, this calls 'ramorder'
  order(x)
#>  [1]  4  5 26 28 30  9 13 32 15 20 11 19 29 17 21  2  3 12  1  6  7  8
#> [23] 10 14 16 18 22 23 24 25 27 31
  message("slower with less RAM, this calls 'ramsortorder'")
#> slower with less RAM, this calls 'ramsortorder'
  order(x, optimize="memory")
#>  [1]  4  5 26 28 30  9 13 32 15 20 11 19 29 17 21  2  3 12  1  6  7  8
#> [23] 10 14 16 18 22 23 24 25 27 31
```
