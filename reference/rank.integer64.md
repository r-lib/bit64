# Sample Ranks from integer64

Returns the sample ranks of the values in a vector. Ties (i.e., equal
values) are averaged and missing values propagated.

## Usage

``` r
# S3 method for class 'integer64'
rank(x, method = NULL, ...)
```

## Arguments

- x:

  a integer64 vector

- method:

  NULL for automatic method selection or a suitable low-level method,
  see details

- ...:

  ignored

## Value

A numeric vector of the same length as `x`.

## Details

This function automatically chooses from several low-level functions
considering the size of `x` and the availability of a cache. Suitable
methods are

- [`sortorderrnk()`](https://bit64.r-lib.org/reference/sortnut.md) (fast
  ordering)

- [`orderrnk()`](https://bit64.r-lib.org/reference/sortnut.md) (memory
  saving ordering).

## See also

[`order.integer64()`](https://bit64.r-lib.org/reference/sort.integer64.md),
[`rank()`](https://bit64.r-lib.org/reference/bit64S3.md) and
[`prank()`](https://bit64.r-lib.org/reference/prank.md) for percent
rank.

## Examples

``` r
x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
rank.integer64(x)
#>  [1]   NA   NA   NA   NA  2.0   NA  4.0   NA  8.0   NA   NA   NA   NA
#> [14] 10.0   NA  8.0   NA  8.0   NA 11.5   NA   NA  1.0   NA  5.0  3.0
#> [27]   NA   NA   NA   NA 11.5  6.0

stopifnot(identical(rank.integer64(x),  rank(as.integer(x)
, na.last="keep", ties.method = "average")))
```
