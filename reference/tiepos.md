# Extract Positions of Tied Elements

`tiepos` returns the positions of those elements that participate in
ties.

## Usage

``` r
tiepos(x, ...)

# S3 method for class 'integer64'
tiepos(x, nties = NULL, method = NULL, ...)
```

## Arguments

- x:

  a vector or a data frame or an array or `NULL`.

- ...:

  ignored

- nties:

  NULL or the number of tied values (including NA). Providing `nties`
  can speed-up when `x` has no cache. Note that a wrong nties can cause
  undefined behaviour up to a crash.

- method:

  NULL for automatic method selection or a suitable low-level method,
  see details

## Value

an integer vector of positions

## Details

This function automatically chooses from several low-level functions
considering the size of `x` and the availability of a cache.

Suitable methods are

- [`sortordertie`](https://bit64.r-lib.org/reference/sortnut.md) (fast
  ordering)

- [`ordertie`](https://bit64.r-lib.org/reference/sortnut.md) (memory
  saving ordering).

## See also

[`rank.integer64()`](https://bit64.r-lib.org/reference/rank.integer64.md)
for possibly tied ranks and
[`unipos.integer64()`](https://bit64.r-lib.org/reference/unipos.md) for
positions of unique values.

## Examples

``` r
x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
tiepos(x)
#>  [1]  1  2  3  4  7 10 11 12 13 14 15 16 17 18 19 20 22 24 25 26 27 28
#> [23] 29 30 31

stopifnot(identical(tiepos(x),  (1:length(x))[duplicated(x) | rev(duplicated(rev(x)))]))
```
