# (Q)uan(Tile)s

Function
[`prank.integer64()`](https://bit64.r-lib.org/reference/prank.md)
projects the values `[min..max]` via ranks `[1..n]` to `[0..1]`.

## Usage

``` r
qtile(x, probs = seq(0, 1, 0.25), ...)

# S3 method for class 'integer64'
qtile(x, probs = seq(0, 1, 0.25), names = TRUE, method = NULL, ...)

# S3 method for class 'integer64'
quantile(
  x,
  probs = seq(0, 1, 0.25),
  na.rm = FALSE,
  names = TRUE,
  type = 0L,
  ...
)

# S3 method for class 'integer64'
median(x, na.rm = FALSE, ...)

# S3 method for class 'integer64'
mean(x, na.rm = FALSE, ...)

# S3 method for class 'integer64'
summary(object, ...)
```

## Arguments

- x:

  a integer64 vector

- probs:

  numeric vector of probabilities with values in `[0, 1]` - possibly
  containing `NA`s

- ...:

  ignored

- names:

  logical; if `TRUE`, the result has a `names` attribute. Set to `FALSE`
  for speedup with many probs.

- method:

  NULL for automatic method selection or a suitable low-level method,
  see details

- na.rm:

  logical; if `TRUE`, any `NA` and `NaN`'s are removed from `x` before
  the quantiles are computed.

- type:

  an integer selecting the quantile algorithm, currently only 0 is
  supported, see details

- object:

  a integer64 vector

## Value

`prank` returns a numeric vector of the same length as `x`.

`qtile` returns a vector with elements from `x` at the relative
positions specified by `probs`.

## Details

`qtile.integer64` is the inverse function of 'prank.integer64' and
projects `[0..1]` to `[min..max]`.

Functions `quantile.integer64` with `type=0` and `median.integer64` are
convenience wrappers to `qtile`.

Function `qtile` behaves very similar to `quantile.default` with
`type=1` in that it only returns existing values, it is mostly symmetric
but it is using 'round' rather than 'floor'.

Note that this implies that `median.integer64` does not interpolate for
even number of values (interpolation would create values that could not
be represented as 64-bit integers).

This function automatically chooses from several low-level functions
considering the size of `x` and the availability of a cache.

Suitable methods are

- [`sortqtl`](https://bit64.r-lib.org/reference/sortnut.md) (fast
  sorting)

- [`orderqtl`](https://bit64.r-lib.org/reference/sortnut.md) (memory
  saving ordering).

## See also

[`rank.integer64()`](https://bit64.r-lib.org/reference/rank.integer64.md)
for simple ranks and
[`quantile()`](https://rdrr.io/r/stats/quantile.html) for quantiles.

## Examples

``` r
x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
qtile(x, probs=seq(0, 1, 0.25))
#> integer64
#>   0%  25%  50%  75% 100% 
#>    1    3    4    5    8 
quantile(x, probs=seq(0, 1, 0.25), na.rm=TRUE)
#> integer64
#>   0%  25%  50%  75% 100% 
#>    1    3    4    5    8 
median(x, na.rm=TRUE)
#> integer64
#> [1] 4
summary(x)
#> integer64
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>       1       3       4       4       5       8      16 

x <- x[!is.na(x)]
stopifnot(identical(x,  unname(qtile(x, probs=prank(x)))))
```
