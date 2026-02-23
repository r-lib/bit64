# (P)ercent (Rank)s

Function `prank.integer64` projects the values `[min..max]` via ranks
`[1..n]` to `[0..1]`.
[`qtile.integer64()`](https://bit64.r-lib.org/reference/qtile.md) is the
inverse function of 'prank.integer64' and projects `[0..1]` to
`[min..max]`.

## Usage

``` r
prank(x, ...)

# S3 method for class 'integer64'
prank(x, method = NULL, ...)
```

## Arguments

- x:

  a integer64 vector

- ...:

  ignored

- method:

  NULL for automatic method selection or a suitable low-level method,
  see details

## Value

`prank` returns a numeric vector of the same length as `x`.

## Details

Function `prank.integer64` is based on
[`rank.integer64()`](https://bit64.r-lib.org/reference/rank.integer64.md).

## See also

[`rank.integer64()`](https://bit64.r-lib.org/reference/rank.integer64.md)
for simple ranks and
[`qtile()`](https://bit64.r-lib.org/reference/qtile.md) for the inverse
function quantiles.

## Examples

``` r
x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
prank(x)
#>  [1]      NA 0.50000 0.21875 0.21875 0.21875 0.21875      NA 0.50000
#>  [9]      NA      NA      NA 0.00000      NA      NA 0.21875      NA
#> [17] 0.90625 1.00000 0.50000 0.90625 0.62500      NA      NA 0.75000
#> [25] 0.75000 0.75000      NA      NA      NA 0.21875      NA      NA

x <- x[!is.na(x)]
stopifnot(identical(x,  unname(qtile(x, probs=prank(x)))))
```
