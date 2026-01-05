# Replicate elements of integer64 vectors

Replicate elements of integer64 vectors

## Arguments

- x:

  a vector of 'integer64' to be replicated

- ...:

  further arguments passed to
  [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html)

## Value

[`rep()`](https://rdrr.io/r/base/rep.html) returns a integer64 vector

## See also

[`c.integer64()`](https://bit64.r-lib.org/reference/c.integer64.md)
`rep.integer64()`
[`as.data.frame.integer64()`](https://bit64.r-lib.org/reference/as.data.frame.integer64.md)
[`integer64()`](https://bit64.r-lib.org/reference/bit64-package.md)

## Examples

``` r
  rep(as.integer64(1:2), 6)
#> integer64
#>  [1] 1 2 1 2 1 2 1 2 1 2 1 2
  rep(as.integer64(1:2), c(6, 6))
#> integer64
#>  [1] 1 1 1 1 1 1 2 2 2 2 2 2
  rep(as.integer64(1:2), length.out=6)
#> integer64
#> [1] 1 2 1 2 1 2
```
