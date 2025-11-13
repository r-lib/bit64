# Identity function for class 'integer64'

This will discover any deviation between objects containing integer64
vectors.

## Usage

``` r
identical.integer64(
  x,
  y,
  num.eq = FALSE,
  single.NA = FALSE,
  attrib.as.set = TRUE,
  ignore.bytecode = TRUE,
  ignore.environment = FALSE,
  ignore.srcref = TRUE,
  ...
)
```

## Arguments

- x, y:

  Atomic vector of class 'integer64'

- num.eq, single.NA, attrib.as.set, ignore.bytecode, ignore.environment,
  ignore.srcref:

  See [`identical()`](https://rdrr.io/r/base/identical.html).

- ...:

  Passed on to [`identical()`](https://rdrr.io/r/base/identical.html).
  Only `extptr.as.ref=` is available as of R 4.4.1, and then only for
  versions of R \>= 4.2.0.

## Value

A single logical value, `TRUE` or `FALSE`, never `NA` and never anything
other than a single value.

## Details

This is simply a wrapper to
[`identical()`](https://rdrr.io/r/base/identical.html) with default
arguments `num.eq = FALSE, single.NA = FALSE`.

## See also

[`==.integer64`](https://bit64.r-lib.org/reference/xor.integer64.md)
[`identical()`](https://rdrr.io/r/base/identical.html)
[`integer64()`](https://bit64.r-lib.org/reference/bit64-package.md)

## Examples

``` r
  i64 <- as.double(NA); class(i64) <- "integer64"
  identical(i64-1, i64+1)
#> [1] TRUE
  identical.integer64(i64-1, i64+1)
#> [1] FALSE
```
