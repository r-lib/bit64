# Generating sequence of integer64 values

Generating sequence of integer64 values

## Usage

``` r
# S3 method for class 'integer64'
seq(
  from = NULL,
  to = NULL,
  by = NULL,
  length.out = NULL,
  along.with = NULL,
  ...
)
```

## Arguments

- from:

  integer64 scalar (in order to dispatch the integer64 method of
  [`seq()`](https://rdrr.io/r/base/seq.html))

- to:

  scalar

- by:

  scalar

- length.out:

  scalar

- along.with:

  scalar

- ...:

  ignored

## Value

An integer64 vector with the generated sequence

## Details

`seq.integer64` coerces its arguments `from`, `to`, and `by` to
`integer64`. Consistency with [`seq()`](https://rdrr.io/r/base/seq.html)
is typically maintained, though results may differ when mixing
`integer64` and `double` inputs, for the same reason that any arithmetic
with these mixed types can be ambiguous. Whereas
`seq(1L, 10L, length.out=8L)` can back up to double storage to give an
exact result, this not possible for generic inputs
`seq(i64, dbl, length.out=n)`.

## See also

[`c.integer64()`](https://bit64.r-lib.org/reference/c.integer64.md)
[`rep.integer64()`](https://bit64.r-lib.org/reference/rep.integer64.md)
[`as.data.frame.integer64()`](https://bit64.r-lib.org/reference/as.data.frame.integer64.md)
[`integer64()`](https://bit64.r-lib.org/reference/bit64-package.md)

## Examples

``` r
seq(as.integer64(1), 12, 2)
#> integer64
#> [1] 1  3  5  7  9  11
seq(as.integer64(1), by=2, length.out=6)
#> integer64
#> [1] 1  3  5  7  9  11

# truncation rules
seq(as.integer64(1), 10, by=1.5)
#> integer64
#>  [1] 1  2  3  4  5  6  7  8  9  10
seq(as.integer64(1), 10, length.out=5)
#> integer64
#> [1] 1 3 5 7 9
```
