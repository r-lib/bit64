# integer64: Sequence Generation

Generating sequence of integer64 values

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

an integer64 vector with the generated sequence

## Details

`seq.integer64` does coerce its arguments 'from', 'to' and 'by' to
`integer64`. If not provided, the argument 'by' is automatically
determined as `+1` or `-1`, but the size of 'by' is not calculated as in
[`seq()`](https://rdrr.io/r/base/seq.html) (because this might result in
a non-integer value).

## Note

In base R [`:`](https://bit64.r-lib.org/reference/bit64S3.md) currently
is not generic and does not dispatch, see section "Limitations inherited
from Base R" in
[`integer64()`](https://bit64.r-lib.org/reference/bit64-package.md)

## See also

[`c.integer64()`](https://bit64.r-lib.org/reference/c.integer64.md)
[`rep.integer64()`](https://bit64.r-lib.org/reference/rep.integer64.md)
[`as.data.frame.integer64()`](https://bit64.r-lib.org/reference/as.data.frame.integer64.md)
[`integer64()`](https://bit64.r-lib.org/reference/bit64-package.md)

## Examples

``` r
  # colon not activated: as.integer64(1):12
  seq(as.integer64(1), 12, 2)
#> integer64
#> [1] 1  3  5  7  9  11
  seq(as.integer64(1), by=2, length.out=6)
#> integer64
#> [1] 1  3  5  7  9  11
```
