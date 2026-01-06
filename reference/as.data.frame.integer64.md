# integer64: Coercing to data.frame column

Coercing integer64 vector to data.frame.

## Usage

``` r
# S3 method for class 'integer64'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  an integer64 vector

- row.names, optional, ...:

  passed to NextMethod
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) after
  removing the 'integer64' class attribute

## Value

a one-column data.frame containing an integer64 vector

## Details

'as.data.frame.integer64' is rather not intended to be called directly,
but it is required to allow integer64 as data.frame columns.

## Note

This is currently very slow – any ideas for improvement?

## See also

[`cbind.integer64()`](https://bit64.r-lib.org/reference/c.integer64.md)
[`integer64()`](https://bit64.r-lib.org/reference/bit64-package.md)

## Examples

``` r
  as.data.frame(as.integer64(1:12))
#>     x
#> 1   1
#> 2   2
#> 3   3
#> 4   4
#> 5   5
#> 6   6
#> 7   7
#> 8   8
#> 9   9
#> 10 10
#> 11 11
#> 12 12
  data.frame(a=1:12, b=as.integer64(1:12))
#>     a  b
#> 1   1  1
#> 2   2  2
#> 3   3  3
#> 4   4  4
#> 5   5  5
#> 6   6  6
#> 7   7  7
#> 8   8  8
#> 9   9  9
#> 10 10 10
#> 11 11 11
#> 12 12 12
```
