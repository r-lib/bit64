# Summary functions for integer64 vectors

Summary functions for integer64 vectors. Function 'range' without
arguments returns the smallest and largest value of the 'integer64'
class.

## Usage

``` r
# S3 method for class 'integer64'
any(..., na.rm = FALSE)

# S3 method for class 'integer64'
all(..., na.rm = FALSE)

# S3 method for class 'integer64'
sum(..., na.rm = FALSE)

# S3 method for class 'integer64'
prod(..., na.rm = FALSE)

# S3 method for class 'integer64'
min(..., na.rm = FALSE)

# S3 method for class 'integer64'
max(..., na.rm = FALSE)

# S3 method for class 'integer64'
range(..., na.rm = FALSE, finite = FALSE)

lim.integer64()
```

## Arguments

- ...:

  atomic vectors of class 'integer64'

- na.rm:

  logical scalar indicating whether to ignore NAs

- finite:

  logical scalar indicating whether to ignore NAs (just for
  compatibility with
  [`range.default()`](https://rdrr.io/r/base/range.html))

## Value

[`all()`](https://rdrr.io/r/base/all.html) and
[`any()`](https://rdrr.io/r/base/any.html) return a logical scalar

[`range()`](https://rdrr.io/r/base/range.html) returns a integer64
vector with two elements

[`min()`](https://rdrr.io/r/base/Extremes.html),
[`max()`](https://rdrr.io/r/base/Extremes.html),
[`sum()`](https://rdrr.io/r/base/sum.html) and
[`prod()`](https://rdrr.io/r/base/prod.html) return a integer64 scalar

## Details

The numerical summary methods always return `integer64`. Wherever
integer methods would return `Inf` (or its negation), here the extreme
64-bit integer `9223372036854775807` is returned. See
[`min()`](https://rdrr.io/r/base/Extremes.html) for more details about
the behavior.

`lim.integer64` returns these limits in proper order
`-9223372036854775807, +9223372036854775807` and without a
[`warning()`](https://rdrr.io/r/base/warning.html).

## See also

[`mean.integer64()`](https://bit64.r-lib.org/reference/qtile.md)
[`cumsum.integer64()`](https://bit64.r-lib.org/reference/cumsum.integer64.md)
[`integer64()`](https://bit64.r-lib.org/reference/bit64-package.md)

## Examples

``` r
  lim.integer64()
#> integer64
#> [1] -9223372036854775807 9223372036854775807 
  range(as.integer64(1:12))
#> integer64
#> [1] 1  12
```
