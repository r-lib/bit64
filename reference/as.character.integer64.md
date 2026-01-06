# Coerce from integer64

Methods to coerce integer64 to other atomic types. 'as.bitstring'
coerces to a human-readable bit representation (strings of zeroes and
ones). The methods [`format()`](https://rdrr.io/r/base/format.html),
[`as.character()`](https://rdrr.io/r/base/character.html),
[`as.double()`](https://rdrr.io/r/base/double.html),
[`as.logical()`](https://rdrr.io/r/base/logical.html),
[`as.integer()`](https://rdrr.io/r/base/integer.html) do what you would
expect.

## Usage

``` r
as.bitstring(x, ...)

# S3 method for class 'integer64'
as.double(x, ...)

# S3 method for class 'integer64'
as.numeric(x, ...)

# S3 method for class 'integer64'
as.complex(x, ...)

# S3 method for class 'integer64'
as.integer(x, ...)

# S3 method for class 'integer64'
as.raw(x, ...)

# S3 method for class 'integer64'
as.logical(x, ...)

# S3 method for class 'integer64'
as.character(x, ...)

# S3 method for class 'integer64'
as.bitstring(x, ...)

# S3 method for class 'integer64'
as.Date(x, origin, ...)

# S3 method for class 'integer64'
as.POSIXct(x, tz = "", origin, ...)

# S3 method for class 'integer64'
as.POSIXlt(x, tz = "", origin, ...)

# S3 method for class 'bitstring'
print(x, ...)

# S3 method for class 'integer64'
as.list(x, ...)
```

## Arguments

- x:

  an integer64 vector

- ..., origin, tz:

  further arguments to the
  [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html)

## Value

`as.bitstring` returns a string of class 'bitstring'.

The other methods return atomic vectors of the expected types

## See also

[`as.integer64.character()`](https://bit64.r-lib.org/reference/as.integer64.character.md)
[`integer64()`](https://bit64.r-lib.org/reference/bit64-package.md)

## Examples

``` r
  as.character(lim.integer64())
#> [1] "-9223372036854775807" "9223372036854775807" 
  as.bitstring(lim.integer64())
#> [1] "1000000000000000000000000000000000000000000000000000000000000001"
#> [2] "0111111111111111111111111111111111111111111111111111111111111111"
  as.bitstring(as.integer64(c(-2, -1, NA, 0:2)))
#> [1] "1111111111111111111111111111111111111111111111111111111111111110"
#> [2] "1111111111111111111111111111111111111111111111111111111111111111"
#> [3] "1000000000000000000000000000000000000000000000000000000000000000"
#> [4] "0000000000000000000000000000000000000000000000000000000000000000"
#> [5] "0000000000000000000000000000000000000000000000000000000000000001"
#> [6] "0000000000000000000000000000000000000000000000000000000000000010"
```
