# Coerce to integer64

Methods to coerce from other atomic types to integer64.

## Usage

``` r
as.integer64(x, ...)

# S3 method for class '`NULL`'
as.integer64(x, ...)

# S3 method for class 'integer64'
as.integer64(x, ...)

# S3 method for class 'double'
as.integer64(x, ...)

# S3 method for class 'complex'
as.integer64(x, ...)

# S3 method for class 'integer'
as.integer64(x, ...)

# S3 method for class 'raw'
as.integer64(x, ...)

# S3 method for class 'logical'
as.integer64(x, ...)

# S3 method for class 'character'
as.integer64(x, ...)

# S3 method for class 'factor'
as.integer64(x, ...)

# S3 method for class 'Date'
as.integer64(x, ...)

# S3 method for class 'POSIXct'
as.integer64(x, ...)

# S3 method for class 'POSIXlt'
as.integer64(x, ...)

# S3 method for class 'difftime'
as.integer64(x, units = "auto", ...)

# S3 method for class 'bitstring'
as.integer64(x, ...)

NA_integer64_
```

## Format

An object of class `integer64` of length 1.

## Arguments

- x:

  an atomic vector

- ..., units:

  further arguments to the
  [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html)

## Value

The other methods return atomic vectors of the expected types

## Details

`as.integer64.character` is realized using C function `strtoll` which
does not support scientific notation. Instead of '1e6' use '1000000'.
`as.integer64.bitstring` evaluates characters '0' and ' ' as zero-bit,
all other one byte characters as one-bit, multi-byte characters are not
allowed, strings shorter than 64 characters are treated as if they were
left-padded with '0', strings longer than 64 bytes are mapped to
`NA_INTEGER64` and a warning is emitted.

## See also

[`as.character.integer64()`](https://bit64.r-lib.org/reference/as.character.integer64.md)
[`integer64()`](https://bit64.r-lib.org/reference/bit64-package.md)

## Examples

``` r
as.integer64(as.character(lim.integer64()))
#> integer64
#> [1] -9223372036854775807 9223372036854775807 
as.integer64(
  structure(c("1111111111111111111111111111111111111111111111111111111111111110",
              "1111111111111111111111111111111111111111111111111111111111111111",
              "1000000000000000000000000000000000000000000000000000000000000000",
              "0000000000000000000000000000000000000000000000000000000000000000",
              "0000000000000000000000000000000000000000000000000000000000000001",
              "0000000000000000000000000000000000000000000000000000000000000010"
  ), class = "bitstring")
)
#> integer64
#> [1] -2   -1   <NA> 0    1    2   
as.integer64(
 structure(c("............................................................... ",
             "................................................................",
             ".                                                               ",
             "",
             ".",
             "10"
  ), class = "bitstring")
)
#> integer64
#> [1] -2   -1   <NA> 0    1    2   
```
