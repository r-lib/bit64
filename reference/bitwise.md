# Bitwise Logical Operations

Logical operations on integer vectors with elements viewed as sets of
bits. As soon as an integer64 vector is involved, the operations are
performed using integer64 semantics. Otherwise the base package
functions are called.

## Usage

``` r
bitwNot(a)

bitwAnd(a, b)

bitwOr(a, b)

bitwXor(a, b)

bitwShiftL(a, n)

bitwShiftR(a, n)
```

## Arguments

- a, b:

  integer vectors; numeric vectors are coerced to integer vectors.

- n:

  non-negative integer vector of values up to 31.

## Value

An integer64 vector of length the longer of the arguments, or zero
length if one is zero-length.

## See also

[bitwAnd](https://rdrr.io/r/base/bitwise.html),
[bitwOr](https://rdrr.io/r/base/bitwise.html),
[bitwXor](https://rdrr.io/r/base/bitwise.html),
[bitwNot](https://rdrr.io/r/base/bitwise.html),
[bitwShiftL](https://rdrr.io/r/base/bitwise.html),
[bitwShiftR](https://rdrr.io/r/base/bitwise.html)

## Examples

``` r
x <- as.integer64(1:5)
y <- c(1L, 3L, 5L, 7L)
bitwAnd(x, y)
#> integer64
#> [1] 1 2 1 4 1
bitwOr(x, y)
#> integer64
#> [1] 1 3 7 7 5
bitwXor(x, y)
#> integer64
#> [1] 0 1 6 3 4
bitwNot(x)
#> integer64
#> [1] -2 -3 -4 -5 -6
bitwShiftL(x, 1L)
#> integer64
#> [1] 2  4  6  8  10
bitwShiftR(x, 1L)
#> integer64
#> [1] 0 1 1 2 2
```
