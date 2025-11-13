# Unary operators and functions for integer64 vectors

Unary operators and functions for integer64 vectors.

## Usage

``` r
# S3 method for class 'integer64'
format(x, justify = "right", ...)

# S3 method for class 'integer64'
sign(x)

# S3 method for class 'integer64'
abs(x)

# S3 method for class 'integer64'
sqrt(x)

# S3 method for class 'integer64'
log(x, base = NULL)

# S3 method for class 'integer64'
log10(x)

# S3 method for class 'integer64'
log2(x)

# S3 method for class 'integer64'
trunc(x, ...)

# S3 method for class 'integer64'
floor(x)

# S3 method for class 'integer64'
ceiling(x)

# S3 method for class 'integer64'
signif(x, digits = 6L)

# S3 method for class 'integer64'
scale(x, center = TRUE, scale = TRUE)

# S3 method for class 'integer64'
round(x, digits = 0L)

# S3 method for class 'integer64'
is.na(x)

# S3 method for class 'integer64'
is.finite(x)

# S3 method for class 'integer64'
is.infinite(x)

# S3 method for class 'integer64'
is.nan(x)

# S3 method for class 'integer64'
!x
```

## Arguments

- x:

  an atomic vector of class 'integer64'

- justify:

  should it be right-justified (the default), left-justified, centred or
  left alone.

- ...:

  further arguments to the
  [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html)

- base:

  an atomic scalar (we save 50% log-calls by not allowing a vector base)

- digits:

  integer indicating the number of decimal places (round) or significant
  digits (signif) to be used. Negative values are allowed (see
  [`round()`](https://rdrr.io/r/base/Round.html))

- center:

  see [`scale()`](https://rdrr.io/r/base/scale.html)

- scale:

  see [`scale()`](https://rdrr.io/r/base/scale.html)

## Value

[`format()`](https://rdrr.io/r/base/format.html) returns a character
vector

[`is.na()`](https://rdrr.io/r/base/NA.html) and
[`!`](https://rdrr.io/r/base/Logic.html) return a logical vector

[`sqrt()`](https://rdrr.io/r/base/MathFun.html),
[`log()`](https://rdrr.io/r/base/Log.html),
[`log2()`](https://rdrr.io/r/base/Log.html) and
[`log10()`](https://rdrr.io/r/base/Log.html) return a double vector

[`sign()`](https://rdrr.io/r/base/sign.html),
[`abs()`](https://rdrr.io/r/base/MathFun.html),
[`floor()`](https://rdrr.io/r/base/Round.html),
[`ceiling()`](https://rdrr.io/r/base/Round.html),
[`trunc()`](https://rdrr.io/r/base/Round.html) and
[`round()`](https://rdrr.io/r/base/Round.html) return a vector of class
'integer64'

[`signif()`](https://rdrr.io/r/base/Round.html) is not implemented

## See also

[`xor.integer64()`](https://bit64.r-lib.org/reference/xor.integer64.md)
[`integer64()`](https://bit64.r-lib.org/reference/bit64-package.md)

## Examples

``` r
  sqrt(as.integer64(1:12))
#>  [1] 1.000000 1.414214 1.732051 2.000000 2.236068 2.449490 2.645751
#>  [8] 2.828427 3.000000 3.162278 3.316625 3.464102
```
