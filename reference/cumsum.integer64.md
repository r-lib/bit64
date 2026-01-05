# Cumulative Sums, Products, Extremes and lagged differences

Cumulative Sums, Products, Extremes and lagged differences

## Usage

``` r
# S3 method for class 'integer64'
diff(x, lag = 1L, differences = 1L, ...)

# S3 method for class 'integer64'
cummin(x)

# S3 method for class 'integer64'
cummax(x)

# S3 method for class 'integer64'
cumsum(x)

# S3 method for class 'integer64'
cumprod(x)
```

## Arguments

- x:

  an atomic vector of class 'integer64'

- lag:

  see [`diff()`](https://rdrr.io/r/base/diff.html)

- differences:

  see [`diff()`](https://rdrr.io/r/base/diff.html)

- ...:

  ignored

## Value

[`cummin()`](https://rdrr.io/r/base/cumsum.html),
[`cummax()`](https://rdrr.io/r/base/cumsum.html) ,
[`cumsum()`](https://rdrr.io/r/base/cumsum.html) and
[`cumprod()`](https://rdrr.io/r/base/cumsum.html) return a integer64
vector of the same length as their input

[`diff()`](https://rdrr.io/r/base/diff.html) returns a integer64 vector
shorter by `lag*differences` elements

## See also

[`sum.integer64()`](https://bit64.r-lib.org/reference/sum.integer64.md)
[`integer64()`](https://bit64.r-lib.org/reference/bit64-package.md)

## Examples

``` r
  cumsum(rep(as.integer64(1), 12))
#> integer64
#>  [1] 1  2  3  4  5  6  7  8  9  10 11 12
  diff(as.integer64(c(0, 1:12)))
#> integer64
#>  [1] 1 1 1 1 1 1 1 1 1 1 1 1
  cumsum(as.integer64(c(0, 1:12)))
#> integer64
#>  [1] 0  1  3  6  10 15 21 28 36 45 55 66 78
  diff(cumsum(as.integer64(c(0, 0, 1:12))), differences=2)
#> integer64
#>  [1] 1 1 1 1 1 1 1 1 1 1 1 1
```
