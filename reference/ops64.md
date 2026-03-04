# Binary operators for integer64 vectors

Binary operators for integer64 vectors.

## Usage

``` r
binattr(e1, e2)

# S3 method for class 'integer64'
e1 + e2

# S3 method for class 'integer64'
e1 - e2

# S3 method for class 'integer64'
e1%/%e2

# S3 method for class 'integer64'
e1%%e2

# S3 method for class 'integer64'
e1 * e2

# S3 method for class 'integer64'
e1^e2

# S3 method for class 'integer64'
e1/e2

# S3 method for class 'integer64'
e1 == e2

# S3 method for class 'integer64'
e1 != e2

# S3 method for class 'integer64'
e1 < e2

# S3 method for class 'integer64'
e1 <= e2

# S3 method for class 'integer64'
e1 > e2

# S3 method for class 'integer64'
e1 >= e2

# S3 method for class 'integer64'
e1 & e2

# S3 method for class 'integer64'
e1 | e2

# S3 method for class 'integer64'
!x
```

## Arguments

- e1, e2, x:

  numeric or complex vectors or objects which can be coerced to such, or
  other objects for which methods have been written for - especially
  'integer64' vectors.

## Value

`&`, [`|`](https://rdrr.io/r/base/Logic.html),
[`!`](https://rdrr.io/r/base/Logic.html),
[`!=`](https://rdrr.io/r/base/Comparison.html),
[`==`](https://rdrr.io/r/base/Comparison.html), `<`, `<=`, `>`, `>=`
return a logical vector

[`/`](https://rdrr.io/r/base/Arithmetic.html) returns a double vector

[`+`](https://rdrr.io/r/base/Arithmetic.html),
[`-`](https://rdrr.io/r/base/Arithmetic.html),
[`*`](https://rdrr.io/r/base/Arithmetic.html),
[`%/%`](https://rdrr.io/r/base/Arithmetic.html),
[`%%`](https://rdrr.io/r/base/Arithmetic.html),
[`^`](https://rdrr.io/r/base/Arithmetic.html) return a vector of class
'integer64' or different class depending on the operands

## See also

[`integer64()`](https://bit64.r-lib.org/reference/bit64-package.md)

## Examples

``` r
  as.integer64(1:12) - 1
#> integer64
#>  [1] 0  1  2  3  4  5  6  7  8  9  10 11
  options(integer64_semantics="new")
  d <- 2.5
  i <- as.integer64(5)
  d/i  # new 0.5
#> [1] 0.5
  d*i  # new 13
#> integer64
#> [1] 13
  i*d  # new 13
#> integer64
#> [1] 13
  options(integer64_semantics="old")
  d/i  # old: 0.4
#> [1] 0.4
  d*i  # old: 10
#> integer64
#> [1] 10
  i*d  # old: 13
#> integer64
#> [1] 13
```
