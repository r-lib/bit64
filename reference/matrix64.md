# Working with integer64 arrays and matrices

These functions and methods facilitate working with integer64 objects
stored in matrices. As ever, the primary motivation for having
tailor-made functions here is that R's methods often receive input from
bit64 and treat the vectors as doubles, leading to unexpected and/or
incorrect results.

## Usage

``` r
# S3 method for class 'integer64'
matrix(data = NA_integer64_, ...)

# S3 method for class 'integer64'
array(data = NA_integer64_, ...)

# S3 method for class 'integer64'
colSums(x, na.rm = FALSE, dims = 1L)

# S3 method for class 'integer64'
rowSums(x, na.rm = FALSE, dims = 1L)

# S3 method for class 'integer64'
aperm(a, perm, ...)

matrix(data = NA, nrow = 1L, ncol = 1L, byrow = FALSE, dimnames = NULL)

array(data = NA, dim = length(data), dimnames = NULL)

colSums(x, na.rm = FALSE, dims = 1L)

# Default S3 method
colSums(x, na.rm = FALSE, dims = 1L)

rowSums(x, na.rm = FALSE, dims = 1L)

# Default S3 method
rowSums(x, na.rm = FALSE, dims = 1L)
```

## Arguments

- data, nrow, ncol, byrow, dimnames, dim:

  Arguments for `matrix()` and `array()`.

- ...:

  Passed on to subsequent methods.

- x:

  An array of integer64 numbers.

- na.rm, dims:

  Same interpretation as in `colSums()`.

- a, perm:

  Passed on to [`aperm()`](https://rdrr.io/r/base/aperm.html).

## Details

As of now, the `colSums()` and `rowSums()` methods are implemented as
wrappers around equivalent
[`apply()`](https://rdrr.io/r/base/apply.html) approaches, because
re-using the default routine (and then applying integer64 to the result)
does not work for objects with missing elements. Ideally this would
eventually get its own dedicated C routine mimicking that of `colSums()`
for integers; feature requests and PRs welcome.

[`aperm()`](https://rdrr.io/r/base/aperm.html) is required for
[`apply()`](https://rdrr.io/r/base/apply.html) to work, in general,
otherwise `FUN` gets applied to a class-stripped version of the input.

## Examples

``` r
A = matrix(as.integer64(1:6), 3)

colSums(A)
#> integer64
#> [1] 6  15
rowSums(A)
#> integer64
#> [1] 5 7 9
aperm(A, 2:1)
#> integer64
#>      [,1] [,2] [,3]
#> [1,] 1    2    3   
#> [2,] 4    5    6   
```
