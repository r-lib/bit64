# Extract or Replace Parts of an integer64 vector

Methods to extract and replace parts of an integer64 vector.

## Usage

``` r
# S3 method for class 'integer64'
x[i, ...]

# S3 method for class 'integer64'
x[...] <- value

# S3 method for class 'integer64'
x[[...]]

# S3 method for class 'integer64'
x[[...]] <- value
```

## Arguments

- x:

  an atomic vector

- i:

  indices specifying elements to extract

- ...:

  further arguments to the
  [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html)

- value:

  an atomic vector with values to be assigned

## Value

A vector or scalar of class 'integer64'

## Note

You should not subscript non-existing elements and not use `NA`s as
subscripts. The current implementation returns `9218868437227407266`
instead of `NA`.

## See also

[`[`](https://rdrr.io/r/base/Extract.html)
[`integer64()`](https://bit64.r-lib.org/reference/bit64-package.md)

## Examples

``` r
  as.integer64(1:12)[1:3]
#> integer64
#> [1] 1 2 3
  x <- matrix(as.integer64(1:12), nrow = 3L)
  x
#> integer64
#>      [,1] [,2] [,3] [,4]
#> [1,] 1    4    7    10  
#> [2,] 2    5    8    11  
#> [3,] 3    6    9    12  
  x[]
#> integer64
#>      [,1] [,2] [,3] [,4]
#> [1,] 1    4    7    10  
#> [2,] 2    5    8    11  
#> [3,] 3    6    9    12  
  x[, 2:3]
#> integer64
#>      [,1] [,2]
#> [1,] 4    7   
#> [2,] 5    8   
#> [3,] 6    9   
```
