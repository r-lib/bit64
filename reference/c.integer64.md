# Concatenating integer64 vectors

The ususal functions 'c', 'cbind' and 'rbind'

## Usage

``` r
# S3 method for class 'integer64'
c(..., recursive = FALSE)

# S3 method for class 'integer64'
cbind(...)

# S3 method for class 'integer64'
rbind(...)
```

## Arguments

- ...:

  two or more arguments coerced to 'integer64' and passed to
  [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html)

- recursive:

  logical. If `recursive = TRUE`, the function recursively descends
  through lists (and pairlists) combining all their elements into a
  vector.

## Value

[`c()`](https://rdrr.io/r/base/c.html) returns a integer64 vector of the
total length of the input

[`cbind()`](https://rdrr.io/r/base/cbind.html) and
[`rbind()`](https://rdrr.io/r/base/cbind.html) return a integer64 matrix

## Note

R currently only dispatches generic 'c' to method 'c.integer64' if the
first argument is 'integer64'

## See also

[`rep.integer64()`](https://bit64.r-lib.org/reference/rep.integer64.md)
[`seq.integer64()`](https://bit64.r-lib.org/reference/seq.integer64.md)
[`as.data.frame.integer64()`](https://bit64.r-lib.org/reference/as.data.frame.integer64.md)
[`integer64()`](https://bit64.r-lib.org/reference/bit64-package.md)

## Examples

``` r
  c(as.integer64(1), 2:6)
#> integer64
#> [1] 1 2 3 4 5 6
  cbind(1:6, as.integer64(1:6))
#> integer64
#>      [,1] [,2]
#> [1,] 1    1   
#> [2,] 2    2   
#> [3,] 3    3   
#> [4,] 4    4   
#> [5,] 5    5   
#> [6,] 6    6   
  rbind(1:6, as.integer64(1:6))
#> integer64
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,] 1    2    3    4    5    6   
#> [2,] 1    2    3    4    5    6   
```
