# Extract Positions in redundant dimension table

`keypos` returns the positions of the (fact table) elements that
participate in their sorted unique subset (dimension table)

## Usage

``` r
keypos(x, ...)

# S3 method for class 'integer64'
keypos(x, method = NULL, ...)
```

## Arguments

- x:

  a vector or a data frame or an array or `NULL`.

- ...:

  ignored

- method:

  NULL for automatic method selection or a suitable low-level method,
  see details

## Value

an integer vector of the same length as `x` containing positions
relative to `sort(unique(x), na.last=FALSE)`

## Details

NAs are sorted first in the dimension table, see
[`ramorder.integer64()`](https://bit64.r-lib.org/reference/ramsort.integer64.md).

This function automatically chooses from several low-level functions
considering the size of `x` and the availability of a cache.

Suitable methods are

- [`sortorderkey`](https://bit64.r-lib.org/reference/sortnut.md) (fast
  ordering)

- [`orderkey`](https://bit64.r-lib.org/reference/sortnut.md) (memory
  saving ordering).

## See also

[`unique.integer64()`](https://bit64.r-lib.org/reference/unique.integer64.md)
for the unique subset and
[`match.integer64()`](https://bit64.r-lib.org/reference/match.integer64.md)
for finding positions in a different vector.

## Examples

``` r
x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
keypos(x)
#>  [1] 3 8 3 1 1 1 4 1 2 6 3 1 3 6 7 4 1 1 4 1 5 8 1 1 1 1 3 1 6 1 5 7
```
