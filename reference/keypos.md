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
#>  [1]  6  5  5  4 10  3  1  7  1  1  1  1  1  2  9  1  9  2  1  8  1  6
#> [23]  1  1  1  2  1  1  1  6  1  2

stopifnot(identical(keypos(x),  match.integer64(x, sort(unique(x), na.last=FALSE))))
#> Warning: Detected that 'match.integer64' was called directly. Instead only call 'match' and rely on S3 dispatch. To suppress this warning, e.g. if this is a false positive, use options(bit64.warn.exported.s3.method = FALSE). In the next version, this symbol will stop being exported.
```
