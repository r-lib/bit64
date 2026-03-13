# Small cache access methods

These methods are packaged here for methods in packages `bit64` and
`ff`.

## Usage

``` r
# S3 method for class 'integer64'
na.count(x, ...)

# S3 method for class 'integer64'
nvalid(x, ...)

# S3 method for class 'integer64'
is.sorted(x, ...)

# S3 method for class 'integer64'
nunique(x, ...)

# S3 method for class 'integer64'
nties(x, ...)
```

## Arguments

- x:

  some object

- ...:

  ignored

## Value

`is.sorted` returns a logical scalar, the other methods return an
integer scalar.

## Details

All these functions benefit from a
[`sortcache()`](https://bit64.r-lib.org/reference/hashcache.md),
[`ordercache()`](https://bit64.r-lib.org/reference/hashcache.md) or
[`sortordercache()`](https://bit64.r-lib.org/reference/hashcache.md).
`na.count()`, `nvalid()` and `nunique()` also benefit from a
[`hashcache()`](https://bit64.r-lib.org/reference/hashcache.md).

## Functions

- `na.count(integer64)`: returns the number of `NA`s

- `nvalid(integer64)`: returns the number of valid data points, usually
  [`length()`](https://rdrr.io/r/base/length.html) minus `na.count`.

- `is.sorted(integer64)`: checks for sortedness of `x` (NAs sorted
  first)

- `nunique(integer64)`: returns the number of unique values

- `nties(integer64)`: returns the number of tied values.

## Note

If a [`cache()`](https://bit64.r-lib.org/reference/cache.md) exists but
the desired value is not cached, then these functions will store their
result in the cache. We do not consider this a relevant side-effect,
since these small cache results do not have a relevant memory footprint.

## See also

[`cache()`](https://bit64.r-lib.org/reference/cache.md) for caching
functions and
[`sortordercache()`](https://bit64.r-lib.org/reference/hashcache.md) for
functions creating big caches

## Examples

``` r
 x = as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
 length(x)
#> [1] 32
 bit::na.count(x)
#> [1] 16
 bit::nvalid(x)
#> [1] 16
 bit::nunique(x)
#> [1] 8
 bit::nties(x)
#> [1] 30
 table(x)
#> x
#> 2 3 4 5 7 8 9 
#> 2 1 1 2 3 2 5 
 x
#> integer64
#>  [1] <NA> <NA> <NA> 2    <NA> <NA> <NA> 7    4    9    8    <NA> <NA>
#> [14] 8    9    5    <NA> <NA> 5    7    <NA> <NA> 2    <NA> 9    9   
#> [27] <NA> 9    7    <NA> <NA> 3   
```
