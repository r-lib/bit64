# Atomic Caching

Functions for caching results attached to atomic objects

## Usage

``` r
newcache(x)

jamcache(x)

cache(x)

setcache(x, which, value)

getcache(x, which)

remcache(x)

# S3 method for class 'cache'
print(x, all.names = FALSE, pattern, ...)
```

## Arguments

- x:

  an integer64 vector (or a cache object in case of `print.cache`)

- which:

  A character naming the object to be retrieved from the cache or to be
  stored in the cache

- value:

  An object to be stored in the cache

- all.names, pattern:

  passed to [`ls()`](https://rdrr.io/r/base/ls.html) when listing the
  cache content

- ...:

  ignored

## Value

See details

## Details

A `cache` is an [`environment`](https://rdrr.io/r/base/environment.html)
attached to an atomic object with the
[`attribute`](https://rdrr.io/r/base/attr.html) name 'cache'. It
contains at least a reference to the atomic object that carries the
cache. This is used when accessing the cache to detect whether the
object carrying the cache has been modified meanwhile.

## Functions

- `newcache()`: creates a new cache referencing `x`

- `jamcache()`: forces `x` to have a cache

- `cache()`: returns the cache attached to `x` if it is not found to be
  outdated

- `setcache()`: assigns a value into the cache of `x`

- `getcache()`: gets cache value 'which' from `x`

- `remcache()`: removes the cache from `x`

## See also

[`bit::still.identical()`](https://rdrr.io/pkg/bit/man/still.identical.html)
for testing whether to symbols point to the same RAM.

Functions that get and set small cache-content automatically when a
cache is present:
[`bit::na.count()`](https://rdrr.io/pkg/bit/man/Metadata.html),
[`bit::nvalid()`](https://rdrr.io/pkg/bit/man/Metadata.html),
[`bit::is.sorted()`](https://rdrr.io/pkg/bit/man/Metadata.html),
[`bit::nunique()`](https://rdrr.io/pkg/bit/man/Metadata.html) and
[`bit::nties()`](https://rdrr.io/pkg/bit/man/Metadata.html)

Setting big caches with a relevant memory footprint requires a conscious
decision of the user:
[`hashcache`](https://bit64.r-lib.org/reference/hashcache.md),
[`sortcache`](https://bit64.r-lib.org/reference/hashcache.md),
[`ordercache`](https://bit64.r-lib.org/reference/hashcache.md),
[`sortordercache`](https://bit64.r-lib.org/reference/hashcache.md)

Functions that use big caches:
[`match.integer64()`](https://bit64.r-lib.org/reference/match.integer64.md),
[`%in%.integer64`](https://bit64.r-lib.org/reference/match.integer64.md),
[`duplicated.integer64()`](https://bit64.r-lib.org/reference/duplicated.integer64.md),
[`unique.integer64()`](https://bit64.r-lib.org/reference/unique.integer64.md),
[`unipos()`](https://bit64.r-lib.org/reference/unipos.md),
[`table()`](https://bit64.r-lib.org/reference/table.md),
[`keypos()`](https://bit64.r-lib.org/reference/keypos.md),
[`tiepos()`](https://bit64.r-lib.org/reference/tiepos.md),
[`rank.integer64()`](https://bit64.r-lib.org/reference/rank.integer64.md),
[`prank()`](https://bit64.r-lib.org/reference/prank.md),
[`qtile()`](https://bit64.r-lib.org/reference/qtile.md),
[`quantile.integer64()`](https://bit64.r-lib.org/reference/qtile.md),
[`median.integer64()`](https://bit64.r-lib.org/reference/qtile.md), and
[`summary.integer64()`](https://bit64.r-lib.org/reference/qtile.md)

## Examples

``` r
  x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
  y <- x
  bit::still.identical(x, y)
#> [1] TRUE
  y[1] <- NA
  bit::still.identical(x, y)
#> [1] FALSE
  mycache <- newcache(x)
  ls(mycache)
#> [1] "x"
  mycache
#> cache_integer64: x
  rm(mycache)
  jamcache(x)
#> cache_integer64: x
  cache(x)
#> cache_integer64: x
  x[1] <- NA
  cache(x)
#> Warning: removed outdated cache
#> NULL
  getcache(x, "abc")
#> NULL
  setcache(x, "abc", 1)
#> cache_integer64: abc - x
  getcache(x, "abc")
#> [1] 1
  remcache(x)
  cache(x)
#> NULL
```
