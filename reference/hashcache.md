# Big caching of hashing, sorting, ordering

Functions to create cache that accelerates many operations

## Usage

``` r
hashcache(x, nunique = NULL, ...)

sortcache(x, has.na = NULL, na.last = FALSE)

sortordercache(x, has.na = NULL, stable = NULL, na.last = FALSE)

ordercache(x, has.na = NULL, stable = NULL, optimize = "time", na.last = FALSE)
```

## Arguments

- x:

  an atomic vector (note that currently only integer64 is supported)

- nunique:

  giving *correct* number of unique elements can help reducing the size
  of the hashmap

- ...:

  passed to [`hashmap()`](https://bit64.r-lib.org/reference/hashmap.md)

- has.na:

  boolean scalar defining whether the input vector might contain `NA`s.
  If we know we don't have `NA`s, this may speed-up. *Note* that you
  risk a crash if there are unexpected `NA`s with `has.na=FALSE`.

- na.last:

  boolean scalar defining whether NA should be last.

- stable:

  boolean scalar defining whether stable sorting is needed. Allowing
  non-stable may speed-up.

- optimize:

  by default ramsort optimizes for 'time' which requires more RAM, set
  to 'memory' to minimize RAM requirements and sacrifice speed.

## Value

`x` with a [`cache()`](https://bit64.r-lib.org/reference/cache.md) that
contains the result of the expensive operations, possible together with
small derived information (such as
[`nunique.integer64()`](https://bit64.r-lib.org/reference/is.sorted.integer64.md))
and previously cached results.

## Details

The result of relative expensive operations
[`hashmap()`](https://bit64.r-lib.org/reference/hashmap.md),
[`bit::ramsort()`](https://rdrr.io/pkg/bit/man/Sorting.html),
[`bit::ramsortorder()`](https://rdrr.io/pkg/bit/man/Sorting.html), and
[`bit::ramorder()`](https://rdrr.io/pkg/bit/man/Sorting.html) can be
stored in a cache in order to avoid multiple excutions. Unless in very
specific situations, the recommended method is `hashsortorder` only.

## Note

Note that we consider storing the big results from sorting and/or
ordering as a relevant side-effect, and therefore storing them in the
cache should require a conscious decision of the user.

## See also

[`cache()`](https://bit64.r-lib.org/reference/cache.md) for caching
functions and
[`nunique.integer64()`](https://bit64.r-lib.org/reference/is.sorted.integer64.md)
for methods benefiting from small caches

## Examples

``` r
  x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
  sortordercache(x)
```
