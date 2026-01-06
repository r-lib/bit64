# Turning base R functions into S3 generics for bit64

Turn those base functions S3 generic which are used in bit64

## Usage

``` r
from:to
is.double(x)
match(x, table, ...)
x %in% table
rank(x, ...)
order(...)

# Default S3 method
is.double(x)

# S3 method for class 'integer64'
is.double(x)

# S3 method for class 'integer64'
mtfrm(x)

# Default S3 method
match(x, table, ...)

# Default S3 method
x %in% table

# Default S3 method
rank(x, ...)

# Default S3 method
order(...)
```

## Arguments

- x:

  integer64 vector: the values to be matched, optionally carrying a
  cache created with
  [`hashcache()`](https://bit64.r-lib.org/reference/hashcache.md)

- table:

  integer64 vector: the values to be matched against, optionally
  carrying a cache created with
  [`hashcache()`](https://bit64.r-lib.org/reference/hashcache.md) or
  [`sortordercache()`](https://bit64.r-lib.org/reference/hashcache.md)

- ...:

  ignored

- from:

  scalar denoting first element of sequence

- to:

  scalar denoting last element of sequence

## Value

[`invisible()`](https://rdrr.io/r/base/invisible.html)

## Details

The following functions are turned into S3 generics in order to dispatch
methods for
[`integer64()`](https://bit64.r-lib.org/reference/bit64-package.md):

- `:`

- `is.double()`

- `match()`

- `%in%`

- `rank()`

- `order()`

## Note

- `is.double()` returns `FALSE` for
  [`integer64`](https://bit64.r-lib.org/reference/bit64-package.md)

- `:` currently only dispatches at its first argument, thus
  `as.integer64(1):9` works but `1:as.integer64(9)` doesn't

- `match()` currently only dispatches at its first argument and expects
  its second argument also to be integer64, otherwise throws an error.
  Beware of something like `match(2, as.integer64(0:3))`

- `%in%` currently only dispatches at its first argument and expects its
  second argument also to be integer64, otherwise throws an error.
  Beware of something like `2 %in% as.integer64(0:3)`

- `order()` currently only orders a single argument, trying more than
  one raises an error

## See also

[`bit64()`](https://bit64.r-lib.org/reference/bit64-package.md), S3

## Examples

``` r
is.double(as.integer64(1))
#> [1] FALSE
as.integer64(1):9
#> integer64
#> [1] 1 2 3 4 5 6 7 8 9
match(as.integer64(2), as.integer64(0:3))
#> [1] 3
as.integer64(2) %in% as.integer64(0:3)
#> [1] TRUE

unique(as.integer64(c(1,1,2)))
#> integer64
#> [1] 1 2
rank(as.integer64(c(1,1,2)))
#> [1] 1.5 1.5 3.0


order(as.integer64(c(1,NA,2)))
#> [1] 1 3 2
```
