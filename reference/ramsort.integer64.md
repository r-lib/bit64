# Low-level intger64 methods for in-RAM sorting and ordering

Fast low-level methods for sorting and ordering. The `..sortorder`
methods do sorting and ordering at once, which requires more RAM than
ordering but is (almost) as fast as as sorting.

## Usage

``` r
# S3 method for class 'integer64'
shellsort(x, has.na = TRUE, na.last = FALSE, decreasing = FALSE, ...)

# S3 method for class 'integer64'
shellsortorder(x, i, has.na = TRUE, na.last = FALSE, decreasing = FALSE, ...)

# S3 method for class 'integer64'
shellorder(x, i, has.na = TRUE, na.last = FALSE, decreasing = FALSE, ...)

# S3 method for class 'integer64'
mergesort(x, has.na = TRUE, na.last = FALSE, decreasing = FALSE, ...)

# S3 method for class 'integer64'
mergeorder(x, i, has.na = TRUE, na.last = FALSE, decreasing = FALSE, ...)

# S3 method for class 'integer64'
mergesortorder(x, i, has.na = TRUE, na.last = FALSE, decreasing = FALSE, ...)

# S3 method for class 'integer64'
quicksort(
  x,
  has.na = TRUE,
  na.last = FALSE,
  decreasing = FALSE,
  restlevel = floor(1.5 * log2(length(x))),
  ...
)

# S3 method for class 'integer64'
quicksortorder(
  x,
  i,
  has.na = TRUE,
  na.last = FALSE,
  decreasing = FALSE,
  restlevel = floor(1.5 * log2(length(x))),
  ...
)

# S3 method for class 'integer64'
quickorder(
  x,
  i,
  has.na = TRUE,
  na.last = FALSE,
  decreasing = FALSE,
  restlevel = floor(1.5 * log2(length(x))),
  ...
)

# S3 method for class 'integer64'
radixsort(
  x,
  has.na = TRUE,
  na.last = FALSE,
  decreasing = FALSE,
  radixbits = 8L,
  ...
)

# S3 method for class 'integer64'
radixsortorder(
  x,
  i,
  has.na = TRUE,
  na.last = FALSE,
  decreasing = FALSE,
  radixbits = 8L,
  ...
)

# S3 method for class 'integer64'
radixorder(
  x,
  i,
  has.na = TRUE,
  na.last = FALSE,
  decreasing = FALSE,
  radixbits = 8L,
  ...
)

# S3 method for class 'integer64'
ramsort(
  x,
  has.na = TRUE,
  na.last = FALSE,
  decreasing = FALSE,
  stable = TRUE,
  optimize = c("time", "memory"),
  VERBOSE = FALSE,
  ...
)

# S3 method for class 'integer64'
ramsortorder(
  x,
  i,
  has.na = TRUE,
  na.last = FALSE,
  decreasing = FALSE,
  stable = TRUE,
  optimize = c("time", "memory"),
  VERBOSE = FALSE,
  ...
)

# S3 method for class 'integer64'
ramorder(
  x,
  i,
  has.na = TRUE,
  na.last = FALSE,
  decreasing = FALSE,
  stable = TRUE,
  optimize = c("time", "memory"),
  VERBOSE = FALSE,
  ...
)
```

## Arguments

- x:

  a vector to be sorted by `ramsort.integer64()` and
  `ramsortorder.integer64()`, i.e. the output of
  [`sort.integer64()`](https://bit64.r-lib.org/reference/sort.integer64.md)

- has.na:

  boolean scalar defining whether the input vector might contain `NA`s.
  If we know we don't have NAs, this may speed-up. *Note* that you risk
  a crash if there are unexpected `NA`s with `has.na=FALSE`

- na.last:

  boolean scalar telling ramsort whether to sort `NA`s last or first.
  *Note* that 'boolean' means that there is no third option `NA` as in
  [`sort()`](https://rdrr.io/r/base/sort.html)

- decreasing:

  boolean scalar telling ramsort whether to sort increasing or
  decreasing

- ...:

  further arguments, passed from generics, ignored in methods

- i:

  integer positions to be modified by `ramorder.integer64()` and
  `ramsortorder.integer64()`, default is 1:n, in this case the output is
  similar to
  [`order.integer64()`](https://bit64.r-lib.org/reference/sort.integer64.md)

- restlevel:

  number of remaining recursionlevels before `quicksort` switches from
  recursing to `shellsort`

- radixbits:

  size of radix in bits

- stable:

  boolean scalar defining whether stable sorting is needed. Allowing
  non-stable may speed-up.

- optimize:

  by default ramsort optimizes for 'time' which requires more RAM, set
  to 'memory' to minimize RAM requirements and sacrifice speed

- VERBOSE:

  cat some info about chosen method

## Value

These functions return the number of `NAs` found or assumed during
sorting

## Details

See [`bit::ramsort()`](https://rdrr.io/pkg/bit/man/Sorting.html)

## Note

Note that these methods purposely violate the functional programming
paradigm: they are called for the side-effect of changing some of their
arguments. The `sort`-methods change `x`, the `order`-methods change
`i`, and the `sortoder`-methods change both `x` and `i`

## See also

[`bit::ramsort()`](https://rdrr.io/pkg/bit/man/Sorting.html) for the
generic, `ramsort.default` for the methods provided by package ff,
[`sort.integer64()`](https://bit64.r-lib.org/reference/sort.integer64.md)
for the sort interface and
[`sortcache()`](https://bit64.r-lib.org/reference/hashcache.md) for
caching the work of sorting

## Examples

``` r
  x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
  x
#> integer64
#>  [1] 5    <NA> 3    <NA> <NA> <NA> 1    <NA> <NA> 4    7    <NA> 6   
#> [14] <NA> 9    8    4    2    3    2    <NA> <NA> <NA> 7    <NA> 7   
#> [27] 4    3    1    5    3    <NA>
  message("ramsort example")
#> ramsort example
  s <- bit::clone(x)
  bit::ramsort(s)
#> [1] 13
  message("s has been changed in-place - whether or not ramsort uses an in-place algorithm")
#> s has been changed in-place - whether or not ramsort uses an in-place algorithm
  s
#> integer64
#>  [1] <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>
#> [14] 1    1    2    2    3    3    3    3    4    4    4    5    5   
#> [27] 6    7    7    7    8    9   
  message("ramorder example")
#> ramorder example
  s <- bit::clone(x)
  o <- seq_along(s)
  bit::ramorder(s, o)
#> [1] 13
  message("o has been changed in-place - s remains unchanged")
#> o has been changed in-place - s remains unchanged
  s
#> integer64
#>  [1] 5    <NA> 3    <NA> <NA> <NA> 1    <NA> <NA> 4    7    <NA> 6   
#> [14] <NA> 9    8    4    2    3    2    <NA> <NA> <NA> 7    <NA> 7   
#> [27] 4    3    1    5    3    <NA>
  o
#>  [1]  2  4  5  6  8  9 12 14 21 22 23 25 32  7 29 18 20  3 19 28 31 10
#> [23] 17 27  1 30 13 11 24 26 16 15
  s[o]
#> integer64
#>  [1] <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>
#> [14] 1    1    2    2    3    3    3    3    4    4    4    5    5   
#> [27] 6    7    7    7    8    9   
  message("ramsortorder example")
#> ramsortorder example
  o <- seq_along(s)
  bit::ramsortorder(s, o)
#> [1] 13
  message("s and o have both been changed in-place - this is much faster")
#> s and o have both been changed in-place - this is much faster
  s
#> integer64
#>  [1] <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>
#> [14] 1    1    2    2    3    3    3    3    4    4    4    5    5   
#> [27] 6    7    7    7    8    9   
  o
#>  [1]  2  4  5  6  8  9 12 14 21 22 23 25 32  7 29 18 20  3 19 28 31 10
#> [23] 17 27  1 30 13 11 24 26 16 15
```
