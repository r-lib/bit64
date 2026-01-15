# Extract Unique Elements from integer64

`unique` returns a vector like `x` but with duplicate elements/rows
removed.

## Usage

``` r
# S3 method for class 'integer64'
unique(
  x,
  incomparables = FALSE,
  order = c("original", "values", "any"),
  nunique = NULL,
  method = NULL,
  ...
)
```

## Arguments

- x:

  a vector or a data frame or an array or `NULL`.

- incomparables:

  ignored

- order:

  The order in which unique values will be returned, see details

- nunique:

  NULL or the number of unique values (including NA). Providing
  `nunique` can speed-up matching when `x` has no cache. Note that a
  wrong \`nunique“ can cause undefined behaviour up to a crash.

- method:

  NULL for automatic method selection or a suitable low-level method,
  see details

- ...:

  ignored

## Value

For a vector, an object of the same type of `x`, but with only one copy
of each duplicated element. No attributes are copied (so the result has
no names).

## Details

This function automatically chooses from several low-level functions
considering the size of `x` and the availability of a cache.

Suitable methods are

- [`hashmapuni`](https://bit64.r-lib.org/reference/hashmap.md)
  (simultaneously creating and using a hashmap)

- [`hashuni`](https://bit64.r-lib.org/reference/hashmap.md) (first
  creating a hashmap then using it)

- [`sortuni`](https://bit64.r-lib.org/reference/sortnut.md) (fast
  sorting for sorted order only)

- [`sortorderuni`](https://bit64.r-lib.org/reference/sortnut.md) (fast
  ordering for original order only)

- [`orderuni`](https://bit64.r-lib.org/reference/sortnut.md) (memory
  saving ordering).

The default `order="original"` returns unique values in the order of the
first appearance in `x` like in
[`unique()`](https://rdrr.io/r/base/unique.html), this costs extra
processing. `order="values"` returns unique values in sorted order like
in [`table()`](https://bit64.r-lib.org/reference/table.md), this costs
extra processing with the hash methods but comes for free. `order="any"`
returns unique values in undefined order, possibly faster. For hash
methods this will be a quasi random order, for sort methods this will be
sorted order.

## See also

[`unique()`](https://rdrr.io/r/base/unique.html) for the generic,
[`unipos()`](https://bit64.r-lib.org/reference/unipos.md) which gives
the indices of the unique elements and
[`table()`](https://bit64.r-lib.org/reference/table.md) which gives
frequencies of the unique elements.

## Examples

``` r
x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
unique(x)
#> integer64
#>  [1] 9    <NA> 6    5    7    3    1    2    8    4   
unique(x, order="values")
#> integer64
#>  [1] <NA> 1    2    3    4    5    6    7    8    9   

stopifnot(identical(unique(x),  x[!duplicated(x)]))
stopifnot(identical(unique(x),  as.integer64(unique(as.integer(x)))))
stopifnot(identical(unique(x, order="values")
,  as.integer64(sort(unique(as.integer(x)), na.last=FALSE))))
```
