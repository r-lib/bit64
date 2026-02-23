# Extract Positions of Unique Elements

`unipos` returns the positions of those elements returned by
[`unique()`](https://rdrr.io/r/base/unique.html).

## Usage

``` r
unipos(x, incomparables = FALSE, order = c("original", "values", "any"), ...)

# S3 method for class 'integer64'
unipos(
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

  The order in which positions of unique values will be returned, see
  details

- ...:

  ignored

- nunique:

  NULL or the number of unique values (including NA). Providing
  `nunique` can speed-up when `x` has no cache. Note that a wrong
  `nunique` can cause undefined behaviour up to a crash.

- method:

  NULL for automatic method selection or a suitable low-level method,
  see details

## Value

an integer vector of positions

## Details

This function automatically chooses from several low-level functions
considering the size of `x` and the availability of a cache.

Suitable methods are

- [`hashmapupo`](https://bit64.r-lib.org/reference/hashmap.md)
  (simultaneously creating and using a hashmap)

- [`hashupo`](https://bit64.r-lib.org/reference/hashmap.md) (first
  creating a hashmap then using it)

- [`sortorderupo`](https://bit64.r-lib.org/reference/sortnut.md) (fast
  ordering)

- [`orderupo`](https://bit64.r-lib.org/reference/sortnut.md) (memory
  saving ordering).

The default `order="original"` collects unique values in the order of
the first appearance in `x` like in
[`unique()`](https://rdrr.io/r/base/unique.html), this costs extra
processing. `order="values"` collects unique values in sorted order like
in [`table()`](https://bit64.r-lib.org/reference/table.md), this costs
extra processing with the hash methods but comes for free. `order="any"`
collects unique values in undefined order, possibly faster. For hash
methods this will be a quasi random order, for sort methods this will be
sorted order.

## See also

[`unique.integer64()`](https://bit64.r-lib.org/reference/unique.integer64.md)
for unique values and
[`match.integer64()`](https://bit64.r-lib.org/reference/match.integer64.md)
for general matching.

## Examples

``` r
x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
unipos(x)
#> [1]  1  3  5  6  7 20 22 30
unipos(x, order="values")
#> [1]  1  3  5 22 30  6 20  7

stopifnot(identical(unipos(x),  (1:length(x))[!duplicated(x)]))
stopifnot(identical(unipos(x),  match.integer64(unique(x), x)))
#> Warning: Detected that 'match.integer64' was called directly. Instead only call 'match' and rely on S3 dispatch. To suppress this warning, e.g. if this is a false positive, use options(bit64.warn.exported.s3.method = FALSE). In the next version, this symbol will stop being exported.
stopifnot(identical(unipos(x, order="values"),  match.integer64(unique(x, order="values"), x)))
#> Warning: Detected that 'match.integer64' was called directly. Instead only call 'match' and rely on S3 dispatch. To suppress this warning, e.g. if this is a false positive, use options(bit64.warn.exported.s3.method = FALSE). In the next version, this symbol will stop being exported.
stopifnot(identical(unique(x),  x[unipos(x)]))
stopifnot(identical(unique(x, order="values"),  x[unipos(x, order="values")]))
```
