# Determine Duplicate Elements of integer64

[`duplicated()`](https://rdrr.io/r/base/duplicated.html) determines
which elements of a vector or data frame are duplicates of elements with
smaller subscripts, and returns a logical vector indicating which
elements (rows) are duplicates.

## Usage

``` r
# S3 method for class 'integer64'
duplicated(x, incomparables = FALSE, nunique = NULL, method = NULL, ...)
```

## Arguments

- x:

  a vector or a data frame or an array or `NULL`.

- incomparables:

  ignored

- nunique:

  NULL or the number of unique values (including NA). Providing
  `nunique` can speed-up matching when `x` has no cache. Note that a
  wrong `nunique` can cause undefined behaviour up to a crash.

- method:

  NULL for automatic method selection or a suitable low-level method,
  see details

- ...:

  ignored

## Value

[`duplicated()`](https://rdrr.io/r/base/duplicated.html): a logical
vector of the same length as `x`.

## Details

This function automatically chooses from several low-level functions
considering the size of `x` and the availability of a cache.

Suitable methods are

- [`hashdup`](https://bit64.r-lib.org/reference/hashmap.md) (hashing)

- [`sortorderdup`](https://bit64.r-lib.org/reference/sortnut.md) (fast
  ordering)

- [`orderdup`](https://bit64.r-lib.org/reference/sortnut.md) (memory
  saving ordering).

## See also

[`duplicated()`](https://rdrr.io/r/base/duplicated.html),
[`unique.integer64()`](https://bit64.r-lib.org/reference/unique.integer64.md)

## Examples

``` r
x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
duplicated(x)
#>  [1] FALSE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE  TRUE  TRUE
#> [12]  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE
#> [23] FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE

stopifnot(identical(duplicated(x),  duplicated(as.integer(x))))
```
