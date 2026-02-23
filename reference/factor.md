# Factors

The function factor is used to encode a vector as a factor.

## Usage

``` r
factor(
  x = character(),
  levels,
  labels = levels,
  exclude = NA,
  ordered = is.ordered(x),
  nmax = NA
)

ordered(x = character(), ...)
```

## Arguments

- x:

  a vector of data, usually taking a small number of distinct values.

- levels:

  an optional vector of the unique values (as character strings) that
  `x` might have taken. The default is the unique set of values taken by
  [`as.character`](https://rdrr.io/r/base/character.html)`(x)`, sorted
  into increasing order *of `x`*. Note that this set can be specified as
  smaller than `sort(unique(x))`.

- labels:

  *either* an optional character vector of labels for the levels (in the
  same order as `levels` after removing those in `exclude`), *or* a
  character string of length 1. Duplicated values in `labels` can be
  used to map different values of `x` to the same factor level.

- exclude:

  a vector of values to be excluded when forming the set of levels. This
  may be factor with the same level set as `x` or should be a
  `character`.

- ordered:

  logical flag to determine if the levels should be regarded as ordered
  (in the order given).

- nmax:

  an upper bound on the number of levels.

- ...:

  (in `ordered(.)`): any of the above, apart from `ordered` itself.

## Value

An object of class "factor" or "ordered".

## See also

[factor](https://rdrr.io/r/base/factor.html)

## Examples

``` r
  x <- as.integer64(c(132724613L, -2143220989L, -1L, NA, 1L))
  factor(x)
#> [1] 132724613   -2143220989 -1          <NA>        1          
#> Levels: -2143220989 -1 1 132724613
  ordered(x)
#> [1] 132724613   -2143220989 -1          <NA>        1          
#> Levels: -2143220989 < -1 < 1 < 132724613
```
