# Set Operations

Performs set union, intersection, (asymmetric!) difference, equality and
membership on two vectors. As soon as an integer64 vector is involved,
the operations are performed using integer64 semantics. Otherwise the
`base` package functions are called.

## Usage

``` r
union(x, y)

intersect(x, y)

setequal(x, y)

setdiff(x, y)

is.element(el, set)
```

## Arguments

- x, y, el, set:

  vectors (of the same mode) containing a sequence of items
  (conceptually) with no duplicated values.

## Value

For union, a vector of a common mode or class.

For intersect, a vector of a common mode or class, or NULL if x or y is
NULL.

For setdiff, a vector of the same mode or class as x.

A logical scalar for setequal and a logical of the same length as x for
is.element.

## See also

base::union

## Examples

``` r
x <- as.integer64(1:5)
y <- c(1L, 3L, 5L, 7L)
union(x, y)
#> integer64
#> [1] 1 2 3 4 5 7
intersect(x, y)
#> integer64
#> [1] 1 3 5
setdiff(x, y)
#> integer64
#> [1] 2 4
setequal(x, y)
#> [1] FALSE
is.element(x, y)
#> [1]  TRUE FALSE  TRUE FALSE  TRUE
```
