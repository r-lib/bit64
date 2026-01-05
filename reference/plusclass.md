# integer64: Maintaining S3 class attribute

Maintaining integer64 S3 class attribute.

## Usage

``` r
minusclass(class, whichclass)
```

## Arguments

- class:

  NULL or a character vector of class attributes

- whichclass:

  the (single) class name to add or remove from the class vector

## Value

NULL or a character vector of class attributes

## See also

[`oldClass()`](https://rdrr.io/r/base/class.html)
[`integer64()`](https://bit64.r-lib.org/reference/bit64-package.md)

## Examples

``` r
  plusclass("inheritingclass", "integer64")
#> [1] "inheritingclass" "integer64"      
  minusclass(c("inheritingclass", "integer64"), "integer64")
#> [1] "inheritingclass"
```
