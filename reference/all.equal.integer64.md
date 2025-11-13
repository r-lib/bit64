# Test if two integer64 vectors are all.equal

A utility to compare integer64 objects 'x' and 'y' testing for ‘near
equality’, see [`all.equal()`](https://rdrr.io/r/base/all.equal.html).

## Usage

``` r
# S3 method for class 'integer64'
all.equal(
  target,
  current,
  tolerance = sqrt(.Machine$double.eps),
  scale = NULL,
  countEQ = FALSE,
  formatFUN = function(err, what) format(err),
  ...,
  check.attributes = TRUE
)
```

## Arguments

- target:

  a vector of 'integer64' or an object that can be coerced with
  [`as.integer64()`](https://bit64.r-lib.org/reference/as.integer64.character.md)

- current:

  a vector of 'integer64' or an object that can be coerced with
  [`as.integer64()`](https://bit64.r-lib.org/reference/as.integer64.character.md)

- tolerance:

  numeric \> 0. Differences smaller than `tolerance` are not reported.
  The default value is close to `1.5e-8`.

- scale:

  `NULL` or numeric \> 0, typically of length 1 or `length(target)`. See
  Details.

- countEQ:

  logical indicating if the `target == current` cases should be counted
  when computing the mean (absolute or relative) differences. The
  default, `FALSE` may seem misleading in cases where `target` and
  `current` only differ in a few places; see the extensive example.

- formatFUN:

  a [`function()`](https://rdrr.io/r/base/function.html) of two
  arguments, `err`, the relative, absolute or scaled error, and `what`,
  a character string indicating the *kind* of error; maybe used, e.g.,
  to format relative and absolute errors differently.

- ...:

  further arguments are ignored

- check.attributes:

  logical indicating if the
  [`attributes()`](https://rdrr.io/r/base/attributes.html) of `target`
  and `current` (other than the names) should be compared.

## Value

Either ‘TRUE’ (‘NULL’ for ‘attr.all.equal’) or a vector of ‘mode’
‘"character"’ describing the differences between ‘target’ and ‘current’.

## Details

In [`all.equal.numeric()`](https://rdrr.io/r/base/all.equal.html) the
type `integer` is treated as a proper subset of `double` i.e. does not
complain about comparing `integer` with `double`. Following this logic
`all.equal.integer64` treats `integer` as a proper subset of `integer64`
and does not complain about comparing `integer` with `integer64`.
`double` also compares without warning as long as the values are within
[`lim.integer64()`](https://bit64.r-lib.org/reference/sum.integer64.md),
if `double` are bigger `all.equal.integer64` complains about the
`all.equal.integer64 overflow warning`. For further details see
[`all.equal()`](https://rdrr.io/r/base/all.equal.html).

## Note

[`all.equal()`](https://rdrr.io/r/base/all.equal.html) only dispatches
to this method if the first argument is `integer64`, calling
[`all.equal()`](https://rdrr.io/r/base/all.equal.html) with a
`non-integer64` first and a `integer64` second argument gives undefined
behavior!

## See also

[`all.equal()`](https://rdrr.io/r/base/all.equal.html)

## Examples

``` r
  all.equal(as.integer64(1:10), as.integer64(0:9))
#> [1] "Mean relative difference: 0.1818182"
  all.equal(as.integer64(1:10), as.integer(1:10))
#> [1] TRUE
  all.equal(as.integer64(1:10), as.double(1:10))
#> [1] TRUE
  all.equal(as.integer64(1), as.double(1e300))
#> Error in as.integer64.double(current) : 
#>   (converted from warning) NAs produced by integer64 overflow
#> [1] "while coercing 'current' to 'integer64': (converted from warning) NAs produced by integer64 overflow"
```
