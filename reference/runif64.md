# integer64: random numbers

Create uniform random 64-bit integers within defined range

## Usage

``` r
runif64(
  n,
  min = lim.integer64()[1L],
  max = lim.integer64()[2L],
  replace = TRUE
)
```

## Arguments

- n:

  length of return vector

- min:

  lower inclusive bound for random numbers

- max:

  upper inclusive bound for random numbers

- replace:

  set to FALSE for sampleing from a finite pool, see
  [`sample()`](https://rdrr.io/r/base/sample.html)

## Value

a integer64 vector

## Details

For each random integer we call R's internal C interface `unif_rand()`
twice. Each call is mapped to 2^32 unsigned integers. The two 32-bit
patterns are concatenated to form the new integer64. This process is
repeated until the result is not a `NA_INTEGER64_`.

## See also

[`runif()`](https://rdrr.io/r/stats/Uniform.html),
[`hashfun()`](https://bit64.r-lib.org/reference/hashmap.md)

## Examples

``` r
  runif64(12)
#> integer64
#>  [1] 2919667377749220241  -9217633356078084536 1863243537759179232 
#>  [4] -4147011132517925468 -4070908537411413742 4634345630669979668 
#>  [7] -666895255421834316  -1261880457717640272 -7449551699424276001
#> [10] -5258832035619644162 -9069818429303395917 -2311743981375507974
  runif64(12, -16, 16)
#> integer64
#>  [1] 7   -9  8   -3  3   -10 7   13  -10 -13 15  -7 
  runif64(12, 0, as.integer64(2^60)-1)  # not 2^60-1 !
#> integer64
#>  [1] 487708710025063356 422511654302077704 383311022619956180
#>  [4] 284967664771235424 537111568398072312 51120190511248319 
#>  [7] 392543978056304657 790898744552531979 697687579755620398
#> [10] 878934225106669508 226876692854159875 606102669791587352
  var(runif(1e4))
#> [1] 0.08336018
  var(as.double(runif64(1e4, 0, 2^40))/2^40)  # ~ = 1/12 = .08333
#> [1] 0.08290255

  table(sample(16, replace=FALSE))
#> 
#>  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 
#>  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 
  table(runif64(16, 1, 16, replace=FALSE))
#> 
#>  2  4  5  6  7  8 10 11 14 16 
#>  2  1  2  1  2  1  2  1  1  3 
  table(sample(16, replace=TRUE))
#> 
#>  1  3  4  5  6  7  8  9 10 12 13 14 15 16 
#>  1  1  1  1  1  1  1  1  2  1  1  1  2  1 
  table(runif64(16, 1, 16, replace=TRUE))
#> 
#>  1  2  3  5  6  7  8 11 13 15 
#>  2  1  2  1  2  1  1  1  3  2 
```
