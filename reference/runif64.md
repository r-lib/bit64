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
#>  [1] -4907153757530237863 2297215137837035766  4517639749818251378 
#>  [4] -7774236924194376415 3157361672959160553  -2252541624473150823
#>  [7] -7920764405809697242 7231932327786254689  8966949907655518413 
#> [10] 8869809578369324686  3266720816486347908  -419918909734750174 
  runif64(12, -16, 16)
#> integer64
#>  [1] -7  7   15  10  4   -10 16  -15 13  14  -3  6  
  runif64(12, 0, as.integer64(2^60)-1)  # not 2^60-1 !
#> integer64
#>  [1] 410321503976700422 952899565017148986 818491665556622412
#>  [4] 959297925008029157 743713501387121850 980510168028165491
#>  [7] 461977244155803316 42986640335880427  269355488783127835
#> [10] 692414521219393114 297078351443348005 646555823237762071
  var(runif(1e4))
#> [1] 0.08436535
  var(as.double(runif64(1e4, 0, 2^40))/2^40)  # ~ = 1/12 = .08333
#> [1] 0.08380824

  table(sample(16, replace=FALSE))
#> 
#>  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 
#>  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 
  table(runif64(16, 1, 16, replace=FALSE))
#> 
#>  3  8  9 10 11 12 13 14 16 
#>  1  1  1  1  2  2  2  3  3 
  table(sample(16, replace=TRUE))
#> 
#>  2  4  5  6  8 10 11 13 16 
#>  1  2  3  2  1  1  1  1  4 
  table(runif64(16, 1, 16, replace=TRUE))
#> 
#>  2  3  5  6  8  9 10 11 14 15 16 
#>  1  3  1  2  2  2  1  1  1  1  1 
```
