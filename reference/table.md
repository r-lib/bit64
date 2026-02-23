# Cross Tabulation and Table Creation for integer64

`table.integer64` uses the cross-classifying integer64 vectors to build
a contingency table of the counts at each combination of vector values.

## Usage

``` r
table(
  ...,
  exclude = if (useNA == "no") c(NA, NaN),
  useNA = c("no", "ifany", "always"),
  dnn = list.names(...),
  deparse.level = 1L
)

# S3 method for class 'integer64'
table(
  ...,
  return = c("table", "data.frame", "list"),
  order = c("values", "counts"),
  nunique = NULL,
  method = NULL,
  exclude = if (useNA == "no") c(NA, NaN),
  useNA = c("no", "ifany", "always"),
  dnn = list.names(...),
  deparse.level = 1L
)
```

## Arguments

- ...:

  one or more objects which can be interpreted as factors (including
  character strings), or a list (or data frame) whose components can be
  so interpreted. (For `as.table` and `as.data.frame`, arguments passed
  to specific methods.)

- exclude:

  levels to remove for all factors in ....

- useNA:

  whether to include NA values in the table.

- dnn:

  the names to be given to the dimensions in the result (the *dimnames
  names*).

- deparse.level:

  controls how the default `dnn` is constructed. See Details.

- return:

  choose the return format, see details

- order:

  By default results are created sorted by "values", or by "counts"

- nunique:

  NULL or the number of unique values of table (including NA). Providing
  `nunique` can speed-up matching when `table` has no cache. Note that a
  wrong `nunique` can cause undefined behaviour up to a crash.

- method:

  NULL for automatic method selection or a suitable low-level method,
  see details

## Value

By default (with `return="table"`) `table()` returns a *contingency
table*, an object of class `"table"`, an array of integer values. Note
that unlike S the result is always an array, a 1D array if one factor is
given. Note also that for multidimensional arrays this is a *dense*
return structure which can dramatically increase RAM requirements (for
large arrays with high mutual information, i.e. many possible input
combinations of which only few occur) and that `table()` is limited to
`2^31` possible combinations (e.g. two input vectors with 46340 unique
values only). Finally note that the tabulated values or
value-combinations are represented as `dimnames` and that the implied
conversion of values to strings can cause *severe* performance problems
since each string needs to be integrated into R's global string cache.

You can use the other `return=` options to cope with these problems, the
potential combination limit is increased from `2^31` to `2^63` with
these options, RAM is only required for observed combinations and string
conversion is avoided.

With `return="data.frame"` you get a *dense* representation as a
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) (like that
resulting from `as.data.frame(table(...))`) where only observed
combinations are listed (each as a data.frame row) with the
corresponding frequency counts (the latter as component named by
`responseName`). This is the inverse of
[`xtabs()`](https://rdrr.io/r/stats/xtabs.html).

With `return="list"` you also get a *dense* representation as a simple
[`list()`](https://rdrr.io/r/base/list.html) with components

- `values` a integer64 vector of the technically tabulated values, for
  1D this is the tabulated values themselves, for kD these are the
  values representing the potential combinations of input values

- `counts` the frequency counts

- `dims` only for kD: a list with the vectors of the unique values of
  the input dimensions

## Details

If at least one argument of `...` is integer64 and the remaining
arguments of `...` are integer64 or integer the \`table.integer64\`
method is used. Only this method supports the arguments `return`,
`order`, `nunique`, and `method`.

This function automatically chooses from several low-level functions
considering the size of `x` and the availability of a cache.

Suitable methods are

- [`hashmaptab`](https://bit64.r-lib.org/reference/hashmap.md)
  (simultaneously creating and using a hashmap)

- [`hashtab`](https://bit64.r-lib.org/reference/hashmap.md) (first
  creating a hashmap then using it)

- [`sortordertab`](https://bit64.r-lib.org/reference/sortnut.md) (fast
  ordering)

- [`ordertab`](https://bit64.r-lib.org/reference/sortnut.md) (memory
  saving ordering).

If the argument `dnn` is not supplied, the internal function
`list.names` is called to compute the 'dimname names'. If the arguments
in `...` are named, those names are used. For the remaining arguments,
`deparse.level = 0` gives an empty name, `deparse.level = 1` uses the
supplied argument if it is a symbol, and `deparse.level = 2` will
deparse the argument.

## Note

Note that by using
[`as.integer64.factor()`](https://bit64.r-lib.org/reference/as.integer64.character.md)
we can also input factors into `table.integer64` – only the
[`levels()`](https://rdrr.io/r/base/levels.html) get lost.

## See also

`table()` for more info on the standard version coping with Base R's
data types, [`tabulate()`](https://rdrr.io/r/base/tabulate.html) which
can faster tabulate [`integer`](https://rdrr.io/r/base/integer.html)s
with a limited range `[1L .. nL not too big]`,
[`unique.integer64()`](https://bit64.r-lib.org/reference/unique.integer64.md)
for the unique values without counting them and
[`unipos.integer64()`](https://bit64.r-lib.org/reference/unipos.md) for
the positions of the unique values.

## Examples

``` r
message("pure integer64 examples")
#> pure integer64 examples
x <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
y <- as.integer64(sample(c(rep(NA, 9), 1:9), 32, TRUE))
z <- sample(c(rep(NA, 9), letters), 32, TRUE)
table(x)
#> x
#> 1 2 3 5 6 7 8 9 
#> 4 1 1 1 1 3 1 2 
table(x, order="counts")
#> x
#> 5 3 6 8 2 9 7 1 
#> 1 1 1 1 1 2 3 4 
table(x, y)
#>    y
#> x   1 2 3 4 5 6 8 9
#>   1 0 0 0 0 0 0 1 0
#>   2 0 0 0 0 0 0 1 0
#>   3 1 0 0 0 0 0 0 0
#>   5 0 0 0 0 1 0 0 0
#>   6 0 0 0 0 0 0 0 1
#>   7 0 0 0 1 0 1 0 0
#>   8 0 0 0 0 0 0 0 0
#>   9 0 0 1 0 0 0 0 1
table(x, y, return="data.frame")
#>       x    y Freq
#> 1     3    1    1
#> 2  <NA>    2    1
#> 3     9    3    1
#> 4  <NA>    3    2
#> 5     7    4    1
#> 6  <NA>    4    3
#> 7     5    5    1
#> 8  <NA>    5    1
#> 9     7    6    1
#> 10 <NA>    6    1
#> 11    1    8    1
#> 12    2    8    1
#> 13 <NA>    8    3
#> 14    6    9    1
#> 15    9    9    1
#> 16 <NA>    9    2
#> 17    1 <NA>    3
#> 18    7 <NA>    1
#> 19    8 <NA>    1
#> 20 <NA> <NA>    5

message("via as.integer64.factor we can use 'table.integer64' also for factors")
#> via as.integer64.factor we can use 'table.integer64' also for factors
table(x, as.integer64(as.factor(z)))
#>    
#> x   1 2 3 4 5 6 7 8 9 10 11 12 13
#>   1 0 1 1 0 0 0 0 0 0  0  0  0  0
#>   2 0 0 0 0 0 0 0 0 0  1  0  0  0
#>   3 0 0 0 0 0 0 0 0 0  0  0  0  0
#>   5 0 0 0 0 0 0 1 0 0  0  0  0  0
#>   6 0 0 0 0 0 1 0 0 0  0  0  0  0
#>   7 0 0 0 0 0 0 0 0 0  0  0  0  0
#>   8 0 0 0 0 0 0 0 0 0  0  0  0  0
#>   9 0 0 1 0 0 0 0 0 0  0  0  1  0
```
