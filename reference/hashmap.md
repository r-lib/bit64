# Hashing for 64bit integers

This is an explicit implementation of hash functionality that underlies
matching and other functions in R. Explicit means that you can create,
store and use hash functionality directly. One advantage is that you can
re-use hashmaps, which avoid re-building hashmaps again and again.

## Usage

``` r
hashfun(x, ...)

# S3 method for class 'integer64'
hashfun(x, minfac = 1.41, hashbits = NULL, ...)

hashmap(x, ...)

# S3 method for class 'integer64'
hashmap(x, nunique = NULL, minfac = 1.41, hashbits = NULL, cache = NULL, ...)

hashpos(cache, ...)

# S3 method for class 'cache_integer64'
hashpos(cache, x, nomatch = NA_integer_, ...)

hashrev(cache, ...)

# S3 method for class 'cache_integer64'
hashrev(cache, x, nomatch = NA_integer_, ...)

hashfin(cache, ...)

# S3 method for class 'cache_integer64'
hashfin(cache, x, ...)

hashrin(cache, ...)

# S3 method for class 'cache_integer64'
hashrin(cache, x, ...)

hashdup(cache, ...)

# S3 method for class 'cache_integer64'
hashdup(cache, ...)

hashuni(cache, ...)

# S3 method for class 'cache_integer64'
hashuni(cache, keep.order = FALSE, ...)

hashupo(cache, ...)

# S3 method for class 'cache_integer64'
hashupo(cache, keep.order = FALSE, ...)

hashtab(cache, ...)

# S3 method for class 'cache_integer64'
hashtab(cache, ...)

hashmaptab(x, ...)

# S3 method for class 'integer64'
hashmaptab(x, nunique = NULL, minfac = 1.5, hashbits = NULL, ...)

hashmapuni(x, ...)

# S3 method for class 'integer64'
hashmapuni(x, nunique = NULL, minfac = 1.5, hashbits = NULL, ...)

hashmapupo(x, ...)

# S3 method for class 'integer64'
hashmapupo(x, nunique = NULL, minfac = 1.5, hashbits = NULL, ...)
```

## Arguments

- x:

  an integer64 vector

- ...:

  further arguments, passed from generics, ignored in methods

- minfac:

  minimum factor by which the hasmap has more elements compared to the
  data `x`, ignored if `hashbits` is given directly

- hashbits:

  length of hashmap is `2^hashbits`

- nunique:

  giving *correct* number of unique elements can help reducing the size
  of the hashmap

- cache:

  an optional [`cache()`](https://bit64.r-lib.org/reference/cache.md)
  object into which to put the hashmap (by default a new cache is
  created

- nomatch:

  the value to be returned if an element is not found in the hashmap

- keep.order:

  determines order of results and speed: `FALSE` (the default) is faster
  and returns in the (pseudo)random order of the hash function, `TRUE`
  returns in the order of first appearance in the original data, but
  this requires extra work

## Value

See Details

## Details

|              |                                                                             |                                                                         |
|--------------|-----------------------------------------------------------------------------|-------------------------------------------------------------------------|
| **function** | **see also**                                                                | **description**                                                         |
| `hashfun`    | `digest`                                                                    | export of the hash function used in `hashmap`                           |
| `hashmap`    | [`match()`](https://bit64.r-lib.org/reference/match.integer64.md)           | return hashmap                                                          |
| `hashpos`    | [`match()`](https://bit64.r-lib.org/reference/match.integer64.md)           | return positions of `x` in `hashmap`                                    |
| `hashrev`    | [`match()`](https://bit64.r-lib.org/reference/match.integer64.md)           | return positions of `hashmap` in `x`                                    |
| `hashfin`    | [`%in%.integer64`](https://bit64.r-lib.org/reference/match.integer64.md)    | return logical whether `x` is in `hashmap`                              |
| `hashrin`    | [`%in%.integer64`](https://bit64.r-lib.org/reference/match.integer64.md)    | return logical whether `hashmap` is in `x`                              |
| `hashdup`    | [`duplicated()`](https://bit64.r-lib.org/reference/duplicated.integer64.md) | return logical whether hashdat is duplicated using hashmap              |
| `hashuni`    | [`unique()`](https://bit64.r-lib.org/reference/unique.integer64.md)         | return unique values of hashmap                                         |
| `hashmapuni` | [`unique()`](https://bit64.r-lib.org/reference/unique.integer64.md)         | return unique values of `x`                                             |
| `hashupo`    | [`unique()`](https://bit64.r-lib.org/reference/unique.integer64.md)         | return positions of unique values in hashdat                            |
| `hashmapupo` | [`unique()`](https://bit64.r-lib.org/reference/unique.integer64.md)         | return positions of unique values in `x`                                |
| `hashtab`    | [`table()`](https://bit64.r-lib.org/reference/table.md)                     | tabulate values of hashdat using hashmap in `keep.order=FALSE`          |
| `hashmaptab` | [`table()`](https://bit64.r-lib.org/reference/table.md)                     | tabulate values of `x` building hasmap on the fly in `keep.order=FALSE` |

## See also

[`match()`](https://bit64.r-lib.org/reference/match.integer64.md),
[`runif64()`](https://bit64.r-lib.org/reference/runif64.md)

## Examples

``` r
x <- as.integer64(sample(c(NA, 0:9)))
y <- as.integer64(sample(c(NA, 1:9), 10, TRUE))
hashfun(y)
#>  [1]  3  8  7  5 11  9  9 11  9  9
hx <- hashmap(x)
hy <- hashmap(y)
ls(hy)
#> [1] "hashbits" "hashmap"  "nhash"    "nunique"  "x"       
hashpos(hy, x)
#>  [1] NA  6  2 NA NA  4  3 NA  5 NA  1
hashrev(hx, y)
#>  [1] NA  6  2 NA NA  4  3 NA  5 NA  1
hashfin(hy, x)
#>  [1] FALSE  TRUE  TRUE FALSE FALSE  TRUE  TRUE FALSE  TRUE FALSE  TRUE
hashrin(hx, y)
#>  [1] FALSE  TRUE  TRUE FALSE FALSE  TRUE  TRUE FALSE  TRUE FALSE  TRUE
hashdup(hy)
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
hashuni(hy)
#> integer64
#> [1] 2    7    4    <NA> 1    6   
hashuni(hy, keep.order=TRUE)
#> integer64
#> [1] 2    <NA> 4    7    6    1   
hashmapuni(y)
#> integer64
#> [1] 2    <NA> 4    7    6    1   
hashupo(hy)
#> [1] 1 4 3 2 6 5
hashupo(hy, keep.order=TRUE)
#> [1] 1 2 3 4 5 6
hashmapupo(y)
#> [1] 1 2 3 4 5 6
hashtab(hy)
#> $values
#> integer64
#> [1] 2    7    4    <NA> 1    6   
#> 
#> $counts
#> [1] 1 1 1 1 4 2
#> 
hashmaptab(y)
#> $values
#> integer64
#> [1] 2    7    4    <NA> 1    6   
#> 
#> $counts
#> [1] 1 1 1 1 4 2
#> 

stopifnot(identical(match(as.integer(x), as.integer(y)), hashpos(hy, x)))
stopifnot(identical(match(as.integer(x), as.integer(y)), hashrev(hx, y)))
stopifnot(identical(as.integer(x) %in% as.integer(y), hashfin(hy, x)))
stopifnot(identical(as.integer(x) %in% as.integer(y), hashrin(hx, y)))
stopifnot(identical(duplicated(as.integer(y)), hashdup(hy)))
stopifnot(identical(as.integer64(unique(as.integer(y))), hashuni(hy, keep.order=TRUE)))
stopifnot(identical(sort(hashuni(hy, keep.order=FALSE)), sort(hashuni(hy, keep.order=TRUE))))
stopifnot(identical(y[hashupo(hy, keep.order=FALSE)], hashuni(hy, keep.order=FALSE)))
stopifnot(identical(y[hashupo(hy, keep.order=TRUE)], hashuni(hy, keep.order=TRUE)))
stopifnot(identical(hashpos(hy, hashuni(hy, keep.order=TRUE)), hashupo(hy, keep.order=TRUE)))
stopifnot(identical(hashpos(hy, hashuni(hy, keep.order=FALSE)), hashupo(hy, keep.order=FALSE)))
stopifnot(identical(hashuni(hy, keep.order=FALSE), hashtab(hy)$values))
stopifnot(identical(as.vector(table(as.integer(y), useNA="ifany"))
, hashtab(hy)$counts[order.integer64(hashtab(hy)$values)]))
#> Warning: Detected that 'order.integer64' was called directly. Instead only call 'order' and rely on S3 dispatch. To suppress this warning, e.g. if this is a false positive, use options(bit64.warn.exported.s3.method = FALSE). In the next version, this symbol will stop being exported.
stopifnot(identical(hashuni(hy, keep.order=TRUE), hashmapuni(y)))
stopifnot(identical(hashupo(hy, keep.order=TRUE), hashmapupo(y)))
stopifnot(identical(hashtab(hy), hashmaptab(y)))

    if (FALSE) { # \dontrun{
    message("explore speed given size of the hasmap in 2^hashbits and size of the data")
    message("more hashbits means more random access and less collisions")
    message("i.e. more data means less random access and more collisions")
    bits <- 24
    b <- seq(-1, 0, 0.1)
    tim <- matrix(NA, length(b), 2, dimnames=list(b, c("bits", "bits+1")))
    for (i in 1:length(b)) {
      n <- as.integer(2^(bits+b[i]))
      x <- as.integer64(sample(n))
      tim[i, 1] <- repeat.time(hashmap(x, hashbits=bits))[3]
      tim[i, 2] <- repeat.time(hashmap(x, hashbits=bits+1))[3]
      print(tim)
      matplot(b, tim)
    }
    message("we conclude that n*sqrt(2) is enough to avoid collisions")
    } # }
```
