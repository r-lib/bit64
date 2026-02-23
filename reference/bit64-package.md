# A S3 class for vectors of 64bit integers

Package 'bit64' provides fast serializable S3 atomic 64bit (signed)
integers that can be used in vectors, matrices, arrays and data.frames.
Methods are available for coercion from and to logicals, integers,
doubles, characters and factors as well as many elementwise and summary
functions.

### Version 0.8

With 'integer64' vectors you can store very large integers at the
expense of 64 bits, which is by factor 7 better than 'int64' from
package 'int64'. Due to the smaller memory footprint, the atomic vector
architecture and using only S3 instead of S4 classes, most operations
are one to three orders of magnitude faster: Example speedups are 4x for
serialization, 250x for adding, 900x for coercion and 2000x for object
creation. Also 'integer64' avoids an ongoing (potentially infinite)
penalty for garbage collection observed during existence of 'int64'
objects (see code in example section).

### Version 0.9

Package 'bit64' - which extends R with fast 64-bit integers - now has
fast (single-threaded) implementations the most important univariate
algorithmic operations (those based on hashing and sorting). We now have
methods for 'match', '%in%', 'duplicated', 'unique', 'table', 'sort',
'order', 'rank', 'quantile', 'median' and 'summary'. Regarding data
management we also have novel generics 'unipos' (positions of the unique
values), 'tiepos' ( positions of ties), 'keypos' (positions of foreign
keys in a sorted dimension table) and derived methods 'as.factor' and
'as.ordered'. This 64- bit functionality is implemented carefully to be
not slower than the respective 32-bit operations in Base R and also to
avoid outlying waiting times observed with 'order', 'rank' and 'table'
(speedup factors 20/16/200 respective). This increases the dataset size
with wich we can work truly interactive. The speed is achieved by simple
heuristic optimizers in high- level functions choosing the best from
multiple low-level algorithms and further taking advantage of a novel
caching if activated. In an example R session using a couple of these
operations the 64-bit integers performed 22x faster than base 32-bit
integers, hash-caching improved this to 24x, sortorder-caching was most
efficient with 38x (caching hashing and sorting is not worth it with 32x
at duplicated RAM consumption).

## Usage

``` r
integer64(length = 0L)

is.integer64(x)

# S3 method for class 'integer64'
length(x) <- value

# S3 method for class 'integer64'
print(x, quote = FALSE, ...)

# S3 method for class 'integer64'
str(
  object,
  vec.len = strO$vec.len,
  give.head = TRUE,
  give.length = give.head,
  ...
)
```

## Arguments

- length:

  length of vector using
  [`integer()`](https://rdrr.io/r/base/integer.html)

- x:

  an integer64 vector

- value:

  an integer64 vector of values to be assigned

- quote:

  logical, indicating whether or not strings should be printed with
  surrounding quotes.

- ...:

  further arguments to the
  [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html)

- object:

  an integer64 vector

- vec.len, give.head, give.length:

  see [`utils::str()`](https://rdrr.io/r/utils/str.html)

## Value

`integer64` returns a vector of 'integer64', i.e., a vector of
[`double()`](https://rdrr.io/r/base/double.html) decorated with class
'integer64'.

## Note

`integer64` are useful for handling database keys and exact counting in
+-2^63. Do not use them as replacement for 32bit integers, integer64 are
not supported for subscripting by R-core and they have different
semantics when combined with double. Do understand that `integer64` can
only be useful over `double` if we do not coerce it to `double`.

While

integer + double -\> double + double -\> double

or

1L + 0.5 -\> 1.5

for additive operations we coerce to `integer64`

integer64 + double -\> integer64 + integer64 -\> integer64

hence

as.integer64(1) + 0.5 -\> 1LL + 0LL -\> 1LL

see section "Arithmetic precision and coercion" above

## Design considerations

64 bit integers are related to big data: we need them to overcome
address space limitations. Therefore performance of the 64 bit integer
type is critical. In the S language – designed in 1975 – atomic objects
were defined to be vectors for a couple of good reasons: simplicity,
option for implicit parallelization, good cache locality. In recent
years many analytical databases have learnt that lesson: column based
data bases provide superior performance for many applications, the
result are products such as MonetDB, Sybase IQ, Vertica, Exasol, Ingres
Vectorwise. If we introduce 64 bit integers not natively in Base R but
as an external package, we should at least strive to make them as
'basic' as possible. Therefore the design choice of bit64 not only
differs from package int64, it is obvious: Like the other atomic types
in Base R, we model data type 'integer64' as a contiguous
[`atomic`](https://rdrr.io/r/base/vector.html) vector in memory, and we
use the more basic S3 class system, not
[S4](https://rdrr.io/r/base/isS4.html). Like package int64 we want our
'integer64' to be
[`serialize`](https://rdrr.io/r/base/serialize.html)able, therefore we
also use an existing data type as the basis. Again the choice is
obvious: R has only one 64 bit data type: doubles. By using
[`double`](https://rdrr.io/r/base/double.html)s, `integer64`
[`inherits`](https://rdrr.io/r/base/class.html) some functionality such
as [`is.atomic()`](https://rdrr.io/r/base/is.recursive.html),
[`length()`](https://rdrr.io/r/base/length.html), `length<-`,
[`names()`](https://rdrr.io/r/base/names.html), `names<-`,
[`dim()`](https://rdrr.io/r/base/dim.html), `dim<-`,
[`dimnames()`](https://rdrr.io/r/base/dimnames.html), `dimnames<-`.

Our R level functions strictly follow the functional programming
paradigm: no modification of arguments or other side-effects. Before
version 0.93 we internally deviated from the strict paradigm in order to
boost performance. Our C functions do not create new return values,
instead we pass-in the memory to be returned as an argument. This gives
us the freedom to apply the C-function to new or old vectors, which
helps to avoid unnecessary memory allocation, unnecessary copying and
unnecessary garbage collection. Prior to 0.93 *within* our R functions
we also deviated from conventional R programming by not using `attr<-`
and `attributes<-` because they always did new memory allocation and
copying in older R versions. If we wanted to set attributes of return
values that we have freshly created, we instead used functions
[`bit::setattr()`](https://rdrr.io/pkg/bit/man/getsetattr.html) and
[`bit::setattributes()`](https://rdrr.io/pkg/bit/man/getsetattr.html).
From version 0.93
[`bit::setattr()`](https://rdrr.io/pkg/bit/man/getsetattr.html) is only
used for manipulating
[`cache`](https://bit64.r-lib.org/reference/cache.md) objects, in
[`ramsort.integer64()`](https://bit64.r-lib.org/reference/ramsort.integer64.md),
[`sort.integer64()`](https://bit64.r-lib.org/reference/sort.integer64.md),
and
[`as.data.frame.integer64()`](https://bit64.r-lib.org/reference/as.data.frame.integer64.md).

## Arithmetic precision and coercion

The fact that we introduce 64 bit long long integers – without
introducing 128-bit long doubles – creates some subtle challenges:
Unlike 32 bit [`integer`](https://rdrr.io/r/base/integer.html)s, the
`integer64` are no longer a proper subset of
[`double`](https://rdrr.io/r/base/double.html). If a binary arithmetic
operation does involve a `double` and a `integer`, it is a no-brainer to
return `double` without loss of information. If an `integer64` meets a
`double`, it is not trivial what type to return. Switching to
`integer64` limits our ability to represent very large numbers,
switching to `double` limits our ability to distinguish `x` from `x+1`.
Since the latter is the purpose of introducing 64 bit integers, we
usually return `integer64` from functions involving `integer64`, for
example in [`c()`](https://bit64.r-lib.org/reference/c.integer64.md),
[`cbind()`](https://bit64.r-lib.org/reference/c.integer64.md), and
[`rbind()`](https://bit64.r-lib.org/reference/c.integer64.md)

Different from Base R, our operators
[`+`](https://bit64.r-lib.org/reference/xor.integer64.md),
[`-`](https://bit64.r-lib.org/reference/xor.integer64.md),
[`%/%`](https://bit64.r-lib.org/reference/xor.integer64.md), and
[`%%`](https://bit64.r-lib.org/reference/xor.integer64.md) coerce their
arguments to `integer64` and always return `integer64`.

The multiplication operator
[`*`](https://bit64.r-lib.org/reference/xor.integer64.md) coerces its
first argument to `integer64` but allows its second argument to be also
`double`: the second argument is internaly coerced to 'long double' and
the result of the multiplication is returned as `integer64`.

The division [`/`](https://bit64.r-lib.org/reference/xor.integer64.md)
and power [`^`](https://bit64.r-lib.org/reference/xor.integer64.md)
operators also coerce their first argument to `integer64` and coerce
internally their second argument to 'long double', they return as
`double`, like
[`sqrt()`](https://bit64.r-lib.org/reference/format.integer64.md),
[`log()`](https://bit64.r-lib.org/reference/format.integer64.md),
[`log2()`](https://bit64.r-lib.org/reference/format.integer64.md), and
[`log10()`](https://bit64.r-lib.org/reference/format.integer64.md) do.

|               |        |               |         |              |        |              |         |            |
|---------------|--------|---------------|---------|--------------|--------|--------------|---------|------------|
| **argument1** | **op** | **argument2** | **-\>** | **coerced1** | **op** | **coerced2** | **-\>** | **result** |
| integer64     | \+     | double        | -\>     | integer64    | \+     | integer64    | -\>     | integer64  |
| double        | \+     | integer64     | -\>     | integer64    | \+     | integer64    | -\>     | integer64  |
| integer64     | \-     | double        | -\>     | integer64    | \-     | integer64    | -\>     | integer64  |
| double        | \-     | integer64     | -\>     | integer64    | \-     | integer64    | -\>     | integer64  |
| integer64     | %/%    | double        | -\>     | integer64    | %/%    | integer64    | -\>     | integer64  |
| double        | %/%    | integer64     | -\>     | integer64    | %/%    | integer64    | -\>     | integer64  |
| integer64     | %%     | double        | -\>     | integer64    | %%     | integer64    | -\>     | integer64  |
| double        | %%     | integer64     | -\>     | integer64    | %%     | integer64    | -\>     | integer64  |
| integer64     | \*     | double        | -\>     | integer64    | \*     | long double  | -\>     | integer64  |
| double        | \*     | integer64     | -\>     | integer64    | \*     | integer64    | -\>     | integer64  |
| integer64     | /      | double        | -\>     | integer64    | /      | long double  | -\>     | double     |
| double        | /      | integer64     | -\>     | integer64    | /      | long double  | -\>     | double     |
| integer64     | ^      | double        | -\>     | integer64    | /      | long double  | -\>     | double     |
| double        | ^      | integer64     | -\>     | integer64    | /      | long double  | -\>     | double     |
| integer64     | %\*%   | double        | -\>     | integer64    | %\*%   | integer64    | -\>     | integer64  |
| double        | %\*%   | integer64     | -\>     | integer64    | %\*%   | integer64    | -\>     | integer64  |
| integer64     | %\*%   | complex       | -\>     | double       | %\*%   | complex      | -\>     | complex    |
| complex       | %\*%   | integer64     | -\>     | complex      | %\*%   | double       | -\>     | complex    |

## Creating and testing S3 class 'integer64'

Our creator function `integer64` takes an argument `length`, creates an
atomic double vector of this length, attaches an S3 class attribute
'integer64' to it, and that's it. We simply rely on S3 method dispatch
and interpret those 64-bit elements as 'long long int'.

[`is.double()`](https://bit64.r-lib.org/reference/bit64S3.md) currently
returns TRUE for `integer64` and might return `FALSE` in a later
release. Consider
[`is.double()`](https://bit64.r-lib.org/reference/bit64S3.md) to have
undefined behavior and do query `is.integer64()` *before* querying
[`is.double()`](https://bit64.r-lib.org/reference/bit64S3.md).

The methods `is.integer64()` and
[`is.vector()`](https://rdrr.io/r/base/vector.html) both return `TRUE`
for `integer64`. Note that we did not patch
[`storage.mode()`](https://rdrr.io/r/base/mode.html) and
[`typeof()`](https://rdrr.io/r/base/typeof.html), which both continue
returning 'double'. Like for 32 bit
[`integer`](https://rdrr.io/r/base/integer.html),
[`mode()`](https://rdrr.io/r/base/mode.html) returns 'numeric' and
[`as.double()`](https://rdrr.io/r/base/double.html) tries coercing to
[`double`](https://rdrr.io/r/base/double.html). It is possible that
'integer64' becomes a `vmode` in package ff.

Further methods for creating `integer64` are
[`range()`](https://bit64.r-lib.org/reference/sum.integer64.md) which
returns the range of the data type if calles without arguments,
[`rep()`](https://bit64.r-lib.org/reference/rep.integer64.md),
[`seq()`](https://bit64.r-lib.org/reference/seq.integer64.md).

For all available methods on `integer64` vectors see the index below and
the examples.

## Index of implemented methods

|                                                                                     |                                                        |                                                        |
|-------------------------------------------------------------------------------------|--------------------------------------------------------|--------------------------------------------------------|
| **creating, testing, printing**                                                     | **see also**                                           | **description**                                        |
| `NA_integer64_`                                                                     | [`NA_integer_`](https://rdrr.io/r/base/NA.html)        | NA constant                                            |
| `integer64`                                                                         | [`integer`](https://rdrr.io/r/base/integer.html)       | create zero atomic vector                              |
| [`runif64()`](https://bit64.r-lib.org/reference/runif64.md)                         | [`runif()`](https://rdrr.io/r/stats/Uniform.html)      | create random vector                                   |
| [`rep.integer64()`](https://bit64.r-lib.org/reference/rep.integer64.md)             | [`rep()`](https://rdrr.io/r/base/rep.html)             |                                                        |
| [`seq.integer64()`](https://bit64.r-lib.org/reference/seq.integer64.md)             | [`seq()`](https://rdrr.io/r/base/seq.html)             |                                                        |
| `is.integer64()`                                                                    | `is()`                                                 |                                                        |
|                                                                                     | [`is.integer()`](https://rdrr.io/r/base/integer.html)  | inherited from Base R                                  |
| `is.vector.integer64()`                                                             | [`is.vector()`](https://rdrr.io/r/base/vector.html)    |                                                        |
| [`identical.integer64()`](https://bit64.r-lib.org/reference/identical.integer64.md) | [`identical()`](https://rdrr.io/r/base/identical.html) |                                                        |
| `length<-.integer64`                                                                | `length<-`                                             |                                                        |
|                                                                                     | [`length()`](https://rdrr.io/r/base/length.html)       | inherited from Base R                                  |
| `names<-`                                                                           | inherited from Base R                                  |                                                        |
|                                                                                     | [`names()`](https://rdrr.io/r/base/names.html)         | inherited from Base R                                  |
|                                                                                     | `dim<-`                                                | inherited from Base R                                  |
|                                                                                     | [`dim()`](https://rdrr.io/r/base/dim.html)             | inherited from Base R                                  |
|                                                                                     | `dimnames<-`                                           | inherited from Base R                                  |
|                                                                                     | [`dimnames()`](https://rdrr.io/r/base/dimnames.html)   | inherited from Base R                                  |
|                                                                                     | [`str()`](https://rdrr.io/r/utils/str.html)            | inherited from Base R, does not print values correctly |
| `print.integer64()`                                                                 | [`print()`](https://rdrr.io/r/base/print.html)         |                                                        |
| `str.integer64()`                                                                   | [`str()`](https://rdrr.io/r/utils/str.html)            |                                                        |

|                                                                                           |                                                                                 |                 |
|-------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------|-----------------|
| **coercing to integer64**                                                                 | **see also**                                                                    | **description** |
| [`as.integer64()`](https://bit64.r-lib.org/reference/as.integer64.character.md)           |                                                                                 | generic         |
| [`as.integer64.bitstring()`](https://bit64.r-lib.org/reference/as.integer64.character.md) | [`as.bitstring()`](https://bit64.r-lib.org/reference/as.character.integer64.md) |                 |
| [`as.integer64.character()`](https://bit64.r-lib.org/reference/as.integer64.character.md) | [`character()`](https://rdrr.io/r/base/character.html)                          |                 |
| [`as.integer64.double()`](https://bit64.r-lib.org/reference/as.integer64.character.md)    | [`double()`](https://rdrr.io/r/base/double.html)                                |                 |
| [`as.integer64.integer()`](https://bit64.r-lib.org/reference/as.integer64.character.md)   | [`integer()`](https://rdrr.io/r/base/integer.html)                              |                 |
| [`as.integer64.integer64()`](https://bit64.r-lib.org/reference/as.integer64.character.md) | `integer64`                                                                     |                 |
| [`as.integer64.logical()`](https://bit64.r-lib.org/reference/as.integer64.character.md)   | [`logical()`](https://rdrr.io/r/base/logical.html)                              |                 |
| [`as.integer64.NULL()`](https://bit64.r-lib.org/reference/as.integer64.character.md)      | [`NULL()`](https://rdrr.io/r/base/NULL.html)                                    |                 |

|                                                                                           |                                                                                 |                 |
|-------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------|-----------------|
| **coercing from integer64**                                                               | **see also**                                                                    | **description** |
| [`as.list.integer64()`](https://bit64.r-lib.org/reference/as.character.integer64.md)      | [`as.list()`](https://rdrr.io/r/base/list.html)                                 | generic         |
| [`as.bitstring()`](https://bit64.r-lib.org/reference/as.character.integer64.md)           | [`as.bitstring()`](https://bit64.r-lib.org/reference/as.character.integer64.md) | generic         |
| [`as.bitstring.integer64()`](https://bit64.r-lib.org/reference/as.character.integer64.md) |                                                                                 |                 |
| [`as.character.integer64()`](https://bit64.r-lib.org/reference/as.character.integer64.md) | [`as.character()`](https://rdrr.io/r/base/character.html)                       |                 |
| [`as.double.integer64()`](https://bit64.r-lib.org/reference/as.character.integer64.md)    | [`as.double()`](https://rdrr.io/r/base/double.html)                             |                 |
| [`as.integer.integer64()`](https://bit64.r-lib.org/reference/as.character.integer64.md)   | [`as.integer()`](https://rdrr.io/r/base/integer.html)                           |                 |
| [`as.logical.integer64()`](https://bit64.r-lib.org/reference/as.character.integer64.md)   | [`as.logical()`](https://rdrr.io/r/base/logical.html)                           |                 |

|                                                                                             |                                                                |                                              |
|---------------------------------------------------------------------------------------------|----------------------------------------------------------------|----------------------------------------------|
| **data structures**                                                                         | **see also**                                                   | **description**                              |
| [`c.integer64()`](https://bit64.r-lib.org/reference/c.integer64.md)                         | [`c()`](https://rdrr.io/r/base/c.html)                         | vector concatenate                           |
| [`cbind.integer64()`](https://bit64.r-lib.org/reference/c.integer64.md)                     | [`cbind()`](https://rdrr.io/r/base/cbind.html)                 | column bind                                  |
| [`rbind.integer64()`](https://bit64.r-lib.org/reference/c.integer64.md)                     | [`rbind()`](https://rdrr.io/r/base/cbind.html)                 | row bind                                     |
| [`as.data.frame.integer64()`](https://bit64.r-lib.org/reference/as.data.frame.integer64.md) | [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) | coerce atomic object to data.frame           |
|                                                                                             | [`data.frame()`](https://rdrr.io/r/base/data.frame.html)       | inherited from Base R since we have coercion |

|                                                                                    |                                               |                          |
|------------------------------------------------------------------------------------|-----------------------------------------------|--------------------------|
| **subscripting**                                                                   | **see also**                                  | **description**          |
| [`[.integer64`](https://bit64.r-lib.org/reference/extract.replace.integer64.md)    | [`[`](https://rdrr.io/r/base/Extract.html)    | vector and array extract |
| [`[<-.integer64`](https://bit64.r-lib.org/reference/extract.replace.integer64.md)  | [`[<-`](https://rdrr.io/r/base/Extract.html)  | vector and array assign  |
| [`[[.integer64`](https://bit64.r-lib.org/reference/extract.replace.integer64.md)   | [`[[`](https://rdrr.io/r/base/Extract.html)   | scalar extract           |
| [`[[<-.integer64`](https://bit64.r-lib.org/reference/extract.replace.integer64.md) | [`[[<-`](https://rdrr.io/r/base/Extract.html) | scalar assign            |

|                                                                     |                                                 |                   |
|---------------------------------------------------------------------|-------------------------------------------------|-------------------|
| **binary operators**                                                | **see also**                                    | **description**   |
| [`+.integer64`](https://bit64.r-lib.org/reference/xor.integer64.md) | [`+`](https://rdrr.io/r/base/Arithmetic.html)   | returns integer64 |
| [`-.integer64`](https://bit64.r-lib.org/reference/xor.integer64.md) | [`-`](https://rdrr.io/r/base/Arithmetic.html)   | returns integer64 |
| [`*.integer64`](https://bit64.r-lib.org/reference/xor.integer64.md) | [`*`](https://rdrr.io/r/base/Arithmetic.html)   | returns integer64 |
| [`^.integer64`](https://bit64.r-lib.org/reference/xor.integer64.md) | [`^`](https://rdrr.io/r/base/Arithmetic.html)   | returns double    |
| [`/.integer64`](https://bit64.r-lib.org/reference/xor.integer64.md) | [`/`](https://rdrr.io/r/base/Arithmetic.html)   | returns double    |
| [`%/%`](https://bit64.r-lib.org/reference/xor.integer64.md)         | [`%/%`](https://rdrr.io/r/base/Arithmetic.html) | returns integer64 |
| [`%%`](https://bit64.r-lib.org/reference/xor.integer64.md)          | [`%%`](https://rdrr.io/r/base/Arithmetic.html)  | returns integer64 |

|                                                                      |                                                |                 |
|----------------------------------------------------------------------|------------------------------------------------|-----------------|
| **comparison operators**                                             | **see also**                                   | **description** |
| [`==.integer64`](https://bit64.r-lib.org/reference/xor.integer64.md) | [`==`](https://rdrr.io/r/base/Comparison.html) |                 |
| [`!=.integer64`](https://bit64.r-lib.org/reference/xor.integer64.md) | [`!=`](https://rdrr.io/r/base/Comparison.html) |                 |
| `<.integer64`                                                        | `<`                                            |                 |
| `<=.integer64`                                                       | `<=`                                           |                 |
| `>.integer64`                                                        | `>`                                            |                 |
| `>=.integer64`                                                       | `>=`                                           |                 |

|                                                                        |                                              |                 |
|------------------------------------------------------------------------|----------------------------------------------|-----------------|
| **logical operators**                                                  | **see also**                                 | **description** |
| [`!.integer64`](https://bit64.r-lib.org/reference/format.integer64.md) | [`!`](https://rdrr.io/r/base/Logic.html)     |                 |
| `&.integer64`                                                          | `&`                                          |                 |
| [`|.integer64`](https://bit64.r-lib.org/reference/xor.integer64.md)    | [`|`](https://rdrr.io/r/base/Logic.html)     |                 |
| [`xor.integer64`](https://bit64.r-lib.org/reference/xor.integer64.md)  | [`xor()`](https://rdrr.io/r/base/Logic.html) |                 |

|                                                                                |                                                  |                              |
|--------------------------------------------------------------------------------|--------------------------------------------------|------------------------------|
| **math functions**                                                             | **see also**                                     | **description**              |
| [`is.na.integer64()`](https://bit64.r-lib.org/reference/format.integer64.md)   | [`is.na()`](https://rdrr.io/r/base/NA.html)      | returns logical              |
| [`format.integer64()`](https://bit64.r-lib.org/reference/format.integer64.md)  | [`format()`](https://rdrr.io/r/base/format.html) | returns character            |
| [`abs.integer64()`](https://bit64.r-lib.org/reference/format.integer64.md)     | [`abs()`](https://rdrr.io/r/base/MathFun.html)   | returns integer64            |
| [`sign.integer64()`](https://bit64.r-lib.org/reference/format.integer64.md)    | [`sign()`](https://rdrr.io/r/base/sign.html)     | returns integer64            |
| [`log.integer64()`](https://bit64.r-lib.org/reference/format.integer64.md)     | [`log()`](https://rdrr.io/r/base/Log.html)       | returns double               |
| [`log10.integer64()`](https://bit64.r-lib.org/reference/format.integer64.md)   | [`log10()`](https://rdrr.io/r/base/Log.html)     | returns double               |
| [`log2.integer64()`](https://bit64.r-lib.org/reference/format.integer64.md)    | [`log2()`](https://rdrr.io/r/base/Log.html)      | returns double               |
| [`sqrt.integer64()`](https://bit64.r-lib.org/reference/format.integer64.md)    | [`sqrt()`](https://rdrr.io/r/base/MathFun.html)  | returns double               |
| [`ceiling.integer64()`](https://bit64.r-lib.org/reference/format.integer64.md) | [`ceiling()`](https://rdrr.io/r/base/Round.html) | dummy returning its argument |
| [`floor.integer64()`](https://bit64.r-lib.org/reference/format.integer64.md)   | [`floor()`](https://rdrr.io/r/base/Round.html)   | dummy returning its argument |
| [`trunc.integer64()`](https://bit64.r-lib.org/reference/format.integer64.md)   | [`trunc()`](https://rdrr.io/r/base/Round.html)   | dummy returning its argument |
| [`round.integer64()`](https://bit64.r-lib.org/reference/format.integer64.md)   | [`round()`](https://rdrr.io/r/base/Round.html)   | dummy returning its argument |
| [`signif.integer64()`](https://bit64.r-lib.org/reference/format.integer64.md)  | [`signif()`](https://rdrr.io/r/base/Round.html)  | dummy returning its argument |

|                                                                                |                                                   |                 |
|--------------------------------------------------------------------------------|---------------------------------------------------|-----------------|
| **cumulative functions**                                                       | **see also**                                      | **description** |
| [`cummin.integer64()`](https://bit64.r-lib.org/reference/cumsum.integer64.md)  | [`cummin()`](https://rdrr.io/r/base/cumsum.html)  |                 |
| [`cummax.integer64()`](https://bit64.r-lib.org/reference/cumsum.integer64.md)  | [`cummax()`](https://rdrr.io/r/base/cumsum.html)  |                 |
| [`cumsum.integer64()`](https://bit64.r-lib.org/reference/cumsum.integer64.md)  | [`cumsum()`](https://rdrr.io/r/base/cumsum.html)  |                 |
| [`cumprod.integer64()`](https://bit64.r-lib.org/reference/cumsum.integer64.md) | [`cumprod()`](https://rdrr.io/r/base/cumsum.html) |                 |
| [`diff.integer64()`](https://bit64.r-lib.org/reference/cumsum.integer64.md)    | [`diff()`](https://rdrr.io/r/base/diff.html)      |                 |

|                                                                           |                                                 |                 |
|---------------------------------------------------------------------------|-------------------------------------------------|-----------------|
| **summary functions**                                                     | **see also**                                    | **description** |
| [`range.integer64()`](https://bit64.r-lib.org/reference/sum.integer64.md) | [`range()`](https://rdrr.io/r/base/range.html)  |                 |
| [`min.integer64()`](https://bit64.r-lib.org/reference/sum.integer64.md)   | [`min()`](https://rdrr.io/r/base/Extremes.html) |                 |
| [`max.integer64()`](https://bit64.r-lib.org/reference/sum.integer64.md)   | [`max()`](https://rdrr.io/r/base/Extremes.html) |                 |
| [`sum.integer64()`](https://bit64.r-lib.org/reference/sum.integer64.md)   | [`sum()`](https://rdrr.io/r/base/sum.html)      |                 |
| [`mean.integer64()`](https://bit64.r-lib.org/reference/qtile.md)          | [`mean()`](https://rdrr.io/r/base/mean.html)    |                 |
| [`prod.integer64()`](https://bit64.r-lib.org/reference/sum.integer64.md)  | [`prod()`](https://rdrr.io/r/base/prod.html)    |                 |
| [`all.integer64()`](https://bit64.r-lib.org/reference/sum.integer64.md)   | [`all()`](https://rdrr.io/r/base/all.html)      |                 |
| [`any.integer64()`](https://bit64.r-lib.org/reference/sum.integer64.md)   | [`any()`](https://rdrr.io/r/base/any.html)      |                 |

|                                                                                       |                                                           |                                                                    |
|---------------------------------------------------------------------------------------|-----------------------------------------------------------|--------------------------------------------------------------------|
| **algorithmically complex functions**                                                 | **see also**                                              | **description (caching)**                                          |
| [`match.integer64()`](https://bit64.r-lib.org/reference/match.integer64.md)           | [`match()`](https://bit64.r-lib.org/reference/bit64S3.md) | position of x in table (h//o/so)                                   |
| [`%in%.integer64`](https://bit64.r-lib.org/reference/match.integer64.md)              | [`%in%`](https://bit64.r-lib.org/reference/bit64S3.md)    | is x in table? (h//o/so)                                           |
| [`duplicated.integer64()`](https://bit64.r-lib.org/reference/duplicated.integer64.md) | [`duplicated()`](https://rdrr.io/r/base/duplicated.html)  | is current element duplicate of previous one? (h//o/so)            |
| [`unique.integer64()`](https://bit64.r-lib.org/reference/unique.integer64.md)         | [`unique()`](https://rdrr.io/r/base/unique.html)          | (shorter) vector of unique values only (h/s/o/so)                  |
| [`unipos.integer64()`](https://bit64.r-lib.org/reference/unipos.md)                   | [`unipos()`](https://bit64.r-lib.org/reference/unipos.md) | positions corresponding to unique values (h/s/o/so)                |
| [`tiepos.integer64()`](https://bit64.r-lib.org/reference/tiepos.md)                   | [`tiepos()`](https://bit64.r-lib.org/reference/tiepos.md) | positions of values that are tied (//o/so)                         |
| [`keypos.integer64()`](https://bit64.r-lib.org/reference/keypos.md)                   | [`keypos()`](https://bit64.r-lib.org/reference/keypos.md) | position of current value in sorted list of unique values (//o/so) |
| [`table.integer64()`](https://bit64.r-lib.org/reference/table.md)                     | [`table()`](https://bit64.r-lib.org/reference/table.md)   | unique values and their frequencies (h/s/o/so)                     |
| [`sort.integer64()`](https://bit64.r-lib.org/reference/sort.integer64.md)             | [`sort()`](https://rdrr.io/r/base/sort.html)              | sorted vector (/s/o/so)                                            |
| [`order.integer64()`](https://bit64.r-lib.org/reference/sort.integer64.md)            | [`order()`](https://bit64.r-lib.org/reference/bit64S3.md) | positions of elements that would create sorted vector (//o/so)     |
| [`rank.integer64()`](https://bit64.r-lib.org/reference/rank.integer64.md)             | [`rank()`](https://bit64.r-lib.org/reference/bit64S3.md)  | (average) ranks of non-NAs, NAs kept in place (/s/o/so)            |
| [`quantile.integer64()`](https://bit64.r-lib.org/reference/qtile.md)                  | [`quantile()`](https://rdrr.io/r/stats/quantile.html)     | (existing) values at specified percentiles (/s/o/so)               |
| [`median.integer64()`](https://bit64.r-lib.org/reference/qtile.md)                    | [`median()`](https://rdrr.io/r/stats/median.html)         | (existing) value at percentile 0.5 (/s/o/so)                       |
| [`summary.integer64()`](https://bit64.r-lib.org/reference/qtile.md)                   | [`summary()`](https://rdrr.io/r/base/summary.html)        | (/s/o/so)                                                          |
| [`all.equal.integer64()`](https://bit64.r-lib.org/reference/all.equal.integer64.md)   | [`all.equal()`](https://rdrr.io/r/base/all.equal.html)    | test if two objects are (nearly) equal (/s/o/so)                   |

|                                                                   |                                                                   |                            |
|-------------------------------------------------------------------|-------------------------------------------------------------------|----------------------------|
| **helper functions**                                              | **see also**                                                      | **description**            |
| [`minusclass()`](https://bit64.r-lib.org/reference/plusclass.md)  | [`minusclass()`](https://bit64.r-lib.org/reference/plusclass.md)  | removing class attritbute  |
| [`plusclass()`](https://bit64.r-lib.org/reference/plusclass.md)   | [`plusclass()`](https://bit64.r-lib.org/reference/plusclass.md)   | inserting class attribute  |
| [`binattr()`](https://bit64.r-lib.org/reference/xor.integer64.md) | [`binattr()`](https://bit64.r-lib.org/reference/xor.integer64.md) | define binary op behaviour |

|                          |                                                             |                       |
|--------------------------|-------------------------------------------------------------|-----------------------|
| **tested I/O functions** | **see also**                                                | **description**       |
|                          | [`read.table()`](https://rdrr.io/r/utils/read.table.html)   | inherited from Base R |
|                          | [`write.table()`](https://rdrr.io/r/utils/write.table.html) | inherited from Base R |
|                          | [`serialize()`](https://rdrr.io/r/base/serialize.html)      | inherited from Base R |
|                          | [`unserialize()`](https://rdrr.io/r/base/serialize.html)    | inherited from Base R |
|                          | [`save()`](https://rdrr.io/r/base/save.html)                | inherited from Base R |
|                          | [`load()`](https://rdrr.io/r/base/load.html)                | inherited from Base R |
|                          | [`dput()`](https://rdrr.io/r/base/dput.html)                | inherited from Base R |
|                          | [`dget()`](https://rdrr.io/r/base/dput.html)                | inherited from Base R |

## Limitations inherited from implementing 64 bit integers via an external package

- **vector size** of atomic vectors is still limited to
  [`.Machine$integer.max`](https://rdrr.io/r/base/zMachine.html).
  However, external memory extending packages such as ff or bigmemory
  can extend their address space now with `integer64`. Having 64 bit
  integers also help with those not so obvious address issues that arise
  once we exchange data with SQL databases and datawarehouses, which use
  big integers as surrogate keys, e.g. on indexed primary key columns.
  This puts R into a relatively strong position compared to certain
  commercial statistical softwares, which sell database connectivity but
  neither have the range of 64 bit integers, nor have integers at all,
  nor have a single numeric data type in their macro-glue-language.

- **literals** such as `123LL` would require changes to Base R, up to
  then we need to write (and call) `as.integer64(123L)` or
  `as.integer64(123)` or `as.integer64('123')`. Only the latter allows
  to specify numbers beyond Base R's numeric data types and therefore is
  the recommended way to use – using only one way may facilitate
  migrating code to literals at a later stage.

## Limitations inherited from Base R, Core team, can you change this?

- **[`identical()`](https://rdrr.io/r/base/identical.html)** with
  default parameters does not distinguish all bit-patterns of doubles.
  For testing purposes we provide a wrapper
  [`identical.integer64()`](https://bit64.r-lib.org/reference/identical.integer64.md)
  that will distinguish all bit-patterns. It would be desireable to have
  a single call of
  [`identical()`](https://rdrr.io/r/base/identical.html) handle both,
  [`double`](https://rdrr.io/r/base/double.html) and `integer64`.

- the **colon** operator
  [:](https://bit64.r-lib.org/reference/bit64S3.md) officially does not
  dispatch S3 methods, however, we have made it generic:

      from <- lim.integer64()[1]
      to <- from+99
      from:to

  As a limitation remains: it will only dispatch at its first argument
  `from` but not at its second `to`.

- **[`is.double()`](https://bit64.r-lib.org/reference/bit64S3.md)** does
  not dispatch S3 methods, However, we have made it generic and it will
  return `FALSE` on `integer64`.

- **[`c()`](https://rdrr.io/r/base/c.html)** only dispatches
  [`c.integer64()`](https://bit64.r-lib.org/reference/c.integer64.md) if
  the first argument is `integer64` and it does not recursively dispatch
  the proper method when called with argument `recursive=TRUE`.
  Therefore `c(list(integer64, integer64))` does not work and for now
  you can only call `c.integer64(list(x, x))`.

- **generic binary operators** fail to dispatch *any* user-defined S3
  method if the two arguments have two different S3 classes. For example
  we have two classes [`bit::bit`](https://rdrr.io/pkg/bit/man/bit.html)
  and [`bit::bitwhich`](https://rdrr.io/pkg/bit/man/bitwhich.html)
  sparsely representing boolean vectors and we have methods
  [`&.bit`](https://rdrr.io/pkg/bit/man/xor.html) and
  [`&.bitwhich`](https://rdrr.io/pkg/bit/man/xor.html). For an
  expression involving both as in `bit & bitwhich`, none of the two
  methods is dispatched. Instead a standard method is dispatched, which
  neither handles `bit` nor `bitwhich`. Although it lacks symmetry, the
  better choice would be to dispatch simply the method of the class of
  the first argument in case of class conflict. This choice would allow
  authors of extension packages providing coherent behaviour at least
  within their contributed classes. But as long as none of the package
  author's methods is dispatched, they cannot handle the conflicting
  classes at all.

- **[`unlist()`](https://rdrr.io/r/base/unlist.html)** is not generic
  and if it were, we would face similar problems as with
  [`c()`](https://rdrr.io/r/base/c.html)

- **[`vector()`](https://rdrr.io/r/base/vector.html)** with argument
  `mode='integer64'` cannot work without adjustment of Base R

- **[`as.vector()`](https://rdrr.io/r/base/vector.html)** with argument
  `mode='integer64'` cannot work without adjustment of Base R

- **[`is.vector()`](https://rdrr.io/r/base/vector.html)** does not
  dispatch its method `is.vector.integer64()`

- **[`mode<-()`](https://rdrr.io/r/base/mode.html)** drops the class
  'integer64' which is returned from
  [`as.integer64()`](https://bit64.r-lib.org/reference/as.integer64.character.md).
  Also it does not remove an existing class 'integer64' when assigning
  mode 'integer'.

- **[`storage.mode<-()`](https://rdrr.io/r/base/mode.html)** does not
  support external data types such as `integer64`

- **[`matrix()`](https://bit64.r-lib.org/reference/matrix64.md)** does
  drop the 'integer64' class attribute.

- **[`array()`](https://bit64.r-lib.org/reference/matrix64.md)** does
  drop the 'integer64' class attribute.

  - In current R versions (1.15.1) this can be circumvented by
    activating the function `as.vector.integer64()`. However, the CRAN
    maintainer has requested to remove `as.vector.integer64()`, even at
    the price of breaking previously working functionality of the
    package.

- **[`str()`](https://rdrr.io/r/utils/str.html)** does not print the
  values of `integer64` correctly

## Further limitations

- **subscripting** non-existing elements and subscripting with `NA`s is
  currently not supported. Such subscripting currently returns
  `9218868437227407266` instead of `NA` (the `NA` value of the
  underlying double code). Following the full R behaviour here would
  either destroy performance or require extensive C-coding.

## See also

[`integer()`](https://rdrr.io/r/base/integer.html) in base R

## Author

**Maintainer**: Michael Chirico <michaelchirico4@gmail.com>

Authors:

- Jens Oehlschlägel

Other contributors:

- Leonardo Silvestri \[contributor\]

- Ofek Shilon \[contributor\]

## Examples

``` r
message("Using integer64 in vector")
#> Using integer64 in vector
x <- integer64(8)    # create 64 bit vector
x
#> integer64
#> [1] 0 0 0 0 0 0 0 0
is.atomic(x)         # TRUE
#> [1] TRUE
is.integer64(x)      # TRUE
#> [1] TRUE
is.numeric(x)        # TRUE
#> [1] TRUE
is.integer(x)        # FALSE - debatable
#> [1] FALSE
is.double(x)         # FALSE - might change
#> [1] FALSE
x[] <- 1:2           # assigned value is recycled as usual
x[1:6]               # subscripting as usual
#> integer64
#> [1] 1 2 1 2 1 2
length(x) <- 13      # changing length as usual
x
#> integer64
#>  [1] 1 2 1 2 1 2 1 2 0 0 0 0 0
rep(x, 2)            # replicate as usual
#> integer64
#>  [1] 1 2 1 2 1 2 1 2 0 0 0 0 0 1 2 1 2 1 2 1 2 0 0 0 0 0
seq(as.integer64(1), 10)     # seq.integer64 is dispatched on first given argument
#> integer64
#>  [1] 1  2  3  4  5  6  7  8  9  10
seq(to=as.integer64(10), 1)  # seq.integer64 is dispatched on first given argument
#> integer64
#>  [1] 1  2  3  4  5  6  7  8  9  10
seq.integer64(along.with=x)  # or call seq.integer64 directly
#> integer64
#>  [1] 1  2  3  4  5  6  7  8  9  10 11 12 13
# c.integer64 is dispatched only if *first* argument is integer64 ...
x <- c(x,runif(length(x), max=100))
# ... and coerces everything to integer64 - including double
x
#> integer64
#>  [1] 1  2  1  2  1  2  1  2  0  0  0  0  0  48 80 25 65 3  98 57 83 31
#> [23] 49 43 65 58
names(x) <- letters  # use names as usual
x
#> integer64
#>  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w 
#>  1  2  1  2  1  2  1  2  0  0  0  0  0 48 80 25 65  3 98 57 83 31 49 
#>  x  y  z 
#> 43 65 58 

message("Using integer64 in array - note that 'matrix' currently does not work")
#> Using integer64 in array - note that 'matrix' currently does not work
message("as.vector.integer64 removed as requested by the CRAN maintainer")
#> as.vector.integer64 removed as requested by the CRAN maintainer
message("as consequence 'array' also does not work anymore")
#> as consequence 'array' also does not work anymore
message("we still can create a matrix or array by assigning 'dim'")
#> we still can create a matrix or array by assigning 'dim'
y <- rep(as.integer64(NA), 12)
dim(y) <- c(3,4)
dimnames(y) <- list(letters[1:3], LETTERS[1:4])
y["a",] <- 1:2       # assigning as usual
y
#> integer64
#>   A    B    C    D   
#> a 1    2    1    2   
#> b <NA> <NA> <NA> <NA>
#> c <NA> <NA> <NA> <NA>
y[1:2,-4]            # subscripting as usual
#> integer64
#>   A    B    C   
#> a 1    2    1   
#> b <NA> <NA> <NA>
# cbind.integer64 dispatched on any argument and coerces everything to integer64
cbind(E=1:3, F=runif(3, 0, 100), G=c("-1","0","1"), y)
#> integer64
#>   E F  G  A    B    C    D   
#> a 1 28 -1 1    2    1    2   
#> b 2 43 0  <NA> <NA> <NA> <NA>
#> c 3 88 1  <NA> <NA> <NA> <NA>

message("Using integer64 in data.frame")
#> Using integer64 in data.frame
str(as.data.frame(x))
#> 'data.frame':    26 obs. of  1 variable:
#>  $ x:integer64  1 2 1 2 1 2 1 2 ... 
str(as.data.frame(y))
#> 'data.frame':    3 obs. of  4 variables:
#>  $ A:integer64  1 NA NA 
#>  $ B:integer64  2 NA NA 
#>  $ C:integer64  1 NA NA 
#>  $ D:integer64  2 NA NA 
str(data.frame(y))
#> 'data.frame':    3 obs. of  4 variables:
#>  $ A:integer64  1 NA NA 
#>  $ B:integer64  2 NA NA 
#>  $ C:integer64  1 NA NA 
#>  $ D:integer64  2 NA NA 
str(data.frame(I(y)))
#> 'data.frame':    3 obs. of  1 variable:
#>  $ y:integer64 [1:3, 1:4] 1 NA NA 2 NA NA 1 NA ... 
d <- data.frame(x=x, y=runif(length(x), 0, 100))
d
#>    x         y
#> a  1 40.209214
#> b  2 72.937943
#> c  1 82.372018
#> d  2 42.105380
#> e  1 58.657630
#> f  2 37.348130
#> g  1  4.057624
#> h  2  8.671996
#> i  0 60.015616
#> j  0 75.417392
#> k  0 83.511327
#> l  0 87.916330
#> m  0  9.763049
#> n 48  1.792242
#> o 80 96.647537
#> p 25 95.348450
#> q 65 30.463774
#> r  3  7.708301
#> s 98 45.650084
#> t 57 62.120304
#> u 83 32.384298
#> v 31 58.292825
#> w 49  1.506723
#> x 43 38.195984
#> y 65 50.755832
#> z 58 23.737675
d$x
#> integer64
#>  [1] 1  2  1  2  1  2  1  2  0  0  0  0  0  48 80 25 65 3  98 57 83 31
#> [23] 49 43 65 58

message("Using integer64 with csv files")
#> Using integer64 with csv files
fi64 <- tempfile()
write.csv(d, file=fi64, row.names=FALSE)
e <- read.csv(fi64, colClasses=c("integer64", NA))
unlink(fi64)
str(e)
#> 'data.frame':    26 obs. of  2 variables:
#>  $ x:integer64  1 2 1 2 1 2 1 2 ... 
#>  $ y: num  40.2 72.9 82.4 42.1 58.7 ...
identical.integer64(d$x,e$x)
#> [1] TRUE

message("Serializing and unserializing integer64")
#> Serializing and unserializing integer64
dput(d, fi64)
e <- dget(fi64)
identical.integer64(d$x,e$x)
#> [1] TRUE
e <- d[,]
save(e, file=fi64)
rm(e)
load(file=fi64)
identical.integer64(d,e)
#> [1] TRUE

  if (FALSE) { # \dontrun{
message("== Differences between integer64 and int64 ==")
require(bit64)
require(int64)

message("-- integer64 is atomic --")
is.atomic(integer64())
#is.atomic(int64())
str(integer64(3))
#str(int64(3))

message("-- The following performance numbers are measured under RWin64  --")
message("-- under RWin32 the advantage of integer64 over int64 is smaller --")

message("-- integer64 needs 7x/5x less RAM than int64 under 64/32 bit OS
(and twice the RAM of integer as it should be) --")
#as.vector(object.size(int64(1e6))/object.size(integer64(1e6)))
as.vector(object.size(integer64(1e6))/object.size(integer(1e6)))

message("-- integer64 creates 2000x/1300x faster than int64 under 64/32 bit OS
(and 3x the time of integer) --")
t32 <- system.time(integer(1e8))
t64 <- system.time(integer64(1e8))
#T64 <- system.time(int64(1e7))*10  # using 1e8 as above stalls our R on an i7 8 GB RAM Thinkpad
#T64/t64
t64/t32

i32 <- sample(1e6)
d64 <- as.double(i32)

message("-- the following timings are rather conservative since timings
 of integer64 include garbage collection -- due to looped calls")
message("-- integer64 coerces 900x/100x faster than int64
 under 64/32 bit OS (and 2x the time of coercing to integer) --")
t32 <- system.time(for(i in 1:1000)as.integer(d64))
t64 <- system.time(for(i in 1:1000)as.integer64(d64))
#T64 <- system.time(as.int64(d64))*1000
#T64/t64
t64/t32
td64 <- system.time(for(i in 1:1000)as.double(i32))
t64 <- system.time(for(i in 1:1000)as.integer64(i32))
#T64 <- system.time(for(i in 1:10)as.int64(i32))*100
#T64/t64
t64/td64

message("-- integer64 serializes 4x/0.8x faster than int64
 under 64/32 bit OS (and less than 2x/6x the time of integer or double) --")
t32 <- system.time(for(i in 1:10)serialize(i32, NULL))
td64 <- system.time(for(i in 1:10)serialize(d64, NULL))
i64 <- as.integer64(i32);
t64 <- system.time(for(i in 1:10)serialize(i64, NULL))
rm(i64); gc()
#I64 <- as.int64(i32);
#T64 <- system.time(for(i in 1:10)serialize(I64, NULL))
#rm(I64); gc()
#T64/t64
t64/t32
t64/td64


message("-- integer64 adds 250x/60x faster than int64
 under 64/32 bit OS (and less than 6x the time of integer or double) --")
td64 <- system.time(for(i in 1:100)d64+d64)
t32 <- system.time(for(i in 1:100)i32+i32)
i64 <- as.integer64(i32);
t64 <- system.time(for(i in 1:100)i64+i64)
rm(i64); gc()
#I64 <- as.int64(i32);
#T64 <- system.time(for(i in 1:10)I64+I64)*10
#rm(I64); gc()
#T64/t64
t64/t32
t64/td64

message("-- integer64 sums 3x/0.2x faster than int64
(and at about 5x/60X the time of integer and double) --")
td64 <- system.time(for(i in 1:100)sum(d64))
t32 <- system.time(for(i in 1:100)sum(i32))
i64 <- as.integer64(i32);
t64 <- system.time(for(i in 1:100)sum(i64))
rm(i64); gc()
#I64 <- as.int64(i32);
#T64 <- system.time(for(i in 1:100)sum(I64))
#rm(I64); gc()
#T64/t64
t64/t32
t64/td64

message("-- integer64 diffs 5x/0.85x faster than integer and double
(int64 version 1.0 does not support diff) --")
td64 <- system.time(for(i in 1:10)diff(d64, lag=2L, differences=2L))
t32 <- system.time(for(i in 1:10)diff(i32, lag=2L, differences=2L))
i64 <- as.integer64(i32);
t64 <- system.time(for(i in 1:10)diff(i64, lag=2L, differences=2L))
rm(i64); gc()
t64/t32
t64/td64


message("-- integer64 subscripts 1000x/340x faster than int64
(and at the same speed / 10x slower as integer) --")
ts32 <- system.time(for(i in 1:1000)sample(1e6, 1e3))
t32<- system.time(for(i in 1:1000)i32[sample(1e6, 1e3)])
i64 <- as.integer64(i32);
t64 <- system.time(for(i in 1:1000)i64[sample(1e6, 1e3)])
rm(i64); gc()
#I64 <- as.int64(i32);
#T64 <- system.time(for(i in 1:100)I64[sample(1e6, 1e3)])*10
#rm(I64); gc()
#(T64-ts32)/(t64-ts32)
(t64-ts32)/(t32-ts32)

message("-- integer64 assigns 200x/90x faster than int64
(and 50x/160x slower than integer) --")
ts32 <- system.time(for(i in 1:100)sample(1e6, 1e3))
t32 <- system.time(for(i in 1:100)i32[sample(1e6, 1e3)] <- 1:1e3)
i64 <- as.integer64(i32);
i64 <- system.time(for(i in 1:100)i64[sample(1e6, 1e3)] <- 1:1e3)
rm(i64); gc()
#I64 <- as.int64(i32);
#I64 <- system.time(for(i in 1:10)I64[sample(1e6, 1e3)] <- 1:1e3)*10
#rm(I64); gc()
#(T64-ts32)/(t64-ts32)
(t64-ts32)/(t32-ts32)


tdfi32 <- system.time(dfi32 <- data.frame(a=i32, b=i32, c=i32))
tdfsi32 <- system.time(dfi32[1e6:1,])
fi32 <- tempfile()
tdfwi32 <- system.time(write.csv(dfi32, file=fi32, row.names=FALSE))
tdfri32 <- system.time(read.csv(fi32, colClasses=rep("integer", 3)))
unlink(fi32)
rm(dfi32); gc()

i64 <- as.integer64(i32);
tdfi64 <- system.time(dfi64 <- data.frame(a=i64, b=i64, c=i64))
tdfsi64 <- system.time(dfi64[1e6:1,])
fi64 <- tempfile()
tdfwi64 <- system.time(write.csv(dfi64, file=fi64, row.names=FALSE))
tdfri64 <- system.time(read.csv(fi64, colClasses=rep("integer64", 3)))
unlink(fi64)
rm(i64, dfi64); gc()

#I64 <- as.int64(i32);
#tdfI64 <- system.time(dfI64<-data.frame(a=I64, b=I64, c=I64))
#tdfsI64 <- system.time(dfI64[1e6:1,])
#fI64 <- tempfile()
#tdfwI64 <- system.time(write.csv(dfI64, file=fI64, row.names=FALSE))
#tdfrI64 <- system.time(read.csv(fI64, colClasses=rep("int64", 3)))
#unlink(fI64)
#rm(I64, dfI64); gc()

message("-- integer64 coerces 40x/6x faster to data.frame than int64
(and factor 1/9 slower than integer) --")
#tdfI64/tdfi64
tdfi64/tdfi32
message("-- integer64 subscripts from data.frame 20x/2.5x faster than int64
 (and 3x/13x slower than integer) --")
#tdfsI64/tdfsi64
tdfsi64/tdfsi32
message("-- integer64 csv writes about 2x/0.5x faster than int64
(and about 1.5x/5x slower than integer) --")
#tdfwI64/tdfwi64
tdfwi64/tdfwi32
message("-- integer64 csv reads about 3x/1.5 faster than int64
(and about 2x slower than integer) --")
#tdfrI64/tdfri64
tdfri64/tdfri32

rm(i32, d64); gc()


message("-- investigating the impact on garbage collection: --")
message("-- the fragmented structure of int64 messes up R's RAM --")
message("-- and slows down R's gargbage collection just by existing --")

td32 <- double(21)
td32[1] <- system.time(d64 <- double(1e7))[3]
for (i in 2:11)td32[i] <- system.time(gc(), gcFirst=FALSE)[3]
rm(d64)
for (i in 12:21)td32[i] <- system.time(gc(), gcFirst=FALSE)[3]

t64 <- double(21)
t64[1] <- system.time(i64 <- integer64(1e7))[3]
for (i in 2:11)t64[i] <- system.time(gc(), gcFirst=FALSE)[3]
rm(i64)
for (i in 12:21)t64[i] <- system.time(gc(), gcFirst=FALSE)[3]

#T64 <- double(21)
#T64[1] <- system.time(I64 <- int64(1e7))[3]
#for (i in 2:11)T64[i] <- system.time(gc(), gcFirst=FALSE)[3]
#rm(I64)
#for (i in 12:21)T64[i] <- system.time(gc(), gcFirst=FALSE)[3]

#matplot(1:21, cbind(td32, t64, T64), pch=c("d","i","I"), log="y")
matplot(1:21, cbind(td32, t64), pch=c("d","i"), log="y")
  } # }
```
