#' A S3 class for vectors of 64bit integers
#'
#' @description
#' Package 'bit64' provides fast serializable S3 atomic 64bit (signed) integers
#' that can be used in vectors, matrices, arrays and data.frames. Methods are
#' available for coercion from and to logicals, integers, doubles, characters
#' and factors as well as many elementwise and summary functions.
#'
#' ### Version 0.8
#' With 'integer64' vectors you can store very large integers at the expense
#' of 64 bits, which is by factor 7 better than 'int64' from package 'int64'.
#' Due to the smaller memory footprint, the atomic vector architecture and
#' using only S3 instead of S4 classes, most operations are one to three orders
#' of magnitude faster: Example speedups are 4x for serialization, 250x for
#' adding, 900x for coercion and 2000x for object creation. Also 'integer64'
#' avoids an ongoing (potentially infinite) penalty for garbage collection
#' observed during existence of 'int64' objects (see code in example section).
#'
#' ### Version 0.9
#' Package 'bit64' - which extends R with fast 64-bit integers - now has fast
#' (single-threaded) implementations the most important univariate algorithmic
#' operations (those based on hashing and sorting). We now have methods for
#' 'match', '%in%', 'duplicated', 'unique', 'table', 'sort', 'order', 'rank',
#' 'quantile', 'median' and 'summary'. Regarding data management we also have
#' novel generics 'unipos' (positions of the unique values), 'tiepos' (
#' positions of ties), 'keypos' (positions of foreign keys in a sorted
#' dimension table) and derived methods 'as.factor' and 'as.ordered'. This 64-
#' bit functionality is implemented carefully to be not slower than the
#' respective 32-bit operations in Base R and also to avoid outlying waiting
#' times observed with 'order', 'rank' and 'table' (speedup factors 20/16/200
#' respective). This increases the dataset size with wich we can work truly
#' interactive. The speed is achieved by simple heuristic optimizers in high-
#' level functions choosing the best from multiple low-level algorithms and
#' further taking advantage of a novel caching if activated. In an example R
#' session using a couple of these operations the 64-bit integers performed 22x
#'  faster than base 32-bit integers, hash-caching improved this to 24x,
#' sortorder-caching was most efficient with 38x (caching hashing and sorting
#' is not worth it with 32x at duplicated RAM consumption).
#'
#' # Design considerations
#'
#' 64 bit integers are related to big data: we need them to overcome address space
#' limitations. Therefore performance of the 64 bit integer type is critical. In the
#' S language -- designed in 1975 -- atomic objects were defined to be vectors for a
#' couple of good reasons: simplicity, option for implicit parallelization, good
#' cache locality. In recent years many analytical databases have learnt that lesson:
#' column based data bases provide superior performance for many applications, the
#' result are products such as MonetDB, Sybase IQ, Vertica, Exasol, Ingres Vectorwise.
#' If we introduce 64 bit integers not natively in Base R but as an external package,
#' we should at least strive to make them as 'basic' as possible. Therefore the design
#' choice of bit64 not only differs from package int64, it is obvious: Like the other
#' atomic types in Base R, we model data type 'integer64' as a contiguous [`atomic`]
#' vector in memory, and we use the more basic [S3] class system, not [S4]. Like
#' package int64 we want our 'integer64' to be [`serialize`]able, therefore we also
#' use an existing data type as the basis. Again the choice is obvious: R has only one
#' 64 bit data type: doubles. By using [`double`]s, `integer64` [`inherits`] some
#' functionality such as [is.atomic()], [length()], [`length<-`], [names()],
#' [`names<-`], [dim()], [`dim<-`], [dimnames()], [`dimnames<-`].
#'
#' Our R level functions strictly follow the functional programming paradigm:
#' no modification of arguments or other side-effects. Before version 0.93  we
#' internally deviated from the strict paradigm in order to boost performance. Our C
#' functions do not create new return values, instead we pass-in the memory to be
#' returned as an argument. This gives us the freedom to apply the C-function to new
#' or old vectors, which helps to avoid unnecessary memory allocation, unnecessary
#' copying and unnecessary garbage collection. Prior to 0.93 _within_ our R functions
#' we also deviated from conventional R programming by not using [`attr<-`] and
#' [`attributes<-`] because they always did new memory allocation and copying in older
#' R versions. If we wanted to set attributes of return values that we have freshly
#' created, we instead used functions [bit::setattr()] and [bit::setattributes()].
#' From version 0.93 `bit::setattr()` is only used for manipulating [`cache`] objects,
#' in [ramsort.integer64()], [sort.integer64()], and [as.data.frame.integer64()].
#'
#' # Arithmetic precision and coercion
#'
#' The fact that we introduce 64 bit long long integers -- without introducing 128-bit
#' long doubles -- creates some subtle challenges: Unlike 32 bit [`integer`]s, the
#' `integer64` are no longer a proper subset of [`double`]. If a binary arithmetic
#' operation does involve a `double` and a `integer`, it is a no-brainer to return
#' `double` without loss of information. If an `integer64` meets a `double`, it is not
#' trivial what type to return. Switching to `integer64` limits our ability to
#' represent very large numbers, switching to `double` limits our ability to
#' distinguish `x` from `x+1`. Since the latter is the purpose of introducing 64 bit
#' integers, we usually return `integer64` from functions involving `integer64`, for
#' example in [`c()`][c.integer64], [`cbind()`][cbind.integer64], and
#' [`rbind()`][rbind.integer64]
#'
#' Different from Base R, our operators [`+`][+.integer64], [`-`][-.integer64],
#' [`%/%`][%/%.integer64], and [`%%`][%%.integer64] coerce their arguments to
#' `integer64` and always return `integer64`.
#'
#' The multiplication operator [`*`][*.integer64] coerces its first argument to
#' `integer64` but allows its second argument to be also `double`: the second
#' argument is internaly coerced to 'long double' and the result of the
#' multiplication is returned as `integer64`.
#'
#' The division [`/`][/.integer64] and power [`^`][^.integer64] operators also
#' coerce their first argument to `integer64` and coerce internally their second
#' argument to 'long double', they return as `double`, like
#' [`sqrt()`][sqrt.integer64], [`log()`][log.integer64],
#' [`log2()`][log2.integer64], and [`log10()`][log10.integer64] do.
#'
#' | **argument1** | **op** | **argument2** | **->** | **coerced1** | **op** | **coerced2** | **->** | **result** |
#' |:-------------:|:------:|:-------------:|:------:|:------------:|:------:|:------------:|:------:|:----------:|
#' | integer64     | +      | double        | ->     | integer64    | +      | integer64    | ->     | integer64  |
#' | double        | +      | integer64     | ->     | integer64    | +      | integer64    | ->     | integer64  |
#' | integer64     | -      | double        | ->     | integer64    | -      | integer64    | ->     | integer64  |
#' | double        | -      | integer64     | ->     | integer64    | -      | integer64    | ->     | integer64  |
#' | integer64     | %/%    | double        | ->     | integer64    | %/%    | integer64    | ->     | integer64  |
#' | double        | %/%    | integer64     | ->     | integer64    | %/%    | integer64    | ->     | integer64  |
#' | integer64     | %%     | double        | ->     | integer64    | %%     | integer64    | ->     | integer64  |
#' | double        | %%     | integer64     | ->     | integer64    | %%     | integer64    | ->     | integer64  |
#' | integer64     | *      | double        | ->     | integer64    | *      | long double  | ->     | integer64  |
#' | double        | *      | integer64     | ->     | integer64    | *      | integer64    | ->     | integer64  |
#' | integer64     | /      | double        | ->     | integer64    | /      | long double  | ->     | double     |
#' | double        | /      | integer64     | ->     | integer64    | /      | long double  | ->     | double     |
#' | integer64     | ^      | double        | ->     | integer64    | /      | long double  | ->     | double     |
#' | double        | ^      | integer64     | ->     | integer64    | /      | long double  | ->     | double     |
#'
#' # Creating and testing S3 class 'integer64'
#'
#' Our creator function `integer64` takes an argument `length`, creates an atomic
#' double vector of this length, attaches an S3 class attribute 'integer64' to it,
#' and that's it. We simply rely on S3 method dispatch and interpret those 64-bit
#' elements as 'long long int'.
#'
#' [is.double()] currently returns TRUE for `integer64` and might return `FALSE` in
#' a later release. Consider `is.double()` to have undefined behavior and do query
#' [is.integer64()] _before_ querying `is.double()`.
#'
# As a second line of defense against misinterpretation we make `is.double()` return
# FALSE by making it S3 generic and adding a method `as.double.integer64()`.
#
#' The methods [is.integer64()] and [is.vector()] both return `TRUE` for `integer64`.
#' Note that we did not patch [storage.mode()] and [typeof()], which both continue
#' returning 'double'. Like for 32 bit [`integer`], [mode()] returns 'numeric' and
#' [as.double()] tries coercing to [`double`]. It is possible that 'integer64' becomes
#' a `vmode` in package ff.
#'
#' Further methods for creating `integer64` are [`range()`][range.integer64] which
#' returns the range of the data type if calles without arguments,
#' [`rep()`][rep.integer64], [`seq()`][seq.integer64].
#'
#' For all available methods on `integer64` vectors see the index below and the examples.
#'
#' # Index of implemented methods
#'
#' | **creating, testing, printing** | **see also**    | **description**           |
#' |--------------------------------:|----------------:|:--------------------------|
#' |                 `NA_integer64_` | [`NA_integer_`] | NA constant               |
#' |                     `integer64` |     [`integer`] | create zero atomic vector |
#' |                     [runif64()] |       [runif()] | create random vector      |
#' |               [rep.integer64()] |         [rep()] |                           |
#' |               [seq.integer64()] |         [seq()] |                           |
#' |                [is.integer64()] |          [is()] |                           |
#' |                                 |  [is.integer()] | inherited from Base R     |
#  |         [is.double.integer64()] |   [is.double()] |                           |
#' |         [is.vector.integer64()] |   [is.vector()] |                           |
#' |         [identical.integer64()] |   [identical()] |                           |
#' |          [`length<-.integer64`] |    [`length<-`] |                           |
#' |                                 |      [length()] | inherited from Base R     |
#' |                                       [`names<-`] | inherited from Base R     |
#' |                                 |       [names()] | inherited from Base R     |
#' |                                 |       [`dim<-`] | inherited from Base R     |
#' |                                 |         [dim()] | inherited from Base R     |
#' |                                 |  [`dimnames<-`] | inherited from Base R     |
#' |                                 |    [dimnames()] | inherited from Base R     |
#' |                                 |         [str()] | inherited from Base R, does not print values correctly |
#' |             [print.integer64()] |       [print()] |                           |
#' |               [str.integer64()] |         [str()] |                           |
#'
#' | **coercing to integer64**  | **see also**     | **description** |
#' |---------------------------:|-----------------:|:----------------|
#' |           [as.integer64()] |                  | generic         |
#' | [as.integer64.bitstring()] | [as.bitstring()] |                 |
#' | [as.integer64.character()] |    [character()] |                 |
#' |    [as.integer64.double()] |       [double()] |                 |
#' |   [as.integer64.integer()] |      [integer()] |                 |
#' | [as.integer64.integer64()] |      `integer64` |                 |
#' |   [as.integer64.logical()] |      [logical()] |                 |
#' |      [as.integer64.NULL()] |         [NULL()] |                 |
#'
#' | **coercing from integer64** | **see also**     | **description** |
#' |----------------------------:|-----------------:|:----------------|
#' |       [as.list.integer64()] |      [as.list()] | generic         |
#' |            [as.bitstring()] | [as.bitstring()] | generic         |
#' |  [as.bitstring.integer64()] |                  |                 |
#' |  [as.character.integer64()] | [as.character()] |                 |
#' |     [as.double.integer64()] |    [as.double()] |                 |
#' |    [as.integer.integer64()] |   [as.integer()] |                 |
#' |    [as.logical.integer64()] |   [as.logical()] |                 |
# removed as requested by the CRAN maintainer
#  |     [as.vector.integer64()] |    [as.vector()] |                 |
#'
#' | **data structures**         | **see also**      | **description**                    |
#' |----------------------------:|------------------:|:-----------------------------------|
#' |             [c.integer64()] |             [c()] | vector concatenate                 |
#' |         [cbind.integer64()] |         [cbind()] | column bind                        |
#' |         [rbind.integer64()] |         [rbind()] | row bind                           |
#' | [as.data.frame.integer64()] | [as.data.frame()] | coerce atomic object to data.frame |
#' |                             |    [data.frame()] | inherited from Base R since we have coercion |
#'
#' | **subscripting**                              | **see also**            | **description**          |
#' |----------------------------------------------:|------------------------:|:-------------------------|
#' |    [`[.integer64`][extract.replace.integer64] |    [`[`][base::Extract] | vector and array extract |
#' |  [`[<-.integer64`][extract.replace.integer64] |  [`[<-`][base::Extract] | vector and array assign  |
#' |   [`[[.integer64`][extract.replace.integer64] |   [`[[`][base::Extract] | scalar extract           |
#' | [`[[<-.integer64`][extract.replace.integer64] | [`[[<-`][base::Extract] | scalar assign            |
#'
#' | **binary operators** | **see also** | **description**   |
#' |---------------------:|-------------:|:------------------|
#' |      [`+.integer64`] |        [`+`] | returns integer64 |
#' |      [`-.integer64`] |        [`-`] | returns integer64 |
#' |      [`*.integer64`] |        [`*`] | returns integer64 |
#' |      [`^.integer64`] |        [`^`] | returns double    |
#' |      [`/.integer64`] |        [`/`] | returns double    |
#' |    [`%/%.integer64`] |      [`%/%`] | returns integer64 |
#' |     [`%%.integer64`] |       [`%%`] | returns integer64 |
#'
#' | **comparison operators** | **see also** | **description** |
#' |-------------------------:|-------------:|:----------------|
#' |         [`==.integer64`] |       [`==`] | |
#' |         [`!=.integer64`] |       [`!=`] | |
#' |          [`<.integer64`] |        [`<`] | |
#' |         [`<=.integer64`] |       [`<=`] | |
#' |          [`>.integer64`] |        [`>`] | |
#' |         [`>=.integer64`] |       [`>=`] | |
#'
#' \tabular{rrl}{
#'    \strong{logical operators} \tab \strong{see also} \tab \strong{description} \cr
#'    \code{\link{!.integer64}} \tab \code{\link{!}} \tab  \cr
#'    \code{\link{&.integer64}} \tab \code{\link{&}} \tab  \cr
#'    \code{\link[=xor.integer64]{|.integer64}} \tab \code{\link[base:Logic]{|}} \tab  \cr
#'    \code{\link{xor.integer64}} \tab \code{\link[=xor]{xor()}} \tab  \cr
#' }
# TODO(r-lib/roxygen2#1668): Restore the markdown representation of the table.
# | **logical operators** | **see also** | **description** |
# |----------------------:|-------------:|:----------------|
# |       [`!.integer64`] |        [`!`] | |
# |       [`&.integer64`] |        [`&`] | |
# | [`\|.integer64`][xor.integer64] | [`\|`][base::Logic] | |
# |     [`xor.integer64`] |      [xor()] | |
#'
#' | **math functions**    | **see also** | **description**              |
#' |----------------------:|-------------:|:-----------------------------|
#' |   [is.na.integer64()] |    [is.na()] | returns logical              |
#' |  [format.integer64()] |   [format()] | returns character            |
#' |     [abs.integer64()] |      [abs()] | returns integer64            |
#' |    [sign.integer64()] |     [sign()] | returns integer64            |
#' |     [log.integer64()] |      [log()] | returns double               |
#' |   [log10.integer64()] |    [log10()] | returns double               |
#' |    [log2.integer64()] |     [log2()] | returns double               |
#' |    [sqrt.integer64()] |     [sqrt()] | returns double               |
#' | [ceiling.integer64()] |  [ceiling()] | dummy returning its argument |
#' |   [floor.integer64()] |    [floor()] | dummy returning its argument |
#' |   [trunc.integer64()] |    [trunc()] | dummy returning its argument |
#' |   [round.integer64()] |    [round()] | dummy returning its argument |
#' |  [signif.integer64()] |   [signif()] | dummy returning its argument |
#'
#' | **cumulative functions** | **see also** | **description** |
#' |-------------------------:|-------------:|:----------------|
#' |     [cummin.integer64()] |   [cummin()] | |
#' |     [cummax.integer64()] |   [cummax()] | |
#' |     [cumsum.integer64()] |   [cumsum()] | |
#' |    [cumprod.integer64()] |  [cumprod()] | |
#' |       [diff.integer64()] |     [diff()] | |
#'
#' | **summary functions** | **see also** | **description** |
#' |----------------------:|-------------:|:----------------|
#' |   [range.integer64()] |    [range()] | |
#' |     [min.integer64()] |      [min()] | |
#' |     [max.integer64()] |      [max()] | |
#' |     [sum.integer64()] |      [sum()] | |
#' |    [mean.integer64()] |     [mean()] | |
#' |    [prod.integer64()] |     [prod()] | |
#' |     [all.integer64()] |      [all()] | |
#' |     [any.integer64()] |      [any()] | |
#'
#' | **algorithmically complex functions** | **see also** | **description (caching)**  |
#' |--------------------------------------:|-------------:|:---------------------------|
#' |      [match.integer64()] |      [match()] | position of x in table (h//o/so) |
#' |       [`%in%.integer64`] |       [`%in%`] | is x in table? (h//o/so) |
#' | [duplicated.integer64()] | [duplicated()] | is current element duplicate of previous one? (h//o/so) |
#' |     [unique.integer64()] |     [unique()] | (shorter) vector of unique values only (h/s/o/so) |
#' |     [unipos.integer64()] |     [unipos()] | positions corresponding to unique values (h/s/o/so) |
#' |     [tiepos.integer64()] |     [tiepos()] | positions of values that are tied (//o/so) |
#' |     [keypos.integer64()] |     [keypos()] | position of current value in sorted list of unique values (//o/so) |
#' |      [table.integer64()] |      [table()] | unique values and their frequencies (h/s/o/so) |
#' |       [sort.integer64()] |       [sort()] | sorted vector (/s/o/so) |
#' |      [order.integer64()] |      [order()] | positions of elements that would create sorted vector (//o/so) |
#' |       [rank.integer64()] |       [rank()] | (average) ranks of non-NAs, NAs kept in place (/s/o/so) |
#' |   [quantile.integer64()] |   [quantile()] | (existing) values at specified percentiles (/s/o/so) |
#' |     [median.integer64()] |     [median()] | (existing) value at percentile 0.5 (/s/o/so) |
#' |    [summary.integer64()] |    [summary()] | (/s/o/so) |
#' |  [all.equal.integer64()] |  [all.equal()] | test if two objects are (nearly) equal (/s/o/so) |
#'
#' | **helper functions** | **see also**   | **description**            |
#' |-----------------:|---------------:|:---------------------------|
#' |   [minusclass()] | [minusclass()] |  removing class attritbute |
#' |    [plusclass()] |  [plusclass()] |  inserting class attribute |
#' |      [binattr()] |    [binattr()] | define binary op behaviour |
#'
#' | **tested I/O functions** | **see also**    | **description**       |
#' |-------------------------:|----------------:|:----------------------|
#' |                          |  [read.table()] | inherited from Base R |
#' |                          | [write.table()] | inherited from Base R |
#' |                          |   [serialize()] | inherited from Base R |
#' |                          | [unserialize()] | inherited from Base R |
#' |                          |        [save()] | inherited from Base R |
#' |                          |        [load()] | inherited from Base R |
#' |                          |        [dput()] | inherited from Base R |
#' |                          |        [dget()] | inherited from Base R |
#'
#' # Limitations inherited from implementing 64 bit integers via an external package
#'
#'  - **vector size** of atomic vectors is still limited to
#'    [`.Machine$integer.max`][.Machine]. However, external memory extending packages
#'    such as ff or bigmemory can extend their address space now with `integer64`.
#'    Having 64 bit integers also help with those not so obvious address issues that
#'    arise once we exchange data with SQL databases and datawarehouses, which use big
#'    integers as surrogate keys, e.g. on indexed primary key columns. This puts R into
#'    a relatively strong position compared to certain commercial statistical softwares,
#'    which sell database connectivity but neither have the range of 64 bit integers,
#'    nor have integers at all, nor have a single numeric data type in their
#'    macro-glue-language.
#'  - **literals** such as `123LL` would require changes to Base R, up to then we need
#'    to write (and call) `as.integer64(123L)` or `as.integer64(123)` or
#'    `as.integer64('123')`. Only the latter allows to specify numbers beyond Base R's
#'    numeric data types and therefore is the recommended way to use -- using only one
#'    way may facilitate migrating code to literals at a later stage.
#'
#' # Limitations inherited from Base R, Core team, can you change this?
#'
#'  -  **[identical()]** with default parameters does not distinguish all bit-patterns of
#'     doubles. For testing purposes we provide a wrapper [identical.integer64()] that
#'     will distinguish all bit-patterns. It would be desireable to have a single call
#'     of `identical()` handle both, [`double`] and `integer64`.
#'
#'  - the **colon** operator [:] officially does not dispatch S3 methods, however, we
#'    have made it generic:
#'
#'    ```r
#'    from <- lim.integer64()[1]
#'    to <- from+99
#'    from:to
#'    ```
#'
#'    As a limitation remains: it will only dispatch at its first argument `from` but
#'    not at its second `to`.
#'
#'  - **[is.double()]** does not dispatch S3 methods, However, we have made it generic
#'    and it will return `FALSE` on `integer64`.
#'
#'  - **[c()]** only dispatches [c.integer64()] if the first argument is `integer64`
#'    and it does not recursively dispatch the proper method when called with argument
#'    `recursive=TRUE`. Therefore `c(list(integer64, integer64))` does not work and
#'    for now you can only call `c.integer64(list(x, x))`.
#'
#'  - **generic binary operators** fail to dispatch *any* user-defined S3 method
#'     if the two arguments have two different S3 classes. For example we have two
#'     classes [`bit::bit`] and [`bit::bitwhich`] sparsely representing boolean vectors
#'     and we have methods [`&.bit`][bit::xor.default] and
#'     [`&.bitwhich`][bit::xor.default]. For an expression involving both as in
#'     `bit & bitwhich`, none of the two methods is dispatched. Instead a standard
#'     method is dispatched, which neither handles `bit` nor `bitwhich`. Although
#'     it lacks symmetry, the better choice would be to dispatch simply the method
#'     of the class of the first argument in case of class conflict. This choice would
#'     allow authors of extension packages providing coherent behaviour at least within
#'     their contributed classes. But as long as none of the package author's methods is
#'     dispatched, they cannot handle the conflicting classes at all.
#'
#'  - **[unlist()]** is not generic and if it were, we would face similar problems as
#'    with [c()]
#'  - **[vector()]** with argument `mode='integer64'` cannot work without adjustment
#'    of Base R
#'  - **[as.vector()]** with argument `mode='integer64'` cannot work without adjustment
#'    of Base R
#'  - **[is.vector()]** does not dispatch its method [is.vector.integer64()]
#'  - **[mode<-()]** drops the class 'integer64' which is returned from
#'    `as.integer64()`. Also it does not remove an existing class 'integer64' when
#'    assigning mode 'integer'.
#'  - **[storage.mode<-()]** does not support external data types such as `integer64`
#'  - **[matrix()]** does drop the 'integer64' class attribute.
#'  - **[array()]**  does drop the 'integer64' class attribute.
#'    + In current R versions (1.15.1) this can be circumvented by activating the
#'      function `as.vector.integer64()`. However, the CRAN maintainer has requested
#'      to remove `as.vector.integer64()`, even at the price of breaking previously
#'      working functionality of the package.
#'
#'  - **[str()]** does not print the values of `integer64` correctly
#'
#' # Further limitations
#'
#'  - **subscripting** non-existing elements and subscripting with `NA`s is currently
#'    not supported. Such subscripting currently returns `9218868437227407266` instead
#'    of `NA` (the `NA` value of the underlying double code). Following the full R
#'    behaviour here would either destroy performance or require extensive C-coding.
#'
#' @note `integer64` are useful for handling database keys and exact counting in +-2^63.
#'   Do not use them as replacement for 32bit integers, integer64 are not supported for
#'   subscripting by R-core and they have different semantics when combined with double.
#'   Do understand that `integer64` can only be useful over `double` if we do not coerce
#'   it to `double`.
#'
#'   While
#'
#'   integer + double -> double + double -> double
#'
#'   or
#'
#'   1L + 0.5 -> 1.5
#'
#'   for additive operations we coerce to `integer64`
#'
#'   integer64 + double ->  integer64 + integer64 -> integer64
#'
#'   hence
#'
#'   as.integer64(1) + 0.5 -> 1LL + 0LL -> 1LL
#'
#'   see section "Arithmetic precision and coercion" above
#'
#' @seealso  [integer()] in base R
#' @examples
#' message("Using integer64 in vector")
#' x <- integer64(8)    # create 64 bit vector
#' x
#' is.atomic(x)         # TRUE
#' is.integer64(x)      # TRUE
#' is.numeric(x)        # TRUE
#' is.integer(x)        # FALSE - debatable
#' is.double(x)         # FALSE - might change
#' x[] <- 1:2           # assigned value is recycled as usual
#' x[1:6]               # subscripting as usual
#' length(x) <- 13      # changing length as usual
#' x
#' rep(x, 2)            # replicate as usual
#' seq(as.integer64(1), 10)     # seq.integer64 is dispatched on first given argument
#' seq(to=as.integer64(10), 1)  # seq.integer64 is dispatched on first given argument
#' seq.integer64(along.with=x)  # or call seq.integer64 directly
#' # c.integer64 is dispatched only if *first* argument is integer64 ...
#' x <- c(x,runif(length(x), max=100))
#' # ... and coerces everything to integer64 - including double
#' x
#' names(x) <- letters  # use names as usual
#' x
#'
#' message("Using integer64 in array - note that 'matrix' currently does not work")
#' message("as.vector.integer64 removed as requested by the CRAN maintainer")
#' message("as consequence 'array' also does not work anymore")
#  y <- array(as.integer64(NA), dim=c(3,4), dimnames=list(letters[1:3], LETTERS[1:4]))
#' message("we still can create a matrix or array by assigning 'dim'")
#' y <- rep(as.integer64(NA), 12)
#' dim(y) <- c(3,4)
#' dimnames(y) <- list(letters[1:3], LETTERS[1:4])
#' y["a",] <- 1:2       # assigning as usual
#' y
#' y[1:2,-4]            # subscripting as usual
#' # cbind.integer64 dispatched on any argument and coerces everything to integer64
#' cbind(E=1:3, F=runif(3, 0, 100), G=c("-1","0","1"), y)
#'
#' message("Using integer64 in data.frame")
#' str(as.data.frame(x))
#' str(as.data.frame(y))
#' str(data.frame(y))
#' str(data.frame(I(y)))
#' d <- data.frame(x=x, y=runif(length(x), 0, 100))
#' d
#' d$x
#'
#' message("Using integer64 with csv files")
#' fi64 <- tempfile()
#' write.csv(d, file=fi64, row.names=FALSE)
#' e <- read.csv(fi64, colClasses=c("integer64", NA))
#' unlink(fi64)
#' str(e)
#' identical.integer64(d$x,e$x)
#'
#' message("Serializing and unserializing integer64")
#' dput(d, fi64)
#' e <- dget(fi64)
#' identical.integer64(d$x,e$x)
#' e <- d[,]
#' save(e, file=fi64)
#' rm(e)
#' load(file=fi64)
#' identical.integer64(d,e)
#'
#'   \dontrun{
#' message("== Differences between integer64 and int64 ==")
#' require(bit64)
#' require(int64)
#'
#' message("-- integer64 is atomic --")
#' is.atomic(integer64())
#' #is.atomic(int64())
#' str(integer64(3))
#' #str(int64(3))
#'
#' message("-- The following performance numbers are measured under RWin64  --")
#' message("-- under RWin32 the advantage of integer64 over int64 is smaller --")
#'
#' message("-- integer64 needs 7x/5x less RAM than int64 under 64/32 bit OS
#' (and twice the RAM of integer as it should be) --")
#' #as.vector(object.size(int64(1e6))/object.size(integer64(1e6)))
#' as.vector(object.size(integer64(1e6))/object.size(integer(1e6)))
#'
#' message("-- integer64 creates 2000x/1300x faster than int64 under 64/32 bit OS
#' (and 3x the time of integer) --")
#' t32 <- system.time(integer(1e8))
#' t64 <- system.time(integer64(1e8))
#' #T64 <- system.time(int64(1e7))*10  # using 1e8 as above stalls our R on an i7 8 GB RAM Thinkpad
#' #T64/t64
#' t64/t32
#'
#' i32 <- sample(1e6)
#' d64 <- as.double(i32)
#'
#' message("-- the following timings are rather conservative since timings
#'  of integer64 include garbage collection -- due to looped calls")
#' message("-- integer64 coerces 900x/100x faster than int64
#'  under 64/32 bit OS (and 2x the time of coercing to integer) --")
#' t32 <- system.time(for(i in 1:1000)as.integer(d64))
#' t64 <- system.time(for(i in 1:1000)as.integer64(d64))
#' #T64 <- system.time(as.int64(d64))*1000
#' #T64/t64
#' t64/t32
#' td64 <- system.time(for(i in 1:1000)as.double(i32))
#' t64 <- system.time(for(i in 1:1000)as.integer64(i32))
#' #T64 <- system.time(for(i in 1:10)as.int64(i32))*100
#' #T64/t64
#' t64/td64
#'
#' message("-- integer64 serializes 4x/0.8x faster than int64
#'  under 64/32 bit OS (and less than 2x/6x the time of integer or double) --")
#' t32 <- system.time(for(i in 1:10)serialize(i32, NULL))
#' td64 <- system.time(for(i in 1:10)serialize(d64, NULL))
#' i64 <- as.integer64(i32);
#' t64 <- system.time(for(i in 1:10)serialize(i64, NULL))
#' rm(i64); gc()
#' #I64 <- as.int64(i32);
#' #T64 <- system.time(for(i in 1:10)serialize(I64, NULL))
#' #rm(I64); gc()
#' #T64/t64
#' t64/t32
#' t64/td64
#'
#'
#' message("-- integer64 adds 250x/60x faster than int64
#'  under 64/32 bit OS (and less than 6x the time of integer or double) --")
#' td64 <- system.time(for(i in 1:100)d64+d64)
#' t32 <- system.time(for(i in 1:100)i32+i32)
#' i64 <- as.integer64(i32);
#' t64 <- system.time(for(i in 1:100)i64+i64)
#' rm(i64); gc()
#' #I64 <- as.int64(i32);
#' #T64 <- system.time(for(i in 1:10)I64+I64)*10
#' #rm(I64); gc()
#' #T64/t64
#' t64/t32
#' t64/td64
#'
#' message("-- integer64 sums 3x/0.2x faster than int64
#' (and at about 5x/60X the time of integer and double) --")
#' td64 <- system.time(for(i in 1:100)sum(d64))
#' t32 <- system.time(for(i in 1:100)sum(i32))
#' i64 <- as.integer64(i32);
#' t64 <- system.time(for(i in 1:100)sum(i64))
#' rm(i64); gc()
#' #I64 <- as.int64(i32);
#' #T64 <- system.time(for(i in 1:100)sum(I64))
#' #rm(I64); gc()
#' #T64/t64
#' t64/t32
#' t64/td64
#'
#' message("-- integer64 diffs 5x/0.85x faster than integer and double
#' (int64 version 1.0 does not support diff) --")
#' td64 <- system.time(for(i in 1:10)diff(d64, lag=2L, differences=2L))
#' t32 <- system.time(for(i in 1:10)diff(i32, lag=2L, differences=2L))
#' i64 <- as.integer64(i32);
#' t64 <- system.time(for(i in 1:10)diff(i64, lag=2L, differences=2L))
#' rm(i64); gc()
#' t64/t32
#' t64/td64
#'
#'
#' message("-- integer64 subscripts 1000x/340x faster than int64
#' (and at the same speed / 10x slower as integer) --")
#' ts32 <- system.time(for(i in 1:1000)sample(1e6, 1e3))
#' t32<- system.time(for(i in 1:1000)i32[sample(1e6, 1e3)])
#' i64 <- as.integer64(i32);
#' t64 <- system.time(for(i in 1:1000)i64[sample(1e6, 1e3)])
#' rm(i64); gc()
#' #I64 <- as.int64(i32);
#' #T64 <- system.time(for(i in 1:100)I64[sample(1e6, 1e3)])*10
#' #rm(I64); gc()
#' #(T64-ts32)/(t64-ts32)
#' (t64-ts32)/(t32-ts32)
#'
#' message("-- integer64 assigns 200x/90x faster than int64
#' (and 50x/160x slower than integer) --")
#' ts32 <- system.time(for(i in 1:100)sample(1e6, 1e3))
#' t32 <- system.time(for(i in 1:100)i32[sample(1e6, 1e3)] <- 1:1e3)
#' i64 <- as.integer64(i32);
#' i64 <- system.time(for(i in 1:100)i64[sample(1e6, 1e3)] <- 1:1e3)
#' rm(i64); gc()
#' #I64 <- as.int64(i32);
#' #I64 <- system.time(for(i in 1:10)I64[sample(1e6, 1e3)] <- 1:1e3)*10
#' #rm(I64); gc()
#' #(T64-ts32)/(t64-ts32)
#' (t64-ts32)/(t32-ts32)
#'
#'
#' tdfi32 <- system.time(dfi32 <- data.frame(a=i32, b=i32, c=i32))
#' tdfsi32 <- system.time(dfi32[1e6:1,])
#' fi32 <- tempfile()
#' tdfwi32 <- system.time(write.csv(dfi32, file=fi32, row.names=FALSE))
#' tdfri32 <- system.time(read.csv(fi32, colClasses=rep("integer", 3)))
#' unlink(fi32)
#' rm(dfi32); gc()
#'
#' i64 <- as.integer64(i32);
#' tdfi64 <- system.time(dfi64 <- data.frame(a=i64, b=i64, c=i64))
#' tdfsi64 <- system.time(dfi64[1e6:1,])
#' fi64 <- tempfile()
#' tdfwi64 <- system.time(write.csv(dfi64, file=fi64, row.names=FALSE))
#' tdfri64 <- system.time(read.csv(fi64, colClasses=rep("integer64", 3)))
#' unlink(fi64)
#' rm(i64, dfi64); gc()
#'
#' #I64 <- as.int64(i32);
#' #tdfI64 <- system.time(dfI64<-data.frame(a=I64, b=I64, c=I64))
#' #tdfsI64 <- system.time(dfI64[1e6:1,])
#' #fI64 <- tempfile()
#' #tdfwI64 <- system.time(write.csv(dfI64, file=fI64, row.names=FALSE))
#' #tdfrI64 <- system.time(read.csv(fI64, colClasses=rep("int64", 3)))
#' #unlink(fI64)
#' #rm(I64, dfI64); gc()
#'
#' message("-- integer64 coerces 40x/6x faster to data.frame than int64
#' (and factor 1/9 slower than integer) --")
#' #tdfI64/tdfi64
#' tdfi64/tdfi32
#' message("-- integer64 subscripts from data.frame 20x/2.5x faster than int64
#'  (and 3x/13x slower than integer) --")
#' #tdfsI64/tdfsi64
#' tdfsi64/tdfsi32
#' message("-- integer64 csv writes about 2x/0.5x faster than int64
#' (and about 1.5x/5x slower than integer) --")
#' #tdfwI64/tdfwi64
#' tdfwi64/tdfwi32
#' message("-- integer64 csv reads about 3x/1.5 faster than int64
#' (and about 2x slower than integer) --")
#' #tdfrI64/tdfri64
#' tdfri64/tdfri32
#'
#' rm(i32, d64); gc()
#'
#'
#' message("-- investigating the impact on garbage collection: --")
#' message("-- the fragmented structure of int64 messes up R's RAM --")
#' message("-- and slows down R's gargbage collection just by existing --")
#'
#' td32 <- double(21)
#' td32[1] <- system.time(d64 <- double(1e7))[3]
#' for (i in 2:11)td32[i] <- system.time(gc(), gcFirst=FALSE)[3]
#' rm(d64)
#' for (i in 12:21)td32[i] <- system.time(gc(), gcFirst=FALSE)[3]
#'
#' t64 <- double(21)
#' t64[1] <- system.time(i64 <- integer64(1e7))[3]
#' for (i in 2:11)t64[i] <- system.time(gc(), gcFirst=FALSE)[3]
#' rm(i64)
#' for (i in 12:21)t64[i] <- system.time(gc(), gcFirst=FALSE)[3]
#'
#' #T64 <- double(21)
#' #T64[1] <- system.time(I64 <- int64(1e7))[3]
#' #for (i in 2:11)T64[i] <- system.time(gc(), gcFirst=FALSE)[3]
#' #rm(I64)
#' #for (i in 12:21)T64[i] <- system.time(gc(), gcFirst=FALSE)[3]
#'
#' #matplot(1:21, cbind(td32, t64, T64), pch=c("d","i","I"), log="y")
#' matplot(1:21, cbind(td32, t64), pch=c("d","i"), log="y")
#'   }
#' @aliases bit64 is.integer.integer64 is.vector.integer64
#' @keywords internal package classes manip
#' @useDynLib bit64, .registration = TRUE, .fixes = "C_"
"_PACKAGE"

## usethis namespace: start
#' @importFrom bit clone is.sorted keyorder keysort keysortorder mergeorder
#'   mergesort mergesortorder na.count nties nunique nvalid quickorder
#'   quicksort quicksortorder radixorder radixsort radixsortorder ramorder
#'   ramsort ramsortorder repeat.time setattr shellorder shellsort
#'   shellsortorder still.identical xor
#' @importFrom graphics barplot par title
#' @importFrom methods as is callGeneric
#' @importFrom stats cor median quantile
#' @importFrom utils head packageDescription strOptions tail getS3method
#' @export : :.default :.integer64
#' @export [.integer64 [[.integer64 [[<-.integer64 [<-.integer64
#' @export %in% %in%.default
#' @export %in%.integer64 abs.integer64
#' @export all.equal.integer64 all.integer64 any.integer64 as.bitstring.integer64
#' @export as.character.integer64 as.data.frame.integer64 as.double.integer64
#' @export as.integer.integer64 as.integer64.bitstring as.integer64.character
#' @export as.integer64.double as.integer64.factor as.integer64.integer
#' @export as.integer64.integer64 as.integer64.logical as.integer64.NULL
#' @export as.list.integer64 as.logical.integer64 c.integer64 cbind.integer64
#' @export diff.integer64 duplicated.integer64
#' @export format.integer64 hashdup.cache_integer64 hashfin.cache_integer64
#' @export hashfun.integer64 hashmap.integer64 hashmaptab.integer64
#' @export hashmapuni.integer64 hashmapupo.integer64 hashpos.cache_integer64
#' @export hashrev.cache_integer64 hashrin.cache_integer64
#' @export hashtab.cache_integer64 hashuni.cache_integer64
#' @export hashupo.cache_integer64 is.double is.double.default
#' @export is.double.integer64 is.finite.integer64 is.infinite.integer64
#' @export is.na.integer64 is.nan.integer64 is.sorted.integer64
#' @export is.vector.integer64 keypos.integer64 length<-.integer64 log.integer64
#' @export match match.default match.integer64
#' @export max.integer64 mean.integer64 median.integer64 mergeorder.integer64
#' @export mergesort.integer64 mergesortorder.integer64 min.integer64
#' @export na.count.integer64 nties.integer64 nunique.integer64 nvalid.integer64
#' @export order order.default order.integer64 orderdup.integer64
#' @export orderfin.integer64 orderkey.integer64 ordernut.integer64
#' @export orderpos.integer64 orderqtl.integer64 orderrnk.integer64
#' @export ordertab.integer64 ordertie.integer64 orderuni.integer64
#' @export orderupo.integer64 prank.integer64 print.bitstring print.cache
#' @export print.integer64 qtile.integer64 quantile.integer64
#' @export quickorder.integer64 quicksort.integer64 quicksortorder.integer64
#' @export radixorder.integer64 radixsort.integer64 radixsortorder.integer64
#' @export ramorder.integer64 ramsort.integer64 ramsortorder.integer64
#' @export rank rank.default rank.integer64 rbind.integer64
#' @export rep.integer64 scale.integer64 seq.integer64
#' @export shellorder.integer64 shellsort.integer64 shellsortorder.integer64
#' @export sort.integer64 sortfin.integer64
#' @export sortnut.integer64 sortorderdup.integer64 sortorderkey.integer64
#' @export sortorderpos.integer64 sortorderrnk.integer64 sortordertab.integer64
#' @export sortordertie.integer64 sortorderuni.integer64 sortorderupo.integer64
#' @export sortqtl.integer64 sorttab.integer64 sortuni.integer64
#' @export str.integer64 sum.integer64 summary.integer64 tiepos.integer64
#' @export unipos.integer64 unique.integer64 xor.integer64
## usethis namespace: end
NULL
