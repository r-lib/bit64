# Changelog

## bit64 4.7.99 (in development)

### BREAKING CHANGES

1.  {bit64} no longer `Depends` on {bit}; instead it `Imports` it.
    Please file an issue if this affects you before release; I plan to
    have {bit64} temporarily re-export some objects from {bit} to
    minimize the number of downstream packages/scripts adversely
    affected.

Here I copy the advice from 4.6.0-1 about how to adapt to this as a user
depending on {bit64} if it affects you:

Users relying on this in scripts can (1) write
[`library(bit)`](https://github.com/r-lib/bit) to attach {bit}
explicitly or (2) namespace-qualify all {bit} calls with `bit::`.

Package authors relying on this can (1) add `import(bit)` to make the
full {bit} namespace available or (2) namespace-qualify all {bit} calls
with `bit::`; adding {bit} to `Imports:` or `Suggests:` will also be
necessary.

1.  The following S3 methods now generate a warning if called directly
    (but not if called properly, i.e. *via* S3 dispatch on the generic):

    `:.default`, `:.integer64`, `[.integer64`, `[[.integer64`,
    `[[<-.integer64`, `%in%.default`, `%in%.integer64`,
    `length<-.integer64`, `all.equal.integer64`,
    `as.bitstring.integer64`, `as.integer64.bitstring`,
    `as.integer64.factor`, `as.integer64.integer64`,
    `as.integer64.NULL`, `as.list.integer64`, `as.logical.integer64`,
    `cbind.integer64`, `diff.integer64`, `duplicated.integer64`,
    `hashdup.cache_integer64`, `hashfin.cache_integer64`,
    `hashfun.integer64`, `hashmap.integer64`, `hashmaptab.integer64`,
    `hashmapuni.integer64`, `hashmapupo.integer64`,
    `hashpos.cache_integer64`, `hashrev.cache_integer64`,
    `hashrin.cache_integer64`, `hashtab.cache_integer64`,
    `hashuni.cache_integer64`, `hashupo.cache_integer64`,
    `is.double.default`, `is.double.integer64`, `is.finite.integer64`,
    `is.infinite.integer64`, `is.nan.integer64`, `is.sorted.integer64`,
    `is.vector.integer64`, `keypos.integer64`, `match.default`,
    `match.integer64`, `mean.integer64`, `median.integer64`,
    `mergeorder.integer64`, `mergesort.integer64`,
    `mergesortorder.integer64`, `na.count.integer64`, `nties.integer64`,
    `nunique.integer64`, `nvalid.integer64`, `order.default`,
    `order.integer64`, `orderdup.integer64`, `orderfin.integer64`,
    `orderkey.integer64`, `ordernut.integer64`, `orderpos.integer64`,
    `orderqtl.integer64`, `orderrnk.integer64`, `ordertab.integer64`,
    `ordertie.integer64`, `orderuni.integer64`, `orderupo.integer64`,
    `prank.integer64`, `print.bitstring`, `qtile.integer64`,
    `quantile.integer64`, `quickorder.integer64`, `quicksort.integer64`,
    `quicksortorder.integer64`, `radixorder.integer64`,
    `radixsort.integer64`, `radixsortorder.integer64`,
    `ramorder.integer64`, `ramsort.integer64`, `ramsortorder.integer64`,
    `rank.default`, `rbind.integer64`, `scale.integer64`,
    `shellorder.integer64`, `shellsort.integer64`,
    `shellsortorder.integer64`, `sort.integer64`, `sortfin.integer64`,
    `sortnut.integer64`, `sortorderdup.integer64`,
    `sortorderkey.integer64`, `sortorderpos.integer64`,
    `sortorderrnk.integer64`, `sortordertab.integer64`,
    `sortordertie.integer64`, `sortorderuni.integer64`,
    `sortorderupo.integer64`, `sortqtl.integer64`, `sorttab.integer64`,
    `sortuni.integer64`, `summary.integer64`, `table.integer64`,
    `tiepos.integer64`, `unipos.integer64`

    As noted in the notes for 4.6.0-1, there was no recorded instance of
    users calling these methods directly on GitHub; in the next release,
    they will be removed from the NAMESPACE. Note that
    `as.integer64.bitstring` is a new addition here – on follow-up, the
    only references were in old (e.g. 2020) Advent of Code solutions and
    archived repos.

    To disable this warning, use
    `options(bit64.warn.exported.s3.method = FALSE)`.

2.  The following S3 methods were directly removed from the NAMESPACE:

    `-.integer64`, `!.integer64`, `!=.integer64`, `*.integer64`,
    `/.integer64`, `&.integer64`, `%/%.integer64`, `%%.integer64`,
    `^.integer64`, `+.integer64`, `<.integer64`, `<=.integer64`,
    `==.integer64`, `>.integer64`, `>=.integer64`, `|.integer64`,
    `ceiling.integer64`, `cummax.integer64`, `cummin.integer64`,
    `cumprod.integer64`, `cumsum.integer64`, `floor.integer64`,
    `log10.integer64`, `log2.integer64`, `prod.integer64`,
    `range.integer64`, `round.integer64`, `sign.integer64`,
    `signif.integer64`, `sqrt.integer64`, `trunc.integer64`

    Owing to a quirk in R, it is not possible to pursue the same
    strategy for these methods as for those mentioned above because it
    is not possible to inspect the call stack to distinguish whether the
    corresponding generic was invoked or the method was invoked
    directly.

    Because there was no recorded direct usage for any of these, I am
    opting to just rip the band-aid off and un-export them in this
    release as opposed to waiting a full cycle more to do so.

3.  `as.integer64.integer64` returns a plain `integer64` vector stripped
    of any attributes. This is consistent with R like behavior,
    e.g. `as.integer.integer`.

4.  `%/%` matches base R/Knuth behavior of taking the
    [`floor()`](https://rdrr.io/r/base/Round.html) of a result, where
    before truncation was towards zero. For example,
    `as.integer64(-10L) %/% as.integer64(7L)` now gives `-2L`, not
    `-1L`. This is consistent with `-10L %/% 7L` in base R.
    Consequently, `%%` is also affected,
    e.g. `as.integer64(-10L) %% as.integer64(7L)` now gives `4L`, not
    `-3L`, consistent with `-10L %% 7L` in base R.

### NEW FEATURES

1.  `anyNA` gets an `integer64` method. Thanks
    [@hcirellu](https://github.com/hcirellu).
2.  `table` now gets a generic function and `table.integer64` is
    extended by [`base::table`](https://rdrr.io/r/base/table.html)
    parameters `exclude` and `useNA`
    ([\#59](https://github.com/r-lib/bit64/issues/59)). Thanks
    [@hcirellu](https://github.com/hcirellu). Note that as of now, for
    multiple inputs like `table(x, y, z)`, the efficient
    `table.integer64` implementation is only invoked if *all* of `x`,
    `y`, and `z` can be losslessly coerced to `integer64`. If, say, `y`
    is a character vector, the default method will be applied, leading
    to `x` being coerced to `character` (and then `factor`), which can
    result in unexpected ordering of the results when negative numbers
    are included. To get around this, do coercion yourself first (either
    coerce all inputs to `integer64` to get the fast implementation, or
    coerce the `integer64` input(s) to `factor` with the desired
    ordering).
3.  The [`seq()`](https://rdrr.io/r/base/seq.html) method for
    `integer64` has been overhauled to better match features from the
    default method.
    - The motivation is
      [\#47](https://github.com/r-lib/bit64/issues/47), where
      `seq(as.integer64(1L), 11L, length.out=6L)` calculated `by=`
      incorrectly to give `1:6` instead of `c(1L, 3L, ..., 9L, 11L)`.
    - `length.out=` was also sometimes ignored, for example
      `seq(to=as.integer64(5L), length.out=0L)` will now always just
      give
      [`integer64()`](https://bit64.r-lib.org/reference/bit64-package.md).
    - `seq(a, a, by=by)` is no longer an error.
    - We match the default method behavior of assuming `from=1` and
      `to=1` if needed in order to support usage like
      `seq(as.integer64(10L), by=-1L)` and
      `seq(by=as.integer64(3L), length.out=8L)`.
    - `seq(a, a, length.out=n)` will give `rep(a, n)`, not
      `seq(a, by=1, length.out=n)`.
4.  Coercion to/from integer64 is expanded greatly (includes
    [\#199](https://github.com/r-lib/bit64/issues/199)). Thanks
    [@hcirellu](https://github.com/hcirellu).
    - `as.Date`, `as.POSIXct`, `as.POSXlt`, `as.complex`, and `as.raw`
      get an `integer64` method.
    - `as.integer64` gets `Date`, `POSIXct`, `POSXlt`, `complex`, `raw`,
      and `difftime` methods.
5.  `as.integer64.character`:
    - Supports hexadecimal (base 16) input when prefixed with “0x” or
      “-0x”, e.g. `as.integer64("0x7FFFFFFFFFFFFFFF")`. Thanks
      [@hcirellu](https://github.com/hcirellu) for a PR which completes
      work begun by [@marcpaterno](https://github.com/marcpaterno).
    - Ignores leading/trailing whitespace (as does
      [`as.integer()`](https://rdrr.io/r/base/integer.html);
      [\#232](https://github.com/r-lib/bit64/issues/232)).
6.  `sortcache`, `sortordercache` and `ordercache` get a new argument
    `na.last`.
7.  `matrix`, `array`, `%*%` and `as.matrix` get an `integer64` method
    ([\#45](https://github.com/r-lib/bit64/issues/45)). Thanks
    [@hcirellu](https://github.com/hcirellu).
8.  `factor`, `as.factor`, `ordered`, and `as.ordered` support
    `integer64` input correctly, i.e. the levels are sorted according to
    `integer64` values. Thanks [@hcirellu](https://github.com/hcirellu).
9.  A replacement in an integer64 vector or array using `[<-` or `[[<-`
    with a complex or POSIXct leads to an R consistent coercion of the
    integer64 object to a complex or POSIXct object and not just an
    error. Thanks [@hcirellu](https://github.com/hcirellu).

### BUG FIXES

1.  `min.integer64`, `max.integer64` and `range.integer64` now support
    `na.rm=TRUE` correctly when combining across mutliple inputs like
    `min(x, NA_integer64_, na.rm=TRUE)`
    ([\#142](https://github.com/r-lib/bit64/issues/142)).
2.  `as.integer64.integer64` is consistent with `as.integer.integer` in
    terms or returning a plain integer64 vector (i.e., stripped of
    attributes; [\#188](https://github.com/r-lib/bit64/issues/188)).
    Thanks [@hcirellu](https://github.com/hcirellu).
3.  `log(integer64(), base=integer64(1))` no longer warns, consistent
    with `log(integer(), base=integer())`
    ([\#93](https://github.com/r-lib/bit64/issues/93)).
4.  `sortfin(integer64(), 1:10)` no longer segfaults
    ([\#164](https://github.com/r-lib/bit64/issues/164)).
5.  `orderfin(as.integer64(10:1), 1:3, 8:11)` enforces that `table` be
    sorted by `order` instead of segfaulting
    ([\#166](https://github.com/r-lib/bit64/issues/166)).
6.  [`ordertab()`](https://bit64.r-lib.org/reference/sortnut.md) no
    longer segfaults when `nunique` is smaller than the actual number of
    unique values ([\#168](https://github.com/r-lib/bit64/issues/168)).
7.  `as.integer64.character` now returns `NA` for out of range values,
    with warning, e.g. `as.integer64("22222222222222222222")`
    ([\#175](https://github.com/r-lib/bit64/issues/175)). Thanks
    [@hcirellu](https://github.com/hcirellu).
8.  [`quicksort()`](https://rdrr.io/pkg/bit/man/Sorting.html) and others
    no longer segfault on trivial cases (e.g. sorting 0 or 1 item,
    [\#220](https://github.com/r-lib/bit64/issues/220)).
9.  `as.integer64(2^63)` returns `NA_integer64_` more consistently
    (e.g. on ARM), consistent with `as.integer(2^31)`
    ([\#19](https://github.com/r-lib/bit64/issues/19)). Thanks
    [@dipterix](https://github.com/dipterix).
10. `[.integer64` now runs faster and correctly regarding `NA` and
    arrays ([\#176](https://github.com/r-lib/bit64/issues/176)). Thanks
    [@hcirellu](https://github.com/hcirellu).

### NOTES

1.  {bit64} no longer prints any start-up messages through an
    `.onAttach()` hook
    ([\#106](https://github.com/r-lib/bit64/issues/106)). Thanks
    [@hadley](https://github.com/hadley) for the request.
2.  The R version dependency has been bumped from 3.4.0 (2017) to 3.5.0
    (2018).

### BUG FIXES

1.  `median(NA_integer64_, na.rm=FALSE)` and `median(integer64())` now
    return `NA_integer64_`, aligning its behavior with
    `median(NA_integer_)`,
    [\#185](https://github.com/r-lib/bit64/issues/185). Previously the
    former threw an error while the latter gave an incorrect result.
    Thanks [@ben-schwen](https://github.com/ben-schwen) for the report
    and the PR.

## bit64 4.6.0-1 (2025-01-16)

CRAN release: 2025-01-16

### NOTICE OF PLANNED BREAKING CHANGES

1.  {bit64} exports many S3 methods directly. Calling S3 methods
    directly is generally bad form; we should rely on the S3 dispatch
    system for this. Needing to export an S3 method is usually
    indicative of some deep issue that’s otherwise hard to work around.

I plan to un-export most if not all S3 methods in future versions. In
this release, there will be no change in behavior besides this notice in
the NEWS. Going forward, I see two types of S3 exports: (1) exports that
have no discoverable direct usage (that is, a global GitHub search,
which includes the CRAN mirror, turned up *no* R code calling them
directly, except perhaps in `:::` form, which would be unaffected by
un-export); and (2) exports that *are* observed to be called directly by
some number of downstreams. With the former, I am more comfortable
un-exporting more aggressively; with the latter, I will take a more
gradual approach.

Here are the S3 methods that are currently exported, for which I found
no record of them being called directly:

`-.integer64`, `:.default`, `:.integer64`, `!.integer64`,
`!=.integer64`, `[.integer64`, `[[.integer64`, `[[<-.integer64`,
`*.integer64`, `/.integer64`, `&.integer64`, `%/%.integer64`,
`%%.integer64`, `%in%.default`, `%in%.integer64`, `^.integer64`,
`+.integer64`, `<.integer64`, `<=.integer64`, `==.integer64`,
`>.integer64`, `>=.integer64`, `|.integer64`, `all.equal.integer64`,
`as.bitstring.integer64`, `as.integer64.factor`,
`as.integer64.integer64`, `as.integer64.NULL`, `as.list.integer64`,
`as.logical.integer64`, `cbind.integer64`, `ceiling.integer64`,
`cummax.integer64`, `cummin.integer64`, `cumprod.integer64`,
`cumsum.integer64`, `diff.integer64`, `duplicated.integer64`,
`floor.integer64`, `hashdup.cache_integer64`, `hashfin.cache_integer64`,
`hashfun.integer64`, `hashmap.integer64`, `hashmaptab.integer64`,
`hashmapuni.integer64`, `hashmapupo.integer64`,
`hashpos.cache_integer64`, `hashrev.cache_integer64`,
`hashrin.cache_integer64`, `hashtab.cache_integer64`,
`hashuni.cache_integer64`, `hashupo.cache_integer64`,
`is.double.default`, `is.double.integer64`, `is.finite.integer64`,
`is.infinite.integer64`, `is.nan.integer64`, `is.sorted.integer64`,
`is.vector.integer64`, `keypos.integer64`, `length<-.integer64`,
`log10.integer64`, `log2.integer64`, `match.default`, `match.integer64`,
`mean.integer64`, `median.integer64`, `mergeorder.integer64`,
`mergesort.integer64`, `mergesortorder.integer64`, `na.count.integer64`,
`nties.integer64`, `nunique.integer64`, `nvalid.integer64`,
`order.default`, `order.integer64`, `orderdup.integer64`,
`orderfin.integer64`, `orderkey.integer64`, `ordernut.integer64`,
`orderpos.integer64`, `orderqtl.integer64`, `orderrnk.integer64`,
`ordertab.integer64`, `ordertie.integer64`, `orderuni.integer64`,
`orderupo.integer64`, `prank.integer64`, `print.bitstring`,
`prod.integer64`, `qtile.integer64`, `quantile.integer64`,
`quickorder.integer64`, `quicksort.integer64`,
`quicksortorder.integer64`, `radixorder.integer64`,
`radixsort.integer64`, `radixsortorder.integer64`, `ramorder.integer64`,
`ramsort.integer64`, `ramsortorder.integer64`, `range.integer64`,
`rank.default`, `rbind.integer64`, `round.integer64`, `scale.integer64`,
`shellorder.integer64`, `shellsort.integer64`,
`shellsortorder.integer64`, `sign.integer64`, `signif.integer64`,
`sort.integer64`, `sortfin.integer64`, `sortnut.integer64`,
`sortorderdup.integer64`, `sortorderkey.integer64`,
`sortorderpos.integer64`, `sortorderrnk.integer64`,
`sortordertab.integer64`, `sortordertie.integer64`,
`sortorderuni.integer64`, `sortorderupo.integer64`, `sortql.integer64`,
`sorttab.integer64`, `sortuni.integer64`, `sqrt.integer64`,
`summary.integer64`, `table.integer64`, `tiepos.integer64`,
`trunc.integer64`, `unipos.integer64`

Here are the S3 methods that are currently exported for which I *do*
find record of them being called directly:

`abs.integer64`, `as.character.integer64`, `as.data.frame.integer64`,
`as.double.integer64`, `as.integer.integer64`, `as.integer64.bitstring`,
`as.integer64.character`, `as.integer64.double`, `as.integer64.integer`,
`as.integer64.logical`, `c.integer64`, `format.integer64`,
`identical.integer64`, `is.na.integer64`, `lim.integer64`,
`max.integer64`, `min.integer64`, `print.integer64`, `rank.integer64`,
`seq.integer64`, `str.integer64`, `sum.integer64`, `unique.integer64`

In the next release (provisionally, 4.7.0), I will add a
[`warning()`](https://rdrr.io/r/base/warning.html) to any S3 method in
the former classification, while nothing will change for the latter
classification. I may reach out to authors observed to call the methods
directly.

In the subsequent release (provisionally, 4.8.0), I will un-export any
S3 method in the former classification, and add a
[`warning()`](https://rdrr.io/r/base/warning.html) to any S3 method in
the latter classification.

In the sub-subsequent release (provisionally, 4.9.0), I will un-export
any S3 method in the latter classification.

Please reach out (e.g., the GitHub log for
[\#76](https://github.com/r-lib/bit64/issues/76)) if you have any
concerns about this plan.

1.  {bit64} lists {bit} as `Depends:`. IMO this form of dependency
    should be deprecated by R now that `Imports:` is widely available
    and well-supported for many years.

In the next release (provisionally, 4.7.0), I will move bit to Imports.
The practical implication is that currently,
[`library(bit64)`](https://github.com/r-lib/bit64) will make {bit}
objects like [`is.bit()`](https://rdrr.io/pkg/bit/man/is.booltype.html)
available for use without namespace-qualification. This practice makes
code harder to read and maintain.

Users relying on this in scripts can (1) write
[`library(bit)`](https://github.com/r-lib/bit) to attach {bit}
explicitly or (2) namespace-qualify all {bit} calls with `bit::`.

Package authors relying on this can (1) add `import(bit)` to make the
full {bit} namespace available or (2) namespace-qualify all {bit} calls
with `bit::`; adding {bit} to `Imports:` or `Suggests:` will also be
necessary.

I will reach out to CRAN authors with any required changes. Depending on
the impact size, I might make this transition more gradual
(e.g. starting by re-exporting some or all {bit} functions from {bit64},
with warning, before un-exporting them in a subsequent release).

### NEW FEATURES

1.  Implemented S3 methods for
    [`rowSums()`](https://bit64.r-lib.org/reference/matrix64.md) and
    [`colSums()`](https://bit64.r-lib.org/reference/matrix64.md).
    Importantly they handle `NA` values correctly,
    [\#38](https://github.com/r-lib/bit64/issues/38). Thanks
    [@vlulla](https://github.com/vlulla) for the request. Note that
    these are implemented as wrappers to
    [`apply()`](https://rdrr.io/r/base/apply.html) calls, so they may
    not be as efficient. PRs welcome for implementing the efficient
    equivalents.

Note that by necessity, this grows the set of base exports overwritten
to include [`rowSums()`](https://bit64.r-lib.org/reference/matrix64.md)
and [`colSums()`](https://bit64.r-lib.org/reference/matrix64.md), which
are exported as S3 generics dispatching to
[`base::rowSums()`](https://rdrr.io/r/base/colSums.html) and
[`base::colSums()`](https://rdrr.io/r/base/colSums.html) by default.

1.  Partially powering this is a new
    [`aperm()`](https://rdrr.io/r/base/aperm.html) method for integer64
    which allows [`apply()`](https://rdrr.io/r/base/apply.html) to work
    as intended. Using [`apply()`](https://rdrr.io/r/base/apply.html)
    directly may still strip the integer64 class; that may be supported
    later (see [\#87](https://github.com/r-lib/bit64/issues/87)).

2.  [`is.na()`](https://rdrr.io/r/base/NA.html) is supported for long
    vector input (more than `2^31` elements),
    [\#30](https://github.com/r-lib/bit64/issues/30). Thanks
    [@ilia-kats](https://github.com/ilia-kats) for the request. Long
    vector support will be added on an as-needed basis as I don’t have a
    great machine for testing these features – PRs welcome!

### BUG FIXES

1.  [`all.equal.integer64()`](https://bit64.r-lib.org/reference/all.equal.integer64.md)
    gets the same fix for vector `scale=` to work as intended that
    [`all.equal.numeric()`](https://rdrr.io/r/base/all.equal.html) got
    in R 4.1.3, [\#23](https://github.com/r-lib/bit64/issues/23).

2.  Made edits to
    [`match()`](https://bit64.r-lib.org/reference/bit64S3.md) to handle
    `is.integer64(table)` better for older versions of R, including a
    new [`mtfrm()`](https://rdrr.io/r/base/mtfrm.html) method for
    integer64 objects in R\>=4.2.0,
    [\#85](https://github.com/r-lib/bit64/issues/85) and
    [\#111](https://github.com/r-lib/bit64/issues/111).

### NOTES

1.  After creating, developing, and maintaining {bit64} for about 13
    years, Jens Oehlschlägel has decided to step down as maintainer of
    the package. Michael Chirico will take over in this duty. Thank you
    Jens for creating such a wonderful & important part of the R
    ecosystem!

I don’t have any major plans for new features, and mostly hope to keep
the package running and up to date. Contributors most welcome! I am also
trying to freshen up the code base to make contribution easier.

1.  The R version dependency has increased from 3.0.1 (May 2013) to
    3.4.0 (April 2017). We plan to keep roughly the same R dependency as
    {data.table}, i.e., as old as possibly for as long as possible, with
    some bias towards gradually bringing in new R features to reduce the
    maintenance overhead of a growing nest of workarounds to keep the
    package “fresh” for users of the latest R versions.

Required package {bit} already requires R 3.4.0, so the old 3.0.1
requirement was effectively impossible anyway.

1.  Default packages {methods}, {stats}, and {utils} are now `Imports:`,
    not `Depends:`, dependencies. `Depends:` is an out-dated mode of
    dependency in R. This will only affect the small audience of users
    that run R with `R_DEFAULT_PACKAGES=NULL` (or some other subset
    excluding some of these three), *and* who are relying (perhaps
    implicitly) on {bit64} being responsible for attaching those
    packages.

It is my intention to move {bit} from `Depends:` to `Imports:` as well,
but this migration will be done more gingerly – it is more conceivable
that this will constitute a breaking change for some use cases,
therefore it will be done in phases. Nothing is done in this release,
but here is your earliest warning that from the next release, it will be
a warning to rely on {bit64} to attach {bit} functions for you.

1.  Package documentation is now managed with {roxygen2},
    [\#61](https://github.com/r-lib/bit64/issues/61). I tried to retain
    everything in the original documentation, but the diff required to
    do so was quite unmanageable (5,000+ lines), so please alert me if
    anything looks amiss. Most importantly, I ensured the NAMESPACE
    remains unchanged.

2.  The signature of
    [`identical.integer64()`](https://bit64.r-lib.org/reference/identical.integer64.md)
    loses `extptr.as.ref=`, which is unavailable for R\<4.2.0, but gains
    `...` to allow this argument in newer versions,
    [\#37](https://github.com/r-lib/bit64/issues/37). This retains the
    transparency of having all arguments named in the signature (and
    thus in
    [`?identical.integer64`](https://bit64.r-lib.org/reference/identical.integer64.md)
    as well as available for tab-completion) while also retaining the
    old R version dependency R 3.3.0.
