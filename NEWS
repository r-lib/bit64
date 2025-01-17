# bit64 4.6.0-1

## NOTICE OF PLANNED BREAKING CHANGES

1. {bit64} exports many S3 methods directly. Calling S3 methods directly is generally bad form; we should rely on the S3 dispatch system for this. Needing to export an S3 method is usually indicative of some deep issue that's otherwise hard to work around.

  I plan to un-export most if not all S3 methods in future versions. In this release, there will be no change in behavior besides this notice in the NEWS. Going forward, I see two types of S3 exports: (1) exports that have no discoverable direct usage (that is, a global GitHub search, which includes the CRAN mirror, turned up _no_ R code calling them directly, except perhaps in `:::` form, which would be unaffected by un-export); and (2) exports that _are_ observed to be called directly by some number of downstreams. With the former, I am more comfortable un-exporting more aggressively; with the latter, I will take a more gradual approach.

  Here are the S3 methods that are currently exported, for which I found no record of them being called directly:

  `-.integer64`, `:.default`, `:.integer64`, `!.integer64`, `!=.integer64`, `[.integer64`, `[[.integer64`, `[[<-.integer64`, `*.integer64`, `/.integer64`, `&.integer64`, `%/%.integer64`, `%%.integer64`, `%in%.default`, `%in%.integer64`, `^.integer64`, `+.integer64`, `<.integer64`, `<=.integer64`, `==.integer64`, `>.integer64`, `>=.integer64`, `|.integer64`, `all.equal.integer64`, `as.bitstring.integer64`, `as.integer64.factor`, `as.integer64.integer64`, `as.integer64.NULL`, `as.list.integer64`, `as.logical.integer64`, `cbind.integer64`, `ceiling.integer64`, `cummax.integer64`, `cummin.integer64`, `cumprod.integer64`, `cumsum.integer64`, `diff.integer64`, `duplicated.integer64`, `floor.integer64`, `hashdup.cache_integer64`, `hashfin.cache_integer64`, `hashfun.integer64`, `hashmap.integer64`, `hashmaptab.integer64`, `hashmapuni.integer64`, `hashmapupo.integer64`, `hashpos.cache_integer64`, `hashrev.cache_integer64`, `hashrin.cache_integer64`, `hashtab.cache_integer64`, `hashuni.cache_integer64`, `hashupo.cache_integer64`, `is.double.default`, `is.double.integer64`, `is.finite.integer64`, `is.infinite.integer64`, `is.nan.integer64`, `is.sorted.integer64`, `is.vector.integer64`, `keypos.integer64`, `length<-.integer64`, `log10.integer64`, `log2.integer64`, `match.default`, `match.integer64`, `mean.integer64`, `median.integer64`, `mergeorder.integer64`, `mergesort.integer64`, `mergesortorder.integer64`, `na.count.integer64`, `nties.integer64`, `nunique.integer64`, `nvalid.integer64`, `order.default`, `order.integer64`, `orderdup.integer64`, `orderfin.integer64`, `orderkey.integer64`, `ordernut.integer64`, `orderpos.integer64`, `orderqtl.integer64`, `orderrnk.integer64`, `ordertab.integer64`, `ordertie.integer64`, `orderuni.integer64`, `orderupo.integer64`, `prank.integer64`, `print.bitstring`, `prod.integer64`, `qtile.integer64`, `quantile.integer64`, `quickorder.integer64`, `quicksort.integer64`, `quicksortorder.integer64`, `radixorder.integer64`, `radixsort.integer64`, `radixsortorder.integer64`, `ramorder.integer64`, `ramsort.integer64`, `ramsortorder.integer64`, `range.integer64`, `rank.default`, `rbind.integer64`, `round.integer64`, `scale.integer64`, `shellorder.integer64`, `shellsort.integer64`, `shellsortorder.integer64`, `sign.integer64`, `signif.integer64`, `sort.integer64`, `sortfin.integer64`, `sortnut.integer64`, `sortorderdup.integer64`, `sortorderkey.integer64`, `sortorderpos.integer64`, `sortorderrnk.integer64`, `sortordertab.integer64`, `sortordertie.integer64`, `sortorderuni.integer64`, `sortorderupo.integer64`, `sortql.integer64`, `sorttab.integer64`, `sortuni.integer64`, `sqrt.integer64`, `summary.integer64`, `table.integer64`, `tiepos.integer64`, `trunc.integer64`, `unipos.integer64`

  Here are the S3 methods that are currently exported for which I _do_ find record of them being called directly:

  `abs.integer64`, `as.character.integer64`, `as.data.frame.integer64`, `as.double.integer64`, `as.integer.integer64`, `as.integer64.bitstring`, `as.integer64.character`, `as.integer64.double`, `as.integer64.integer`, `as.integer64.logical`, `c.integer64`, `format.integer64`, `identical.integer64`, `is.na.integer64`, `lim.integer64`, `max.integer64`, `min.integer64`, `print.integer64`, `rank.integer64`, `seq.integer64`, `str.integer64`, `sum.integer64`, `unique.integer64`

  In the next release (provisionally, 4.7.0), I will add a `warning()` to any S3 method in the former classification, while nothing will change for the latter classification. I may reach out to authors observed to call the methods directly.

  In the subsequent release (provisionally, 4.8.0), I will un-export any S3 method in the former classification, and add a `warning()` to any S3 method in the latter classification.

  In the sub-subsequent release (provisionally, 4.9.0), I will un-export any S3 method in the latter classification.

  Please reach out (e.g., the GitHub log for #76) if you have any concerns about this plan.

1. {bit64} lists {bit} as `Depends:`. IMO this form of dependency should be deprecated by R now that `Imports:` is widely available and well-supported for many years.

  In the next release (provisionally, 4.7.0), I will move bit to Imports. The practical implication is that currently, `library(bit64)` will make {bit} objects like `is.bit()` available for use without namespace-qualification. This practice makes code harder to read and maintain.
  
  Users relying on this in scripts can (1) write `library(bit)` to attach {bit} explicitly or (2) namespace-qualify all {bit} calls with `bit::`.
  
  Package authors relying on this can (1) add `import(bit)` to make the full {bit} namespace available or (2) namespace-qualify all {bit} calls with `bit::`; adding {bit} to `Imports:` or `Suggests:` will also be necessary.
  
  I will reach out to CRAN authors with any required changes. Depending on the impact size, I might make this transition more gradual (e.g. starting by re-exporting some or all {bit} functions from {bit64}, with warning, before un-exporting them in a subsequent release).

## NEW FEATURES

1. Implemented S3 methods for `rowSums()` and `colSums()`. Importantly they handle `NA` values correctly, #38. Thanks @vlulla for the request. Note that these are implemented as wrappers to `apply()` calls, so they may not be as efficient. PRs welcome for implementing the efficient equivalents.

  Note that by necessity, this grows the set of base exports overwritten to include `rowSums()` and `colSums()`, which are exported as S3 generics dispatching to `base::rowSums()` and `base::colSums()` by default.

1. Partially powering this is a new `aperm()` method for integer64 which allows `apply()` to work as intended. Using `apply()` directly may still strip the integer64 class; that may be supported later (see #87).

1. `is.na()` is supported for long vector input (more than `2^31` elements), #30. Thanks @ilia-kats for the request. Long vector support will be added on an as-needed basis as I don't have a great machine for testing these features -- PRs welcome!

## BUG FIXES

1. `all.equal.integer64()` gets the same fix for vector `scale=` to work as intended that `all.equal.numeric()` got in R 4.1.3, #23.

1. Made edits to `match()` to handle `is.integer64(table)` better for older versions of R, including a new `mtfrm()` method for integer64 objects in R>=4.2.0, #85 and #111.

## NOTES

1. After creating, developing, and maintaining {bit64} for about 13 years, Jens Oehlschlägel has decided to step down as maintainer of the package. Michael Chirico will take over in this duty. Thank you Jens for creating such a wonderful & important part of the R ecosystem!

  I don't have any major plans for new features, and mostly hope to keep the package running and up to date. Contributors most welcome! I am also trying to freshen up the code base to make contribution easier.

1. The R version dependency has increased from 3.0.1 (May 2013) to 3.4.0 (April 2017). We plan to keep roughly the same R dependency as {data.table}, i.e., as old as possibly for as long as possible, with some bias towards gradually bringing in new R features to reduce the maintenance overhead of a growing nest of workarounds to keep the package "fresh" for users of the latest R versions.

  Required package {bit} already requires R 3.4.0, so the old 3.0.1 requirement was effectively impossible anyway.

1. Default packages {methods}, {stats}, and {utils} are now `Imports:`, not `Depends:`, dependencies. `Depends:` is an out-dated mode of dependency in R. This will only affect the small audience of users that run R with `R_DEFAULT_PACKAGES=NULL` (or some other subset excluding some of these three), _and_ who are relying (perhaps implicitly) on {bit64} being responsible for attaching those packages.

  It is my intention to move {bit} from `Depends:` to `Imports:` as well, but this migration will be done more gingerly -- it is more conceivable that this will constitute a breaking change for some use cases, therefore it will be done in phases. Nothing is done in this release, but here is your earliest warning that from the next release, it will be a warning to rely on {bit64} to attach {bit} functions for you.

1. Package documentation is now managed with {roxygen2}, #61. I tried to retain everything in the original documentation, but the diff required to do so was quite unmanageable (5,000+ lines), so please alert me if anything looks amiss. Most importantly, I ensured the NAMESPACE remains unchanged.

1. The signature of `identical.integer64()` loses `extptr.as.ref=`, which is unavailable for R<4.2.0, but gains `...` to allow this argument in newer versions, #37. This retains the transparency of having all arguments named in the signature (and thus in `?identical.integer64` as well as available for tab-completion) while also retaining the old R version dependency R 3.3.0.

# bit64 NEWS for versions 0.8-3 through 4.5.2 are now in [NEWS.0](https://github.com/r-lib/bit64/blob/master/NEWS.0)
