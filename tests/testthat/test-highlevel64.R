test_that("match & %in% basics work", {
  x = as.integer64(2:5)
  y = as.integer64(3:6)
  expect_identical(match(x, y), c(NA, 1:3))
  expect_identical(match(y, x), c(2:4, NA))

  expect_identical(match(2:5, y), c(NA, 1:3))
  expect_identical(match(as.numeric(2:5), y), c(NA, 1:3))
  expect_identical(match(y, 2:5), c(2:4, NA))
  expect_identical(match(y, as.numeric(2:5)), c(2:4, NA))

  expect_identical(match(x, y, nomatch=0L), 0:3)

  expect_identical(x %in% y, c(FALSE, TRUE, TRUE, TRUE))
  expect_identical(y %in% x, c(TRUE, TRUE, TRUE, FALSE))
  expect_identical(x %in% 3:6, c(FALSE, TRUE, TRUE, TRUE))
  expect_identical(x %in% c(3.0, 4.0, 5.0, 6.0), c(FALSE, TRUE, TRUE, TRUE))

  expect_identical(match(integer64(), as.integer64(1L)), integer())
  expect_identical(match(as.integer64(1L), integer64()), NA_integer_)
  expect_identical(match(as.integer64(1L), integer64(), nomatch = 0L), 0L)

  x_nm <- as.integer64(c(1L, 3L))
  table_nm <- as.integer64(2:4)
  expect_identical(match(x_nm, table_nm, nomatch = -1L), c(-1L, 2L))
})

test_that("Different method= for match() and %in% work", {
  x = as.integer64(2:5)
  y = as.integer64(3:6)
  expected = c(NA_integer_, 1:3)

  expect_identical(match(x, y, method="hashpos"), expected)
  expect_identical(match(x, y, method="hashrev"), expected)
  expect_identical(match(x, y, method="sortorderpos"), expected)
  expect_error(match(x, y, method="_unknown_"), "'arg' should be one of", fixed=TRUE)
  # TODO(#58): Fix this, currently fails.
  # expect_identical(match(x, y, method="orderpos"), expected)

  # NB: %in% is quite a bit different; while there's a public API to
  #   `%in%.integer64`, likely, there shouldn't be (it's strange to export
  #   an S3 method like is currently done). The tests are designed to tickle
  #   the different methods through the public API only; this makes them
  #   prone to winding up testing something totally different later. I think
  #   that's fine; now that we have coverage tests up, any refactor that bumps
  #   around what exactly the following tests are covering, will show up in the PR.

  # method="hashrin" used when x is "short" but table is "long"
  x = as.integer64(seq_len(10L))
  table = as.integer64(seq_len(2.0**16.0 * 2.0/3.0 + 10.0)) # invert condition for bx>=16, 10.0 arbitrary buffer
  expect_identical(x %in% table, rep(TRUE, 10L))
})

test_that("match.integer64: automatic method selection without cache", {
  # hashrev path: short x, long table
  nx <- 10L
  ny <- 2L^16L
  x <- as.integer64(1:nx)
  table <- as.integer64(1:ny)
  # As of this writing, this should invoke hashrev
  expect_identical(match(x, table), 1:nx)

  # hashpos path: long x, short table
  x_long_match <- as.integer64(1:ny)
  table_short_match <- as.integer64(1:nx)
  # As of this writing, this should use hashpos
  expect_identical(match(x_long_match[1:nx], table_short_match), 1:nx)
  expect_identical(match(x_long_match[(nx+1L):(nx+10L)], table_short_match), rep(NA_integer_, 10L))
})

test_that("%in%.integer64: automatic method selection without cache", {
  # hashrin path: short x, long table
  nx <- 10L
  ny <- 2L^16L
  x <- as.integer64(1:nx)
  table <- as.integer64(1:ny)
  # As of this writing, this should use hashrin
  expect_identical(x %in% table, rep(TRUE, nx))

  # hashfin path: long x, short table
  x_long_in <- as.integer64(1:ny)
  table_short_in <- as.integer64(1:nx)
  # As of this writing, this should use hashfin.
  expect_identical(x_long_in[1:nx] %in% table_short_in, rep(TRUE, nx))
  expect_identical(x_long_in[(nx+1L):(nx+10L)] %in% table_short_in, rep(FALSE, 10L))
})

test_that("match.integer64: cache-based method selection", {
  x <- as.integer64(c(1L, 3L, 5L))
  table <- as.integer64(c(2L, 4L, 3L, 1L))

  # hashcache
  hashcache(table)
  expect_identical(match(x, table), c(4L, 3L, NA_integer_))
  remcache(table)

  # sortordercache
  sortordercache(table)
  expect_identical(match(x, table), c(4L, 3L, NA_integer_))
  remcache(table)

  # ordercache
  ordercache(table)
  expect_identical(match(x, table), c(4L, 3L, NA_integer_))
  remcache(table)
})

test_that("%in%.integer64: cache-based method selection", {
  x <- as.integer64(c(1L, 3L, 5L))
  table <- as.integer64(c(2L, 4L, 3L, 1L))
  expected_in <- c(TRUE, TRUE, FALSE)

  # hashcache
  hashcache(table)
  expect_identical(x %in% table, expected_in)
  remcache(table)

  # sortcache
  sortcache(table)
  expect_identical(x %in% table, expected_in)
  remcache(table)

  # ordercache
  ordercache(table)
  expect_identical(x %in% table, expected_in)
  remcache(table)
})

test_that("match.integer64: nunique argument", {
  x <- as.integer64(c(1L, 3L, 5L))
  table <- as.integer64(c(2L, 4L, 3L, 1L, 3L))
  expect_identical(match(x, table, nunique = 4L), c(4L, 3L, NA_integer_))
})

test_that("%in%.integer64: nunique argument", {
  x <- as.integer64(c(1L, 3L, 5L))
  table <- as.integer64(c(2L, 4L, 3L, 1L, 3L))
  expect_identical(x %in% table, c(TRUE, TRUE, FALSE))
})

test_that("%in%.integer64: with NA values", {
  x <- as.integer64(c(1L, NA, 3L))
  table <- as.integer64(c(NA, 2L, 1L))
  expect_identical(x %in% table, c(TRUE, TRUE, FALSE))
})

test_that("duplicated, unique, table methods work", {
  x = as.integer64(1:3)
  expect_identical(duplicated(x), rep(FALSE, 3L))
  expect_identical(unique(x), x)
  expect_identical(table(x), table(x = 1:3))

  x = as.integer64(rep(1L, 3L))
  expect_identical(duplicated(x), c(FALSE, TRUE, TRUE))
  expect_identical(unique(x), x[1L])
  expect_identical(table(x), table(x = rep(1L, 3L)))

  x = as.integer64(c(1L, 2L, 1L))
  expect_identical(duplicated(x), c(FALSE, FALSE, TRUE))
  expect_identical(unique(x), x[1:2])
  expect_identical(table(x), table(x = c(1L, 2L, 1L)))

  x = as.integer64(c(1L, 1L, 2L))
  expect_identical(duplicated(x), c(FALSE, TRUE, FALSE))
  expect_identical(unique(x), x[c(1L, 3L)])
  expect_identical(table(x), table(x = c(1L, 1L, 2L)))

  x = c(132724613L, -2143220989L)
  expect_identical(table(x=as.integer64(x)), table(x))
  
  expect_identical(table(x, y=lim.integer64()), table(x=as.character(x), y=as.character(lim.integer64())))
  expect_identical(table(y=lim.integer64(), x), table(y=as.character(lim.integer64()), x=as.character(x)))
  
  expected_warning = "coercing argument 3 to integer64"

  expected = table(a=as.integer(c(1,1,2)), b=1:3, c=c(2, NA, 4), exclude=1, useNA="no")
  expect_warning(
    expect_identical(table(a=as.integer64(c(1,1,2)), b=1:3, c=c(2, NA, 4), exclude=1, useNA="no"),  expected),
    expected_warning, fixed=TRUE
  )

  expected = table(a=as.integer(c(1,1,2)), b=1:3, c=c(2, NA, 4), exclude=1, useNA="ifany")
  expect_warning(
    expect_identical(table(a=as.integer64(c(1,1,2)), b=1:3, c=c(2, NA, 4), exclude=1, useNA="ifany"), expected),
    expected_warning, fixed=TRUE
  )

  expected = table(a=as.integer(c(1,1,2)), b=1:3, c=c(2, NA, 4), exclude=1, useNA="always")
  expect_warning(
    expect_identical(table(a=as.integer64(c(1,1,2)), b=1:3, c=c(2, NA, 4), exclude=1, useNA="always"), expected),
    expected_warning, fixed=TRUE
  )
  
  x = as.integer64(c(1L, 1L, 2L))
  expect_error(duplicated(x, method="_unknown_"), "'arg' should be one of", fixed=TRUE)
  expect_error(unique(x, method="_unknown_"), "'arg' should be one of", fixed=TRUE)
})

test_that("different method= for duplicated, unique work", {
  x = as.integer64(c(1L, 2L, 1L))
  exp_dup = c(FALSE, FALSE, TRUE)
  exp_unq = x[1:2]

  expect_identical(duplicated(x, method="hashdup"), exp_dup)
  expect_identical(unique(x, method="hashmapuni"), exp_unq)
  expect_identical(unique(x, method="hashuni"), exp_unq)

  expect_identical(duplicated(x, method="sortorderdup"), exp_dup)
  expect_identical(unique(x, method="sortorderuni"), exp_unq)
  expect_identical(unique(x, method="sortuni"), exp_unq)

  # TODO(#58): Fix this, currently fails.
  # expect_identical(duplicated(x, method="orderdup"), exp_dup)
  expect_identical(unique(x, method="orderuni"), exp_unq)
})

test_that("more coercion works", {
  expect_identical(as.factor(as.integer64(2:4)), factor(2:4))
  expect_identical(as.ordered(as.integer64(2:4)), as.ordered(2:4))
  expect_identical(as.integer64(factor(2:11)), as.integer64(1:10)) # NB: _not_ 2:11!
})

test_that("sorting methods work", {
  x = as.integer64(c(10L, 4L, 8L))
  x_rank = c(3.0, 1.0, 2.0)
  expect_identical(rank(x), x_rank)
  expect_identical(rank(x, method="orderrnk"), x_rank)

  x = as.integer64(1:100)
  q = as.integer64(c(1L, 26L, 50L, 75L, 100L))
  expect_identical(quantile(x, names=FALSE), q)
  expect_identical(median(x), q[3L])
  names(q) = c('0%', '25%', '50%', '75%', '100%')
  expect_identical(quantile(x), q)
  expect_identical(quantile(x, 0.2, names=FALSE), as.integer64(21L))

  expect_error(quantile(x, type=7L), "only.*qtile.*supported")
  expect_error(quantile(NA_integer64_), "missing values not allowed")

  x = as.integer64(1:100)
  q = as.integer64(c(1L, 26L, 50L, 75L, 100L))
  names(q) = c('0%', '25%', '50%', '75%', '100%')
  expect_identical(qtile(x, method="sortqtl"), q)
  expect_identical(qtile(x, method="orderqtl"), q)

  x = as.integer64(c(1L, 1L, 2L, 3L, 2L, 4L))
  x_tiepos = c(1L, 2L, 3L, 5L)
  expect_identical(tiepos(x), x_tiepos)
  expect_identical(tiepos(x, method="ordertie"), x_tiepos)

  expect_error(rank(x, method="_unknown_"), "'arg' should be one of", fixed=TRUE)
  expect_error(qtile(x, method="_unknown_"), "'arg' should be one of", fixed=TRUE)
  expect_error(tiepos(x, method="_unknown_"), "'arg' should be one of", fixed=TRUE)
})

test_that("missing and empty inputs to median() are handled correctly", {
  expect_identical(median(NA_integer64_, na.rm=FALSE), NA_integer64_)
  expect_identical(median(NA_integer64_, na.rm=TRUE), NA_integer64_)
  expect_identical(median(integer64()), NA_integer64_)

  x = as.integer64(c(NA, 1L))
  expect_identical(median(x, na.rm=FALSE), NA_integer64_)
  expect_identical(median(x, na.rm=TRUE), as.integer64(1L))

  x = as.integer64(c(NA, NA))
  expect_identical(median(x, na.rm=FALSE), NA_integer64_)
  expect_identical(median(x, na.rm=TRUE), NA_integer64_)
})

# These tests were previously kept as tests under \examples{\dontshow{...}}.
#   Converted to "proper" unit tests for clarity, after making them more
#   canonical within {testthat}, e.g. better capturing expected warnings,
#   changing stopifnot(identical(...)) to expect_identical(...).
test_that("Old \\dontshow{} tests continue working", {
  xi = c(1L, 1L, 2L)
  xi64 = as.integer64(xi)
  yi = c(3L, 4L, 4L)
  yi64 = as.integer64(yi)

  t_xi = table(x=xi)
  t_xi_yi = table(x=xi, y=yi)

  expect_identical(table(x=xi64), t_xi)
  expect_identical(table(x=xi64, y=yi64), t_xi_yi)

  expect_identical(table(x=xi64, y=yi), t_xi_yi)
  expect_identical(table(x=xi, y=yi64), t_xi_yi)

})

test_that("unipos() works as intended", {
  x = as.integer64(c(1L, 2L, 1L, 3L, 2L, 4L))
  x_unipos = c(1L, 2L, 4L, 6L)
  expect_identical(unipos(x), x_unipos)
  expect_identical(unipos(x, method="hashupo"), x_unipos)
  expect_identical(unipos(x, method="sortorderupo"), x_unipos)
  expect_identical(unipos(x, method="orderupo"), x_unipos)
  expect_error(unipos(x, method="_unknown_"), "'arg' should be one of", fixed=TRUE)
})

test_that("keypos() works as intended", {
  x = as.integer64(c(5L, 2L, 5L, 3L, 2L, 4L))
  x_keypos = c(4L, 1L, 4L, 2L, 1L, 3L)
  expect_identical(keypos(x), x_keypos)
  expect_identical(keypos(x, method="orderkey"), x_keypos)
  expect_error(keypos(x, method="_unknown_"), "'arg' should be one of", fixed=TRUE)
})

test_that("summary() works as intended", {
  x = as.integer64(c(1L, 2L, 10L, 20L, NA, 30L))
  # NB: as.integer64() strips names, so as.integer64(c(Min. = ...)) won't work
  x_summary = as.integer64(c(1L, 2L, 10L, 12L, 20L, 30L, 1L))
  names(x_summary) = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "NA's")
  expect_identical(summary(x), x_summary)
  expect_identical(summary(x[-5L]), x_summary[-7L])
})

test_that("prank() works as intended", {
  x = as.integer64(1:100)
  expect_identical(prank(x), (x-1.0)/99.0)
  expect_identical(prank(x[1L]), NA_integer64_)
})

test_that("match.integer64 with method='orderpos' fails due to bug", {
  x <- as.integer64(1:5)
  table <- as.integer64(3:7)
  expect_error(match(x, table, method="orderpos"), "object 's' not found", fixed=TRUE)
})

test_that("match.integer64 with partial cache triggers fallback", {
  x <- as.integer64(1:5)
  table <- as.integer64(3:7)

  sortcache(table) # creates 'sort' and 'nunique' in cache

  # This should fallback to hashpos/hashrev logic.
  # x is small, table is small.
  expect_identical(match(x, table), c(NA_integer_, NA_integer_, 1L, 2L, 3L))

  remcache(table)
})

test_that("match.integer64 and %in% cover various cache states", {
  x = as.integer64(c(1L, 3L, 5L))
  table = as.integer64(c(2L, 4L, 3L, 1L))

  # Cover hashpos via cache selection
  hashcache(table)
  expect_identical(match(x, table), c(4L, 3L, NA_integer_))
  expect_identical(x %in% table, c(TRUE, TRUE, FALSE))
  remcache(table)

  # Cover sortorderpos/sortfin via cache selection
  sortordercache(table)
  expect_identical(match(x, table), c(4L, 3L, NA_integer_))
  expect_identical(x %in% table, c(TRUE, TRUE, FALSE))
  remcache(table)

  # Cover orderfin via cache selection (used by %in%)
  ordercache(table)
  expect_identical(x %in% table, c(TRUE, TRUE, FALSE))
  # Cover orderpos logic if possible via automatic selection
  # Note: explicit method="orderpos" has known issues in other tests,
  # but this checks the automatic selection path based on cache existence.
  remcache(table)

  # Explicitly trigger hashrev with cache (requires cache on x)
  hashcache(x)
  expect_identical(match(x, table, method="hashrev"), c(4L, 3L, NA_integer_))
  remcache(x)
})

test_that("duplicated.integer64 covers various cache states", {
  x = as.integer64(c(1L, 2L, 1L))
  res = c(FALSE, FALSE, TRUE)

  # Cover sortorderdup with existing cache (retrieving sort/order from cache)
  sortordercache(x)
  expect_identical(duplicated(x), res)
  # Force specific method to ensure cache path is used
  expect_identical(duplicated(x, method="sortorderdup"), res)
  remcache(x)

  # Cover hashdup with existing cache
  hashcache(x)
  expect_identical(duplicated(x), res)
  remcache(x)

  # Cover orderdup with existing cache
  ordercache(x)
  expect_identical(duplicated(x), res)
  expect_identical(duplicated(x, method="orderdup"), res)
  remcache(x)
})

test_that("unique.integer64 covers various cache states and order arguments", {
  x = as.integer64(c(3L, 1L, 3L))

  # order="original" + hashcache
  hashcache(x)
  expect_identical(unique(x, order="original"), x[1:2])
  remcache(x)

  # order="original" + ordercache
  ordercache(x)
  expect_identical(unique(x, order="original"), x[1:2])
  remcache(x)

  # order="original" + sortordercache
  sortordercache(x)
  expect_identical(unique(x, order="original"), x[1:2])
  remcache(x)

  # order="values" + sortcache (triggers sortuni from cache)
  sortcache(x)
  expect_identical(unique(x, order="values"), as.integer64(c(1L, 3L)))
  remcache(x)

  # order="values" + hashcache (triggers hashuni if nunique < length/2)
  x2 = as.integer64(c(1, 1, 1, 1, 1))
  hashcache(x2)
  expect_identical(unique(x2, order="values"), as.integer64(1L))
  remcache(x2)

  # order="any" + hashcache
  hashcache(x)
  res = unique(x, order="any")
  expect_setequal(res, as.integer64(c(1L, 3L)))
  remcache(x)
})

test_that("unipos.integer64 covers various cache states", {
  x = as.integer64(c(3L, 1L, 3L))
  # positions: 1, 2

  # order="original" + hashcache
  hashcache(x)
  expect_identical(unipos(x, order="original"), c(1L, 2L))
  remcache(x)

  # order="values" + sortcache (triggers sortorderupo)
  sortcache(x)
  expect_identical(unipos(x, order="values"), c(2L, 1L))
  remcache(x)

  # order="any" + sortordercache
  sortordercache(x)
  res = unipos(x, order="any")
  expect_true(setequal(res, c(1L, 2L)))
  remcache(x)
})

test_that("table.integer64 covers inputs, cache states, and return types", {
  x = as.integer64(c(1L, 2L, 1L))

  # List input handling
  t_list = table(list(x))
  t_vec = table(x)
  expect_identical(as.vector(t_list), as.vector(t_vec))
  expect_identical(dim(t_list), dim(t_vec))

  # Error: length mismatch
  expect_error(table(x, as.integer64(1:2)), "all arguments must have the same length")

  # return="data.frame"
  df = table(x, return="data.frame")
  expect_identical(df, data.frame(x=as.integer64(c(1, 2)), Freq=as.integer(c(2, 1))))

  # return="list"
  lst = table(x, return="list")
  expect_identical(lst$values, as.integer64(c(1, 2)))
  # Fix: Counts are standard integer
  expect_identical(lst$counts, as.integer(c(2, 1)))

  # order="counts"
  tbl_cnt = table(x, order="counts")
  # 2 appears 1x, 1 appears 2x. Sorted by counts ascending: 2, 1.
  expect_identical(as.vector(tbl_cnt), c(1L, 2L))

  # Method selection: hashtab via cache
  hashcache(x)
  expect_identical(as.vector(table(x)), c(2L, 1L))
  remcache(x)

  # Method selection: sorttab via cache
  sortcache(x)
  expect_identical(as.vector(table(x)), c(2L, 1L))
  remcache(x)

  # Method selection: ordertab via cache
  ordercache(x)
  expect_identical(as.vector(table(x)), c(2L, 1L))
  remcache(x)

  # Cross-tabulation coverage
  y = as.integer64(c(1L, 2L, 1L))
  t2 = table(x, y, return="data.frame")
  # Unique pairs are (1,1) and (2,2) --> 2 rows
  expect_identical(nrow(t2), 2L)

  # Potential overflow check for combinations > 2^63
  # We construct a list of many small vectors.
  args = rep(list(as.integer64(1:2)), 65)
  # Suppress warning about overflow ("NAs produced by integer64 overflow")
  #   to verify the explicit stop error cleanly.
  expect_error(suppressWarnings(do.call(table, args)), "attempt to make a table from more than")
})
