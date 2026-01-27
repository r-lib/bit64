with_parameters_test_that("sortfin works", method=1:3, {
  x = as.integer64(1:10)
  r = sample(x)
  expect_identical(sortfin(integer64(), 1:10, method=method), rep(FALSE, 10L))
  expect_identical(sortfin(x, integer64(), method=method), logical())

  expect_true(all(sortfin(x, r, method=method)))

  expect_true(sortfin(x, 1L, method=method))
  expect_true(all(sortfin(x, c(1.0, 4.0), method=method)))
  expect_identical(sortfin(x, as.integer64(0:1), method=method), c(FALSE, TRUE))
  # only 'x' need be sorted
  expect_identical(sortfin(x, as.integer64(1:0), method=method), c(TRUE, FALSE))
})

with_parameters_test_that("orderfin and orderpos work", method=1:3, {
  x = as.integer64(1:10)
  idx = seq_along(x)

  expect_identical(orderfin(x, idx, 0:1, method=method), c(FALSE, TRUE))
  expect_identical(orderfin(x, idx, as.integer64(0:1), method=method), c(FALSE, TRUE))

  expect_identical(orderpos(x, idx, 0:1, method=method), c(NA_integer_, 1L))
  expect_identical(orderpos(x, idx, as.integer64(0:1), method=method), c(NA_integer_, 1L))

  # These were segfaulting due to length(order) < length(table)
  table = as.integer64(c(10L, 20L, 30L, 5L, 15L, 25L))
  order = c(4L, 1L, 5L, 2L, 6L, 3L) # order of table is 5, 10, 15, 20, 25, 30

  partial_order = c(4L, 1L, 5L, 2L) # order of subset of table is 5, 10, 15, 20

  x_search = as.integer64(c(5L, 10L, 15L, 20L, 25L, 30L, 99L))

  # reference with full order
  full_fin = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
  full_pos = c(4L, 1L, 5L, 2L, 6L, 3L, NA_integer_)
  expect_identical(orderfin(table, order, x_search, method=method), full_fin)
  expect_identical(orderpos(table, order, x_search, method=method), full_pos)

  # with partial order, we search in a subset
  partial_fin = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  partial_pos = c(4L, 1L, 5L, 2L, NA_integer_, NA_integer_, NA_integer_)
  expect_identical(orderfin(table, partial_order, x_search, method=method), partial_fin)
  expect_identical(orderpos(table, partial_order, x_search, method=method), partial_pos)

  expect_error(
    orderfin(as.integer64(10:1), 1:3, 8:11, method=method),
    "'table' is not sorted by 'order'",
    fixed = TRUE
  )
  expect_error(
    orderpos(as.integer64(10:1), 1:3, 8:11, method=method),
    "'table' is not sorted by 'order'",
    fixed = TRUE
  )
})

test_that("ordertab and orderdup work", {
  x = as.integer64(1:10)
  x = c(x, x[1:8], x[1:6])
  idx = order(x)

  expect_identical(ordertab(x, idx, 10L), rep(3:1, c(6L, 2L, 2L)))
  expect_identical(orderdup(x, idx), rep(c(FALSE, TRUE), c(10L, 14L)))
  expect_identical(orderdup(x, idx, method=2L), rep(c(FALSE, TRUE), c(10L, 14L)))
})

test_that("ordertab handles nunique smaller than actual", {
  x = as.integer64(1:10)
  x = c(x, x[1:8], x[1:6])
  o = order(x)

  # makes operation quite slow, but in my test, this segfaulted on the first invocation every time.
  #   do this inside 'local' because doing so inside 'expect_identical' causes gctorture to apply
  #   to _every_ allocation induced by testthat as well as rep(). This isolated form is much faster.
  res = local({
    on.exit(gctorture(FALSE)); gctorture(TRUE)
    ordertab(x, o, 4L)
  })
  expect_identical(res, rep(3L, 4L))
})

test_that("sortorderdup works with both methods", {
  # sortorderdup requires a sorted vector and an order vector
  # Case 1: Simple duplicates
  x = as.integer64(c(1, 1, 2, 3, 3, 3))
  o = seq_along(x)

  # FALSE for the first occurrence, TRUE for subsequent
  expected = c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE)

  # Method 1 (Direct set, default for small N)
  expect_identical(sortorderdup(x, o, method=1L), expected)

  # Method 2 (Bitflags, usually for large N, but we force it here for coverage)
  expect_identical(sortorderdup(x, o, method=2L), expected)

  # Case 2: All duplicates
  x_all = as.integer64(c(1, 1, 1))
  o_all = seq_along(x_all)
  expect_identical(sortorderdup(x_all, o_all, method=2L), c(FALSE, TRUE, TRUE))

  # Case 3: No duplicates
  x_none = as.integer64(c(1, 2, 3))
  o_none = seq_along(x_none)
  expect_identical(sortorderdup(x_none, o_none, method=2L), c(FALSE, FALSE, FALSE))
})

test_that("sortordertab works with normalization options", {
  # sortordertab calculates frequencies based on a sorted vector
  x = as.integer64(c(1, 1, 2, 3, 3, 3))
  o = seq_along(x)

  # denormalize = FALSE: Returns counts for unique values
  # Unique values are 1, 2, 3. Counts are 2, 1, 3.
  expect_identical(sortordertab(x, o, denormalize=FALSE), c(2L, 1L, 3L))

  # denormalize = TRUE: Returns counts expanded to the original vector positions
  # Positions 1,2 (val 1) -> count 2
  # Position 3 (val 2) -> count 1
  # Positions 4,5,6 (val 3) -> count 3
  expect_identical(sortordertab(x, o, denormalize=TRUE), c(2L, 2L, 1L, 3L, 3L, 3L))

  # Edge case: Single element
  x_single = as.integer64(1)
  o_single = 1L
  expect_identical(sortordertab(x_single, o_single, denormalize=FALSE), 1L)
  expect_identical(sortordertab(x_single, o_single, denormalize=TRUE), 1L)
})

test_that("sortorderuni and sortorderupo work", {
  # These functions allow retrieving unique values or their positions from sorted inputs
  # They share similar bitflag logic in C that needs covering

  # Data setup: 
  # original: 3, 1, 2, 1, 3
  # sorted:   1, 1, 2, 3, 3
  # order:    2, 4, 3, 1, 5
  val = as.integer64(c(3, 1, 2, 1, 3))
  o = order(val)
  s = val[o]

  # sortorderuni: extract unique values using the table, sorted, and order
  # It should return unique(val) but usually in the order of appearance or sorted depending on method
  # The C implementation uses bitflags to filter duplicates.
  # The R wrapper allocates 'nunique' size.

  # We need 'nunique' for the call.
  nu = 3L

  # sortorderuni returns the unique values
  expect_identical(sortorderuni(val, s, o, nunique=nu), as.integer64(c(3, 1, 2)))

  # sortorderupo returns the positions (indices) of the unique values
  # indices in 'val': 1 (3), 2 (1), 3 (2) -> c(1, 2, 3) 
  # (Note: it picks the first occurrence in the original vector if keep.order=TRUE implicit logic matches)
  expect_identical(sortorderupo(s, o, nunique=nu, keep.order=TRUE), 1:3)
})

test_that("sortorderkey works", {
  # This targets r_ram_integer64_sortorderkey_asc
  x = as.integer64(c(10, 10, 20, 30, 30))
  o = seq_along(x)

  # Keys should be assigned sequentially: 1, 1, 2, 3, 3
  expect_identical(sortorderkey(x, o), c(1L, 1L, 2L, 3L, 3L))

  # With NA skipping (na.skip.num)
  x_na = as.integer64(c(NA, NA, 10, 20))
  o_na = seq_along(x_na)

  # If we skip 2 NAs: NA, NA, 1, 2
  # Note: NA_integer_ is used for NAs in the key vector
  expect_identical(sortorderkey(x_na, o_na, na.skip.num=2L), c(NA_integer_, NA_integer_, 1L, 2L))
})

with_parameters_test_that("quantile, median", {
    x32 = as.integer(x)
    x64 = as.integer64(x32)
    convert_x32_result_to_integer64 = function(x) {
      myRound = function(x) {res = round(x); res[x%%1 == 0.5] = ceiling(x[x%%1 == 0.5]); res}
      setNames(as.integer64(myRound(x)), names(x))
    }
    expect_identical(quantile(x64, probs=probs, na.rm=TRUE), convert_x32_result_to_integer64(quantile(x32, probs=probs, na.rm=TRUE)))
    expect_identical(median(x64, na.rm=TRUE), convert_x32_result_to_integer64(median(x32, na.rm=TRUE)))
  }, .cases = expand.grid(
    x = I(list(c(1, 5, 7, NA, 1), c(1, 5, 7, NA, 1), 1:2, 1:3, c(1, 3), c(-5, -2, 0, 2))), 
    probs = I(list(c(0, 0.25, 0.5, 0.75, 1), c(0.1, 0.6, 0.9)))
  )
)

test_that("special median", {
    expect_identical(median(as.integer64(c(1152921504606846976, 1152921504606847232))), as.integer64("1152921504606847104"))
})
