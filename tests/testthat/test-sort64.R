test_that("order basics work", {
  x = as.integer64(c(2L, 4L, 3L))
  expect_identical(order(x), c(1L, 3L, 2L))
  expect_identical(order(x, decreasing=TRUE), c(2L, 3L, 1L))

  x = c(x, NA_integer64_)
  expect_identical(order(x), c(1L, 3L, 2L, 4L))
  expect_identical(order(x, decreasing=TRUE), c(2L, 3L, 1L, 4L))
  expect_identical(order(x, na.last=FALSE), c(4L, 1L, 3L, 2L))
  expect_identical(order(x, na.last=FALSE, decreasing=TRUE), c(4L, 2L, 3L, 1L))
})

# adapted from old if(FALSE) region which used 10000000L to benchmark
local({
  withr::local_seed(348594L)

  x <- as.integer64(c(sample.int(10L), NA))
  sortordercache(x)

  with_parameters_test_that(
    "ramorder and sortordercache work for na.last={na.last}, decreasing={decreasing}",
    expect_identical(
      order(x, na.last=na.last, decreasing=decreasing),
      {
        xo = seq_along(x)
        bit::ramorder(x, xo, na.last=na.last, decreasing=decreasing)
        xo
      }
    ),
    .cases = expand.grid(na.last = c(FALSE, TRUE), decreasing = c(FALSE, TRUE))
  )
})

with_parameters_test_that(
  "sorting methods for integer64 work",
  {
    withr::local_options(list(bit64.warn.exported.s3.method = FALSE))
    x = as.integer64(1:10)

    na_entries = rep(NA_integer64_, n_missing)
    y = sample(c(x, if (duplicates) x[1L], na_entries))
    expect_identical(sort_function(y, decreasing=decreasing, na.last=na.last), n_missing)
    # TODO(#154): Drop explicit 'else' branches
    expected_value = c(
      if (na.last) integer64() else na_entries,
      if (duplicates && !decreasing) x[1L],
      if (decreasing) rev(x) else x,
      if (duplicates && decreasing) x[1L],
      if (na.last) na_entries else integer64()
    )
    expect_identical(y, expected_value)
  },
  .cases = expand.grid(
    sort_function = list(
      bit::mergesort,
      bit::quicksort,
      bit::radixsort,
      bit::ramsort,
      bit::shellsort
    ),
    na.last = c(FALSE, TRUE),
    decreasing = c(FALSE, TRUE),
    duplicates = c(FALSE, TRUE),
    n_missing = 0:2
  )
)

with_parameters_test_that(
  "order methods for integer64 work",
  {
    withr::local_options(list(bit64.warn.exported.s3.method = FALSE))
    x = as.integer64(1:10)

    na_entries = rep(NA_integer64_, n_missing)
    y = sample(c(x, if (duplicates) x[1L], na_entries))
    i = seq_along(y)
    expect_identical(order_function(y, i, decreasing=decreasing, na.last=na.last), n_missing)
    # TODO(#154): Drop explicit 'else' branches
    expected_value = c(
      if (na.last) integer64() else na_entries,
      if (duplicates && !decreasing) x[1L],
      if (decreasing) rev(x) else x,
      if (duplicates && decreasing) x[1L],
      if (na.last) na_entries else integer64()
    )
    expect_identical(y[i], expected_value)
  },
  .cases = expand.grid(
    order_function = list(
      bit::mergeorder,
      bit::quickorder,
      bit::radixorder,
      bit::ramorder,
      bit::shellorder
    ),
    na.last = c(FALSE, TRUE),
    decreasing = c(FALSE, TRUE),
    duplicates = c(FALSE, TRUE),
    n_missing = 0:2
  )
)

with_parameters_test_that(
  "sortorder methods for integer64 work",
  {
    withr::local_options(list(bit64.warn.exported.s3.method = FALSE))
    x = as.integer64(1:10)

    na_entries = rep(NA_integer64_, n_missing)
    y = sample(c(x, if (duplicates) x[1L], na_entries))
    i = seq_along(y)
    expect_identical(sortorder_function(y, i, decreasing=decreasing, na.last=na.last), n_missing)
    # TODO(#154): Drop explicit 'else' branches
    expected_value = c(
      if (na.last) integer64() else na_entries,
      if (duplicates && !decreasing) x[1L],
      if (decreasing) rev(x) else x,
      if (duplicates && decreasing) x[1L],
      if (na.last) na_entries else integer64()
    )
    # TODO(#159): Also add expectations for the update to i
    expect_identical(y, expected_value,
      info=sprintf(
        "(na.last, decreasing, duplicates, n_missing)=(%s, %s, %s, %d)",
        na.last, decreasing, duplicates, n_missing
      )
    )
  },
  .cases = expand.grid(
    sortorder_function = list(
      bit::mergesortorder,
      bit::quicksortorder,
      bit::radixsortorder,
      bit::ramsortorder,
      bit::shellsortorder
    ),
    na.last = c(FALSE, TRUE),
    decreasing = c(FALSE, TRUE),
    duplicates = c(FALSE, TRUE),
    n_missing = 0:2
  )
)

test_that("Explicit algorithm dispatch hits C-level fallbacks and edge cases", {
  # The C code contains specific implementations for shellsort, mergesort, and
  # quicksort. ramsort() dispatches based on heuristics, so we must call the
  # exported algorithm-specific functions directly to ensure coverage of
  # specific C routines (like ram_integer64_sortmerge_desc).

  # We use a vector size likely to trigger partition loops but small enough to
  # be manageable.
  set.seed(42)
  x_base = as.integer64(sample.int(1000L, 500L))
  x_dups = as.integer64(sample(c(1:5, NA), 500L, replace=TRUE)) # High duplicate count for pivot logic

  # --- Target: ram_integer64_sortmerge_desc (Image 1) ---
  # These lines are only hit when mergesort is forced into descending mode.
  # ramsort only selects mergesort for small vectors (< 2048), but we force it here.

  x = bit::clone(x_base)
  expect_identical(
    bit::mergesort(x, decreasing=TRUE),
    0L # No NAs
  )
  expect_identical(x, as.integer64(sort(as.integer(x_base), decreasing=TRUE)))

  # Verify mergesortorder and mergeorder descending paths
  x = bit::clone(x_base)
  i = seq_along(x)
  bit::mergesortorder(x, i, decreasing=TRUE)
  expect_identical(x, as.integer64(sort(as.integer(x_base), decreasing=TRUE)))

  x = bit::clone(x_base)
  i = seq_along(x)
  bit::mergeorder(x, i, decreasing=TRUE)
  expect_identical(x[i], as.integer64(sort(as.integer(x_base), decreasing=TRUE)))

  # --- Target: ram_integer64_quicksort_..._intro (Shellsort fallback) ---
  # The C code switches from Quicksort to Shellsort if recursion depth (restlevel)
  # is exhausted. We force this by setting restlevel=0.

  x = bit::clone(x_base)
  # restlevel=0 forces immediate switch to ram_integer64_shellsort_asc/desc
  bit::quicksort(x, restlevel=0L)
  expect_identical(x, as.integer64(sort(as.integer(x_base))))

  x = bit::clone(x_base)
  bit::quicksort(x, decreasing=TRUE, restlevel=0L)
  expect_identical(x, as.integer64(sort(as.integer(x_base), decreasing=TRUE)))

  # --- Target: Quicksort Partitioning Loops (Image 2 & 3) ---
  # The 'no_sentinels' loops (lines 790+) have complex break conditions.
  # We need to test:
  # 1. Already sorted data (triggers specific pivot movements)
  # 2. Reverse sorted data
  # 3. All duplicates (triggers the "j<=i" breaks in the partition loops)

  # Case: Sorted input
  x_sorted = as.integer64(1:100)
  x = bit::clone(x_sorted)
  bit::quicksort(x, optimize="memory") # Force quicksort
  expect_identical(x, x_sorted)

  # Case: Reverse sorted input (Descending sort on Ascending data and vice versa)
  x_rev = as.integer64(100:1)
  x = bit::clone(x_rev)
  bit::quicksort(x, decreasing=FALSE, optimize="memory")
  expect_identical(x, x_sorted)

  # Case: All duplicates (High stress on partitioning equal values)
  x_all_dups = as.integer64(rep(10L, 50))
  x = bit::clone(x_all_dups)
  bit::quicksort(x, optimize="memory")
  expect_identical(x, x_all_dups)

  # Case: High duplicates with NAs (verify pivot selection with NAs present)
  x = bit::clone(x_dups)
  bit::quicksort(x, optimize="memory", has.na=TRUE)
  expect_true(bit::is.sorted(x))
})

test_that("Specific sortorder/order variants for Quicksort coverage", {
  # Covering ram_integer64_quickorderpart_... and insertion fallbacks
  x_base = as.integer64(sample(100, 50))

  # quicksortorder descending
  x = bit::clone(x_base)
  i = seq_along(x)
  bit::quicksortorder(x, i, decreasing=TRUE)
  expect_identical(x, as.integer64(sort(as.integer(x_base), decreasing=TRUE)))

  # quickorder descending (modifies i, not x)
  x = bit::clone(x_base)
  i = seq_along(x)
  bit::quickorder(x, i, decreasing=TRUE)
  expect_identical(x[i], as.integer64(sort(as.integer(x_base), decreasing=TRUE)))
})

test_that("Shellsort direct invocation", {
  # While quicksort falls back to shellsort, we can also invoke it directly
  # to ensure the 'shellsort_desc' and 'shellsortorder' paths are clean.
  x_base = as.integer64(sample(100, 50))

  x = bit::clone(x_base)
  bit::shellsort(x, decreasing=TRUE)
  expect_identical(x, as.integer64(sort(as.integer(x_base), decreasing=TRUE)))

  x = bit::clone(x_base)
  i = seq_along(x)
  bit::shellorder(x, i, decreasing=FALSE)
  expect_identical(x[i], as.integer64(sort(as.integer(x_base))))
})

test_that("Corner cases for partitioning logic", {
  # Single element and empty vectors often trip up "do { ... } while" or sentinel loops

  # TODO(#220): restore this.
  # Case 1: Empty
  # x_empty = integer64()
  # x = bit::clone(x_empty)
  # # bit::quicksort returns the NA count (0L), and modifies 'x' in-place
  # expect_identical(bit::quicksort(x), 0L) 
  # expect_identical(x, x_empty)

  # Case 2: Single Element
  x_single = as.integer64(1L)
  x = bit::clone(x_single)
  expect_identical(bit::quicksort(x), 0L)
  expect_identical(x, x_single)

  # Case 3: Pair (swap required)
  x_pair = as.integer64(c(2L, 1L))
  x = bit::clone(x_pair)
  expect_identical(bit::quicksort(x), 0L)
  expect_identical(x, as.integer64(c(1L, 2L)))
})

test_that("Radix sort variations cover all byte-shuffling paths", {
  set.seed(42)
  # Radix sort implementation has specific loops for different pass counts (w).
  # By varying radixbits, we force different numbers of passes (64/1 = 64 passes, 64/16 = 4 passes).
  # This covers the loop logic for LSB (w=0), middle bytes, and MSB.

  # Use a vector with enough bits set to matter
  x_base = as.integer64(sample(c(1:100, 2^30, 2^60), 200, replace = TRUE))

  for (rb in c(1L, 2L, 4L, 8L, 16L)) {
    # 1. radixsort
    x = clone(x_base)
    bit::radixsort(x, radixbits = rb)
    expect_identical(x, as.integer64(sort(x_base)), 
                     info = paste("radixsort with bits:", rb))

    # 2. radixorder
    x = clone(x_base)
    i = seq_along(x)
    bit::radixorder(x, i, radixbits = rb)
    expect_identical(x[i], as.integer64(sort(x_base)), 
                     info = paste("radixorder with bits:", rb))

    # 3. radixsortorder
    x = clone(x_base)
    i = seq_along(x)
    bit::radixsortorder(x, i, radixbits = rb)
    expect_identical(x, as.integer64(sort(x_base)), 
                     info = paste("radixsortorder with bits:", rb))
  }
})

test_that("Quicksort recursion limits and small-array fallbacks", {
  # Targets the switch from Quicksort -> Shellsort (restlevel=0)
  # and Quicksort -> Insertion Sort (small N) for all variants.

  x_small = as.integer64(sample(15)) # Small enough to trigger insertion sort fallback
  x_large = as.integer64(sample(100))

  # --- Force Shellsort Fallback (restlevel=0) for Order/SortOrder variants ---

  # quicksortorder (Ascending)
  x = clone(x_large)
  i = seq_along(x)
  bit::quicksortorder(x, i, restlevel = 0L)
  expect_identical(x, as.integer64(sort(x_large)))

  # quicksortorder (Descending)
  x = clone(x_large)
  i = seq_along(x)
  bit::quicksortorder(x, i, decreasing = TRUE, restlevel = 0L)
  expect_identical(x, as.integer64(sort(x_large, decreasing = TRUE)))
  
  # quickorder (Ascending)
  x = clone(x_large)
  i = seq_along(x)
  bit::quickorder(x, i, restlevel = 0L)
  expect_identical(x[i], as.integer64(sort(x_large)))

  # quickorder (Descending)
  x = clone(x_large)
  i = seq_along(x)
  bit::quickorder(x, i, decreasing = TRUE, restlevel = 0L)
  expect_identical(x[i], as.integer64(sort(x_large, decreasing = TRUE)))

  # --- Force Insertion Sort Fallback (Small N) ---
  # Standard ramsort might dispatch small arrays to mergesort, so we must 
  # call quicksort* routines explicitly to hit the "quicksort -> insertion" fallback path.

  # quicksort (Ascending/Descending)
  x = clone(x_small)
  bit::quicksort(x)
  expect_identical(x, as.integer64(sort(x_small)))

  x = clone(x_small)
  bit::quicksort(x, decreasing = TRUE)
  expect_identical(x, as.integer64(sort(x_small, decreasing = TRUE)))

  # quicksortorder (Ascending/Descending)
  x = clone(x_small)
  i = seq_along(x)
  bit::quicksortorder(x, i)
  expect_identical(x, as.integer64(sort(x_small)))

  x = clone(x_small)
  i = seq_along(x)
  bit::quicksortorder(x, i, decreasing = TRUE)
  expect_identical(x, as.integer64(sort(x_small, decreasing = TRUE)))
})

test_that("Median-of-3 index selection logic", {
  # The median selection for indices involves nested ternary operators.
  # We test all permutations of a 3-element vector to ensure every branch is taken.

  perms = list(
    c(1, 2, 3), c(1, 3, 2), c(2, 1, 3), 
    c(2, 3, 1), c(3, 1, 2), c(3, 2, 1)
  )

  for (p in perms) {
    x_base = as.integer64(p)

    # Test quickorder specifically as it relies heavily on index comparisons
    x = clone(x_base)
    i = seq_along(x)
    bit::quickorder(x, i)
    expect_identical(x[i], as.integer64(1:3))
  }
})

test_that("Explicit stop conditions in partitioning scanners", {
  # To hit the 'break' statements inside the partitioning loops, we need
  # cases where scanners might run past bounds if not checked, or extreme pivot cases.
  # Sorted, Reverse Sorted, and All-Equal vectors are the standard stressors here.

  cases = list(
    sorted = as.integer64(1:50),
    rev_sorted = as.integer64(50:1),
    all_equal = as.integer64(rep(10, 50))
  )

  for (nm in names(cases)) {
    val = cases[[nm]]

    # Test descending quicksort (explicit coverage gap from report)
    x = clone(val)
    bit::quicksort(x, decreasing = TRUE)
    expect_identical(x, as.integer64(sort(val, decreasing = TRUE)), info = nm)

    # Test quicksortorder descending
    x = clone(val)
    i = seq_along(x)
    bit::quicksortorder(x, i, decreasing = TRUE)
    expect_identical(x, as.integer64(sort(val, decreasing = TRUE)), info = nm)
  }
})
