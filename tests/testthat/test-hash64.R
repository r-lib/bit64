test_that("runif64 behaves as expected", {
  withr::local_seed(3478)

  expect_identical(
    runif64(10L),
    as.integer64(c(
      "6312937654860439830", "5047107428523623805", "7829389831893364707",
      "-3641910282010306573", "4600438248413496767", "4871064969903669683",
      "2693636032523872093", "4503042760826424596", "-8860474785465525016", "-4614238549190155011"
    ))
  )

  expect_identical(
    runif64(5L, 10L, 20L),
    as.integer64(c(16L, 19L, 16L, 15L, 20L))
  )

  # large enough number to be confident the test isn't "randomly" succeeding,
  #   but not so large as to noticeably slow down the suite.
  x = runif64(100000L, -5L, 5L)
  expect_true(all(x >= -5L & x <= 5L))
})

test_that("implicit tests from ?hashmap continue working", {
  x = as.integer64(sample(c(NA, 0:9)))
  y = as.integer64(sample(c(NA, 1:9), 10, TRUE))
  hx = hashmap(x)
  hy = hashmap(y)

  expect_identical(match(as.integer(x), as.integer(y)), hashpos(hy, x))
  expect_identical(match(as.integer(x), as.integer(y)), hashrev(hx, y))
  expect_identical(as.integer(x) %in% as.integer(y), hashfin(hy, x))
  expect_identical(as.integer(x) %in% as.integer(y), hashrin(hx, y))
  expect_identical(duplicated(as.integer(y)), hashdup(hy))
  expect_identical(as.integer64(unique(as.integer(y))), hashuni(hy, keep.order=TRUE))
  expect_identical(sort(hashuni(hy, keep.order=FALSE)), sort(hashuni(hy, keep.order=TRUE)))
  expect_identical(y[hashupo(hy, keep.order=FALSE)], hashuni(hy, keep.order=FALSE))
  expect_identical(y[hashupo(hy, keep.order=TRUE)], hashuni(hy, keep.order=TRUE))
  expect_identical(hashpos(hy, hashuni(hy, keep.order=TRUE)), hashupo(hy, keep.order=TRUE))
  expect_identical(hashpos(hy, hashuni(hy, keep.order=FALSE)), hashupo(hy, keep.order=FALSE))
  expect_identical(hashuni(hy, keep.order=FALSE), hashtab(hy)$values)
  expect_identical(
    as.vector(table(as.integer(y), useNA="ifany")),
    hashtab(hy)$counts[order(hashtab(hy)$values)]
  )
  expect_identical(hashuni(hy, keep.order=TRUE), hashmapuni(y))
  expect_identical(hashupo(hy, keep.order=TRUE), hashmapupo(y))
  expect_identical(hashtab(hy), hashmaptab(y))
})

  #   require(bit64)
  #   require(microbenchmark)
  #   n <- 1000000L
  #   print(microbenchmark(runif64(n, 1.0, n), times=20L))
  #   for (m in c(1.0, 2.0, 4.0, 8.0, 16.0)) {
  #     print(microbenchmark(runif64(n, 1.0, n*m, replace=FALSE), times=20L))
  #     print(microbenchmark(sample(n*m, n, replace=FALSE), times=20L))
  #   }
  #   print(microbenchmark(runif64(n, 1.0, replace=FALSE), times=20L))


  # library(bit64)
  # n <- 10000000L
  # x <- as.integer64(sample(n, n, TRUE))
  # t1 <- system.time({h <- hashmap(x)})[3L]
  # t2 <- system.time({value <- hashuni(h)})[3L]
  # t3 <- system.time({count <- hashtab(h)})[3L]
  # t4 <- system.time({ret1 <- list(values=value, counts=count)})[3L]
  # t1+t2+t3+t4
  # system.time({ret2 <- hashmaptab(x)})[3L]
  # identical(ret1,ret2)


  # x <- as.integer64(sample(n, n, TRUE))

  # system.time({
  #   ret2 <- hashmaptab(x)
  #   cv2 <- sum(ret2$counts[ret2$counts > 1.0])
  # })[3L]

  # system.time({
  #   s <- clone(x)
  #   na.count <- ramsort(s, has.na = TRUE, na.last = FALSE, decreasing = FALSE, stable = FALSE, optimize = "time")
  #   cv <- .Call(C_r_ram_integer64_sortnut, x = s)[[2L]]
  #   })

  # cv
  # cv2


  # nunique(x)
  # length(value)
  # length(count)
  # length(t1$value)
  # length(t1$count)
  # value
  # t1
  # count

  # s <- clone(x); o <- seq_along(x); ramsortorder(s, o)
  # t2 <- sortordertab(s,o)
  # length(s)
  # length(t2)




  # library(bit64)
  # n <- 1000000L
  # r <- runif64(n, lim.integer64()[1L], lim.integer64()[2L])
  # identical(r, as.integer64(as.bitstring(r)))
  # cbind(r,as.integer64(as.bitstring(r)))
  # cbind(as.bitstring(r),as.bitstring(as.integer64(as.bitstring(r))))

  # #sum(duplicated(r))
  # #table(r)
  # #range(r)
  # log2(abs(range(r)))

  # x <- seq(0.0, 1.0, 0.1)
  # y <- quantile.integer64(r, x)
  # z <- diff(y)
  # plot(log2(z), type="b",ylim=c(0.0, max(log2(z))))


  # n <- 10000000L
  # system.time(runif(n))
  # system.time(runif64(n))

test_that("hashfun works", {
  x = as.integer64(c(1:3, 1:2))
  hf = hashfun(x)
  expect_length(hf, length(x))
  expect_true(all(hf >= 0))
  expect_identical(hf[1], hf[4])
  expect_identical(hf[2], hf[5])
})

test_that("hashmap cache dissociation error", {
  x = as.integer64(1:3)
  ch = newcache(x)
  y = as.integer64(1:3)
  expect_error(hashmap(y, cache=ch), "vector 'x' dissociated from cache")
})

test_that("runif64 replace=FALSE edge cases", {
  expect_error(
    runif64(10L, 1, 5, replace=FALSE),
    "cannot take a sample larger than the population"
  )
  
  r1 = runif64(10L, 1, 100, replace=FALSE)
  expect_length(r1, 10L)
  expect_length(unique(r1), 10L)
  expect_true(all(r1 >= 1 & r1 <= 100))
  
  r2 = runif64(5L, 1, 1000, replace=FALSE)
  expect_length(r2, 5L)
  expect_length(unique(r2), 5L)
  expect_true(all(r2 >= 1 & r2 <= 1000))
  
  r3 = runif64(20L, 1, 20, replace=FALSE)
  expect_length(r3, 20L)
  expect_length(unique(r3), 20L)
  expect_true(all(r3 >= 1 & r3 <= 20))
})

test_that("hashmap with forced collisions", {
  x = as.integer64(c(1, 2, 3, 4, 5, 6, 7))
  h = hashcache(x, hashbits=3L)
  expect_s3_class(h, "cache_integer64")
  expect_equal(getcache(x, "nunique"), 7)
  
  # Trigger collisions in various C functions
  expect_equal(hashpos(h, x), 1:7)
  expect_equal(hashrev(h, x), 1:7)
  expect_equal(hashfin(h, x), rep(TRUE, 7))
  expect_equal(hashrin(h, x), rep(TRUE, 7))
  expect_equal(hashdup(h), rep(FALSE, 7))
  expect_setequal(hashuni(h), x)
  expect_setequal(x[hashupo(h)], x)
  expect_equal(length(hashtab(h)$counts), 7)
  
  # For hashmaptab, hashmapuni, hashmapupo (they build their own hashmap)
  expect_equal(length(hashmaptab(x, hashbits=3L)$counts), 7)
  expect_setequal(hashmapuni(x, hashbits=3L), x)
  expect_setequal(x[hashmapupo(x, hashbits=3L)], x)
  
  remcache(x)
})
