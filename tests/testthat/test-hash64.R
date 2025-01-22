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
  #   cv <- .Call(C_r_ram_integer64_sortnut, x = s, PACKAGE = "bit64")[[2L]]
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
  # #table.integer64(r)
  # #range(r)
  # log2(abs(range(r)))

  # x <- seq(0.0, 1.0, 0.1)
  # y <- quantile.integer64(r, x)
  # z <- diff(y)
  # plot(log2(z), type="b",ylim=c(0.0, max(log2(z))))


  # n <- 10000000L
  # system.time(runif(n))
  # system.time(runif64(n))

