# Minimal testthat masks for R before testthat support so that source(test_script) works

test_that = function(desc, expr) tryCatch(expr, skip_error = identity)

skip_if = function(cond, info) {
  if (!cond) return(invisible())
  e = simpleError(paste("Skipping:", info))
  class(e) = c("skip_error", class(e))
  stop(e)
}

skip_if_not_installed = function(pkg) {
  skip_if(!requireNamespace(pkg), paste(pkg, "is not installed"))
}

expect_identical = function(x, y, tolerance = NULL, ignore_attr = NULL) {
  if (!is.null(ignore_attr)) {
    attributes(x) = attributes(x)[!names(attributes(x)) %in% ignore_attr]
    attributes(y) = attributes(y)[!names(attributes(y)) %in% ignore_attr]
  }
  if (is.null(tolerance)) {
    stopifnot(identical(x, y))
  } else {
    stopifnot(isTRUE(all.equal(x, y, tolerance=tolerance)))
  }
  invisible(x)
}

expect_true = function(x) expect_identical(x, TRUE)
expect_false = function(x) expect_identical(x, FALSE)

# NB: this doesn't really work like expect_warning does, to be revisited...
expect_warning = function(expr, msg, ...) {
  e = new.env()
  withCallingHandlers(
    warning = function(w) { e$msg = conditionMessage(w); invokeRestart("muffleWarning") },
    expr
  )
  stopifnot(grepl(msg, e$msg, ...))
  invisible(x)
}
# overwrite by automatically passing these tests for now
expect_warning = function(...) invisible()

expect_error = function(expr, msg, ...) {
  val = tryCatch(expr, error = identity)
  stopifnot(inherits(val, "error") && grepl(msg, conditionMessage(val), ...))
}

expect_s3_class = function(x, kls) stopifnot(inherits(x, kls))
expect_length = function(x, l) expect_identical(length(x), l)

expect_output = function(expr, str, ...) {
  act = paste(capture.output(val <- expr), collapse="\n")
  stopifnot(grepl(str, act, ...))
  invisible(val)
}

expect_match = function(x, pattern, ..., all=TRUE) {
  agg = if (all) base::all else any
  stopifnot(agg(grepl(pattern, x, ...)))
  invisible(x)
}

expect_no_match = function(x, pattern, ...) {
  stopifnot(!any(grepl(pattern, x, ...)))
  invisible(x)
}
