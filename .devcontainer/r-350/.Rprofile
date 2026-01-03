# Minimal testthat masks for R before testthat support so that source(test_script) works

testthat_shim_env <- new.env()
patrick_shim_env <- new.env()
withr_shim_env <- new.env()

with(testthat_shim_env, {

test_that <- function(desc, code) {
  cat(sprintf("\nTEST: %s\n", desc))
  # Eval in a new environment to keep scope cleanish
  tryCatch(
    eval(substitute(code), envir = new.env(parent = parent.frame())),
    skip_error = identity
  )
}

skip_if = function(cond, info) {
  if (!cond) return(invisible())
  e = simpleError(paste("Skipping:", info))
  class(e) = c("skip_error", class(e))
  stop(e)
}

skip_if_not_installed = function(pkg) {
  skip_if(!requireNamespace(pkg), paste(pkg, "is not installed"))
}

skip_if_not = function(cond, info) skip_if(!cond, info)

skip_unless_r = function(spec) {
  msg = paste("R version requirement not satisfied:", spec)
  spec = unlist(strsplit(trimws(spec), "\\s+"))
  skip_if_not(eval(parse(text = sprintf("getRversion() %s '%s'", spec[1L], spec[2L]))), msg)
}

expect_identical = function(x, y, tolerance = NULL, ignore_attr = NULL, info = character()) {
  if (!is.null(ignore_attr)) {
    attributes(x) = attributes(x)[!names(attributes(x)) %in% ignore_attr]
    attributes(y) = attributes(y)[!names(attributes(y)) %in% ignore_attr]
  }
  if (is.null(tolerance)) {
    if (!identical(x, y)) {
      stop("x and y are not identical", if (length(info)) "\n", info)
    }
  } else {
    if (!isTRUE(all.equal(x, y, tolerance=tolerance))) {
      stop("x and y are not identical (within tolerance)", if (length(info)) "\n", info)
    }
  }
  invisible(x)
}

expect_true = function(x) expect_identical(x, TRUE)
expect_false = function(x) expect_identical(x, FALSE)

expect_warning <- function(object, regexp = NULL, ...) {
  warnings <- character()
  e <- environment()
  withCallingHandlers({
    object
  }, warning = function(w) {
    e$warnings <- c(e$warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })

  if (!length(warnings)) {
    stop("FAILURE: expect_warning() failed (no warning caught).")
  }

  if (is.null(regexp)) return(invisible())
  if (!any(grepl(regexp, warnings))) {
    stop(sprintf("FAILURE: expect_warning() regex mismatch.\n  Expected: %s\n  Actual: %s", regexp, toString(warnings)))
  }
}

expect_no_warning <- function(object, ...) {
  tryCatch({
    object
  }, warning = function(w) {
    stop(sprintf("FAILURE: expect_no_warning() failed. Caught warning: %s", conditionMessage(w)))
  })
}

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

})

with(patrick_shim_env, {

# Handles both .cases data.frame and ... vector arguments
with_parameters_test_that <- function(desc, code, .cases = NULL, .interpret_glue = TRUE, ...) {
  cat(sprintf("\nPARAM TEST: %s\n", desc))
  code_expr <- substitute(code)  
  # If .cases is not provided, build it from ... (grid expansion)
  if (is.null(.cases)) {
    args = list(...)
    args$stringsAsFactors = FALSE
    .cases <- do.call(expand.grid, args)
  }

  # Iterate over cases
  for (i in seq_len(nrow(.cases))) {
    # Create env for this specific case
    case_env <- new.env(parent = parent.frame())
    
    # Assign parameters into the environment
    row_vals <- .cases[i, , drop = FALSE]
    for (var_name in names(row_vals)) {
      assign(var_name, row_vals[[1, var_name]], envir = case_env) 
    }
    # Run the test block 
    tryCatch({
      eval(code_expr, envir = case_env)
      cat(".") # print dot for progress
    }, error = function(e) {
      cat(sprintf("\nFAILED at case %d: %s\n", i, conditionMessage(e)))
      stop(e)
    })
  }
  cat("\n")
}

})

with(withr_shim_env, {

with_options <- function(new_opts, code) {
  old_opts <- options(new_opts)
  on.exit(options(old_opts))
  force(code)
}

# local_options: Apply options, defer reset to parent frame
local_options <- function(new_opts) {
  old_opts <- options(new_opts)
  do.call(on.exit, list(substitute(options(x), list(x = old_opts)), add = TRUE), envir = parent.frame())
  invisible()
}

# local_seed: Save seed, set new seed, defer restore to parent frame
local_seed <- function(seed) {
  # Check if global seed exists
  if (exists(".Random.seed", envir = .GlobalEnv)) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv)
    # Cleanup: restore the vector to global env
    cleanup <- substitute(assign(".Random.seed", val, envir = .GlobalEnv), list(val = old_seed))
  } else {
    # Cleanup: remove the seed if it didn't exist
    cleanup <- quote(rm(".Random.seed", envir = .GlobalEnv))
  }
  
  set.seed(seed)
  do.call(on.exit, list(cleanup, add = TRUE), envir = parent.frame())
  invisible()
}

})

# Attach the shim so functions are available globally
attach(withr_shim_env); attach(testthat_shim_env); attach(patrick_shim_env)
rm(withr_shim_env, testthat_shim_env, patrick_shim_env)
