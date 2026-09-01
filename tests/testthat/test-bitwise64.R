with_parameters_test_that(
  "bitwNot works with basic R types: ",
  {
    if (!is.na(type))
      x_cast = eval(call(paste0("as.", type), x))
    else
      x_cast = x

    if (identical(type, "integer64"))
      x_base = as.integer(x)
    else
      x_base = x_cast

    expected_result_x = tryCatch(base::bitwNot(x_base), error=identity)

    if (inherits(expected_result_x, "error")) {
      expected_result_x = conditionMessage(expected_result_x)
    } else if (identical(type, "integer64")) {
      expected_result_x = as.integer64(expected_result_x)
    }
    fun = getExportedValue("bit64", "bitwNot")
    actual_result_x = tryCatch(fun(x_cast), error=conditionMessage)

    expect_identical(actual_result_x, expected_result_x)
  },
  .cases=expand.grid(
    type=c(
      NA, "integer64", "double", "logical", "integer", "character", "complex", "factor", "ordered",
      if (getRversion() > "3.6.0") c("POSIXct", "Date")
      ),
    x=I(list(NULL, c(-(50:2), 2:50, seq(-1.0, 1.0, 0.25), NA))),
    stringsAsFactors=FALSE
  )
)

with_parameters_test_that(
  "bitwise function works with basic R types: ",
  {
    y32 = -3:3
    y64 = as.integer64(y32)
    if (!is.na(type))
      x_cast = eval(call(paste0("as.", type), x))
    else
      x_cast = x

    if (identical(type, "integer64"))
      x_base = as.integer(x)
    else
      x_base = x_cast

    fun = getExportedValue("base", func)
    expected_result_x_y32 = tryCatch(fun(x_base, y32), error=identity)
    expected_result_y32_x = tryCatch(fun(y32, x_base), error=identity)

    if (inherits(expected_result_x_y32, "error")) {
      expected_result_x_y64 = expected_result_x_y32 = conditionMessage(expected_result_x_y32)
    } else {
      expected_result_x_y64 = as.integer64(expected_result_x_y32)
      if (identical(type, "integer64")) {
        expected_result_x_y32 = as.integer64(expected_result_x_y32)
      }
    }

    if (inherits(expected_result_y32_x, "error")) {
      expected_result_y64_x = expected_result_y32_x = conditionMessage(expected_result_y32_x)
    } else {
      expected_result_y64_x = as.integer64(expected_result_y32_x)
      if (identical(type, "integer64")) {
        expected_result_y32_x = as.integer64(expected_result_y32_x)
      }
    }

    fun = getExportedValue("bit64", func)
    actual_result_x_y32 = tryCatch(fun(x_cast, y32), error=conditionMessage)
    actual_result_y32_x = tryCatch(fun(y32, x_cast), error=conditionMessage)
    actual_result_x_y64 = tryCatch(fun(x_cast, y64), error=conditionMessage)
    actual_result_y64_x = tryCatch(fun(y64, x_cast), error=conditionMessage)

    expect_identical(actual_result_x_y32, expected_result_x_y32)
    expect_identical(actual_result_y32_x, expected_result_y32_x)
    expect_identical(actual_result_x_y64, expected_result_x_y64)
    expect_identical(actual_result_y64_x, expected_result_y64_x)
  },
  .cases=expand.grid(
    func=c("bitwAnd", "bitwOr", "bitwXor"),
    type=c(
      NA, "integer64", "double", "logical", "integer", "character", "complex", "factor", "ordered",
      if (getRversion() > "3.6.0") c("POSIXct", "Date")
      ),
    x=I(list(NULL, c(-(50:2), 2:50, seq(-1.0, 1.0, 0.25), NA))),
    stringsAsFactors=FALSE
  )
)

with_parameters_test_that(
  "bitwise shift function works with basic R types: ",
  {
    y32 = -3:3
    y64 = as.integer64(y32)
    if (!is.na(type))
      x_cast = eval(call(paste0("as.", type), x))
    else
      x_cast = x

    if (identical(type, "integer64"))
      x_base = as.integer(x)
    else
      x_base = x_cast

    fun = getExportedValue("base", func)
    expected_result_x_y32 = tryCatch(fun(x_base, y32), error=identity)
    expected_result_y32_x = tryCatch(fun(y32, x_base), error=identity)

    if (inherits(expected_result_x_y32, "error")) {
      expected_result_x_y64 = expected_result_x_y32 = conditionMessage(expected_result_x_y32)
    } else if (identical(type, "integer64")) {
      expected_result_x_y64 = as.integer64(expected_result_x_y32)
      expected_result_x_y32 = as.integer64(expected_result_x_y32)
    } else {
      expected_result_x_y64 = expected_result_x_y32
    }

    if (inherits(expected_result_y32_x, "error")) {
      expected_result_y64_x = expected_result_y32_x = conditionMessage(expected_result_y32_x)
    } else {
      expected_result_y64_x = as.integer64(expected_result_y32_x)
      # expected_result_y32_x stays 32-bit integer
    }

    # bitwShiftR performs logical (zero-fill) right shifts. For negative numbers (x < 0),
    # sign extension sets bits 32-63 to 1s in integer64, which shift down into the lower
    # 64 bits. base::bitwShiftR operates only in 32-bit space, so we add (2^32 - 1) << (32 - shift)
    # to reconstruct the expected 64-bit result from the 32-bit reference answer.
    if (func == "bitwShiftR" && !is.null(x_base)) {
      if ((is.integer64(expected_result_x_y32) && length(expected_result_x_y32)) ||
          (is.integer64(expected_result_x_y64) && length(expected_result_x_y64))) {
        x_int = suppressWarnings(as.integer(x_base))
        shiftOffset = bitwShiftL(as.integer64(2L)^32L - 1L, 32L - y32)
        idx = which(!is.na(x_int) & x_int < 0L & y32 != 0L)
        if (length(idx)) {
          # TODO(R>=4.0.0): just use rep_len
          # nolint next: rep_len_linter.
          offset = rep(shiftOffset, length.out = max(length(x_int), length(y32)))[idx]
          oldClass(offset) = "integer64"
          if (is.integer64(expected_result_x_y32) && length(expected_result_x_y32))
            expected_result_x_y32[idx] = expected_result_x_y32[idx] + offset
          if (is.integer64(expected_result_x_y64) && length(expected_result_x_y64))
            expected_result_x_y64[idx] = expected_result_x_y64[idx] + offset
        }
      }

      if ((is.integer64(expected_result_y32_x) && length(expected_result_y32_x)) ||
          (is.integer64(expected_result_y64_x) && length(expected_result_y64_x))) {
        x_int = suppressWarnings(as.integer(x_base))
        shiftOffset = bitwShiftL(as.integer64(2L)^32L - 1L, 32L - x_int)
        idx = which(!is.na(x_int) & y32 < 0L & x_int != 0L)
        if (length(idx)) {
          # TODO(R>=4.0.0): just use rep_len
          # nolint next: rep_len_linter.
          offset = rep(shiftOffset, length.out = max(length(y32), length(x_int)))[idx]
          oldClass(offset) = "integer64"
          if (is.integer64(expected_result_y32_x) && length(expected_result_y32_x))
            expected_result_y32_x[idx] = expected_result_y32_x[idx] + offset
          if (is.integer64(expected_result_y64_x) && length(expected_result_y64_x))
            expected_result_y64_x[idx] = expected_result_y64_x[idx] + offset
        }
      }
    }

    fun = getExportedValue("bit64", func)
    actual_result_x_y32 = tryCatch(fun(x_cast, y32), error=conditionMessage)
    actual_result_y32_x = tryCatch(fun(y32, x_cast), error=conditionMessage)
    actual_result_x_y64 = tryCatch(fun(x_cast, y64), error=conditionMessage)
    actual_result_y64_x = tryCatch(fun(y64, x_cast), error=conditionMessage)

    expect_identical(actual_result_x_y32, expected_result_x_y32)
    expect_identical(actual_result_y32_x, expected_result_y32_x)
    expect_identical(actual_result_x_y64, expected_result_x_y64)
    expect_identical(actual_result_y64_x, expected_result_y64_x)
  },
  .cases=expand.grid(
    func=c("bitwShiftL", "bitwShiftR"),
    type=c(
      NA, "integer64", "double", "logical", "integer", "character", "complex", "factor", "ordered",
      if (getRversion() > "3.6.0") c("POSIXct", "Date")
      ),
    x=I(list(NULL, c(-(10:2), 2:10, seq(-1.0, 1.0, 0.25), NA))),
    stringsAsFactors=FALSE
  )
)

test_that("bitwise functions work in integer64 range", {
  expect_identical(bitwShiftL(as.integer64(1L), 62L), as.integer64(2L)^62L)
  expect_identical(bitwShiftL(as.integer64(-1L), 62L), -as.integer64(2L)^62L)
  expect_identical(bitwShiftL(as.integer64(1L), 63:70), rep(NA_integer64_, 8L))
  expect_identical(bitwShiftL(as.integer64(-1L), 63:70), rep(NA_integer64_, 8L))

  expect_identical(bitwShiftR(as.integer64(1L), 63:70), c(as.integer64(0L), rep(NA_integer64_, 7L)))
  expect_identical(bitwShiftR(as.integer64(-1L), 63:70), c(as.integer64(1L), rep(NA_integer64_, 7L)))
})
