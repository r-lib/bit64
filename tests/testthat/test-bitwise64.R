with_parameters_test_that(
  "bitwNot works with basic R types: ",
  {
    if (!is.na(type))
      eval(parse(text=paste0("x_cast = as.", type, "(x)")))
    else
      x_cast = x
    
    if (!is.na(type) && type == "integer64")
      x = as.integer(x)
    else
      x = x_cast

    fun = base::bitwNot
    expected_result_x = tryCatch(fun(x), error=conditionMessage)

    if (!is.na(type) && type == "integer64" && !is.character(expected_result_x)) {
      expected_result_x = as.integer64(expected_result_x)
    }
    fun = bitwNot
    actual_result_x = tryCatch(fun(x_cast), error=conditionMessage)

    expect_identical(actual_result_x, expected_result_x)
  },
  .cases=expand.grid(
    type=c(
      NA, "integer64", "double", "logical", "integer", "character", "complex", "factor", "ordered",
      if (getRversion() > "3.6.0") c("POSIXct", "Date")
      ),
    x=I(list(NULL, c(-(50:2), 2:50, seq(-1, 1, 0.25), NA))),
    stringsAsFactors=FALSE
  )
)

with_parameters_test_that(
  "bitwise function works with basic R types: ",
  {
    y32 = -3:3
    y64 = as.integer64(y32)
    if (!is.na(type))
      eval(parse(text=paste0("x_cast = as.", type, "(x)")))
    else
      x_cast = x
    
    if (!is.na(type) && type == "integer64")
      x = as.integer(x)
    else
      x = x_cast

    fun = get(func, baseenv())
    expected_result_x_y32 = tryCatch(fun(x, y32), error=conditionMessage)
    expected_result_y32_x = tryCatch(fun(y32, x), error=conditionMessage)
    expected_result_x_y64 = tryCatch(as.integer64(fun(x, y32)), error=conditionMessage)
    expected_result_y64_x = tryCatch(as.integer64(fun(y32, x)), error=conditionMessage)
    if (!is.na(type) && type == "integer64" && !is.character(expected_result_x_y32)) {
      expected_result_x_y32 = as.integer64(expected_result_x_y32)
      expected_result_y32_x = as.integer64(expected_result_y32_x)
    }
      
    fun = get(func)
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
    x=I(list(NULL, c(-(50:2), 2:50, seq(-1, 1, 0.25), NA))),
    stringsAsFactors=FALSE
  )
)

my_if_else = function(test, yes, no, na=no) {
  if (class(yes)[1L] != class(no)[1L])
    stop("'yes' and 'no' must have the same type")
  na = eval(substitute(na))
  if (!is.null(na) && class(na)[1L] != class(yes)[1L])
    stop("'na' must have the same type as 'yes' and 'no'")
  l = max(length(test), length(yes), length(no))
  ret = rep_len(no, l)
  test = rep_len(as.logical(test), l)
  if (!is.null(na)) {
    ret[!is.na(test) & test] = rep_len(yes, l)[!is.na(test) & test]
    ret[is.na(test)] = rep_len(na, l)[is.na(test)]
  } else {
    ret[test] = rep_len(yes, l)[test]
  }
  ret
}

# with_parameters_test_that(
#   "bitwise shift function works with basic R types: ",
#   {
#     y32 = -3:3
#     y64 = as.integer64(y32)
#     if (!is.na(type))
#       eval(parse(text=paste0("x_cast = as.", type, "(x)")))
#     else
#       x_cast = x
    
#     if (!is.na(type) && type == "integer64")
#       x = as.integer(x)
#     else
#       x = x_cast

#     fun = get(func, baseenv())
#     expected_result_x_y32 = tryCatch(fun(x, y32), error=conditionMessage)
#     expected_result_y32_x = tryCatch(fun(y32, x), error=conditionMessage)
#     expected_result_x_y64 = tryCatch(fun(x, y32), error=conditionMessage)
#     expected_result_y64_x = tryCatch(as.integer64(fun(y32, x)), error=conditionMessage)
#     if (!is.na(type) && type == "integer64" && !is.character(expected_result_x_y32)) {
#       expected_result_x_y32 = as.integer64(expected_result_x_y32)
#       expected_result_x_y64 = as.integer64(expected_result_x_y64)
#     }
#     # because of the way bitwShiftR is defined, it shifts based on unsigned integers
#     if (func == "bitwShiftR" && !is.null(x)) {
#       shiftOffset = bitwShiftL(as.integer64(2L)^32L - 1L, 32L - y32)
#       if(is.integer64(expected_result_x_y32) && length(expected_result_x_y32))
#         expected_result_x_y32 = expected_result_x_y32 + my_if_else(!is.na(x) & x < 0L & y32 != 0L, shiftOffset, as.integer64(0L))
#       if(is.integer64(expected_result_x_y64) && length(expected_result_x_y64))
#         expected_result_x_y64 = expected_result_x_y64 + my_if_else(!is.na(x) & x < 0L & y32 != 0L, shiftOffset, as.integer64(0L))
        
#       shiftOffset = bitwShiftL(as.integer64(2L)^32L - 1L, 32L - as.integer(x))
#       if(is.integer64(expected_result_y32_x) && length(expected_result_y32_x))
#         expected_result_y32_x = expected_result_y32_x + my_if_else(y32 < 0L & as.integer(x) != 0L, shiftOffset, as.integer64(0L))
#       if(is.integer64(expected_result_y64_x) && length(expected_result_y64_x))
#         expected_result_y64_x = expected_result_y64_x + my_if_else(y32 < 0L & as.integer(x) != 0L, shiftOffset, as.integer64(0L))
#     }
  
#     fun = get(func)
#     actual_result_x_y32 = tryCatch(fun(x_cast, y32), error=conditionMessage)
#     actual_result_y32_x = tryCatch(fun(y32, x_cast), error=conditionMessage)
#     actual_result_x_y64 = tryCatch(fun(x_cast, y64), error=conditionMessage)
#     actual_result_y64_x = tryCatch(fun(y64, x_cast), error=conditionMessage)

#     expect_identical(actual_result_x_y32, expected_result_x_y32)
#     expect_identical(actual_result_y32_x, expected_result_y32_x)
#     expect_identical(actual_result_x_y64, expected_result_x_y64)
#     expect_identical(actual_result_y64_x, expected_result_y64_x)
#   },
#   .cases=expand.grid(
#     func=c("bitwShiftL", "bitwShiftR"),
#     type=c(
#       NA, "integer64", "double", "logical", "integer", "character", "complex", "factor", "ordered",
#       if (getRversion() > "3.6.0") c("POSIXct", "Date")
#       ),
#     x=I(list(NULL, c(-(10:2), 2:10, seq(-1, 1, 0.25), NA))),
#     stringsAsFactors=FALSE
#   )
# )

test_that("bitwise functions work in integer64 range", {
  expect_identical(bitwShiftL(as.integer64(1L), 62L), as.integer64(2L)^62L)
  expect_identical(bitwShiftL(as.integer64(-1L), 62L), -as.integer64(2L)^62L)
  expect_identical(bitwShiftL(as.integer64(1L), 63:70), rep(NA_integer64_, 8))
  expect_identical(bitwShiftL(as.integer64(-1L), 63:70), rep(NA_integer64_, 8))

  expect_identical(bitwShiftR(as.integer64(1L), 63:70), rep(as.integer64(0L), 8))
  expect_identical(bitwShiftR(as.integer64(-1L), 63:70), c(as.integer64(1L), rep(NA_integer64_, 7)))
})

####### to be deleted later #######

with_parameters_test_that(
  "ancient bitwShiftL works: ",
  {
    y32 = -3:3
    y64 = as.integer64(y32)
    if (!is.na(type))
      eval(parse(text=paste0("x_cast = as.", type, "(x)")))
    else
      x_cast = x
    
    if (!is.na(type) && type == "integer64")
      x = as.integer(x)
    else
      x = x_cast

    fun = get(func, baseenv())
    expected_result_x_y32 = tryCatch(fun(x, y32), error=conditionMessage)
    expected_result_y32_x = tryCatch(fun(y32, x), error=conditionMessage)
    expected_result_x_y64 = tryCatch(fun(x, y32), error=conditionMessage)
    expected_result_y64_x = tryCatch(as.integer64(fun(y32, x)), error=conditionMessage)
    if (!is.na(type) && type == "integer64" && !is.character(expected_result_x_y32)) {
      expected_result_x_y32 = as.integer64(expected_result_x_y32)
      expected_result_x_y64 = as.integer64(expected_result_x_y64)
    }
    # because of the way bitwShiftR is defined, it shifts based on unsigned integers
    if (func == "bitwShiftR" && !is.null(x)) {
      shiftOffset = bitwShiftL(as.integer64(2L)^32L - 1L, 32L - y32)
      if(is.integer64(expected_result_x_y32) && length(expected_result_x_y32))
        expected_result_x_y32 = expected_result_x_y32 + my_if_else(!is.na(x) & x < 0L & y32 != 0L, shiftOffset, as.integer64(0L))
      if(is.integer64(expected_result_x_y64) && length(expected_result_x_y64))
        expected_result_x_y64 = expected_result_x_y64 + my_if_else(!is.na(x) & x < 0L & y32 != 0L, shiftOffset, as.integer64(0L))
        
      shiftOffset = bitwShiftL(as.integer64(2L)^32L - 1L, 32L - as.integer(x))
      if(is.integer64(expected_result_y32_x) && length(expected_result_y32_x))
        expected_result_y32_x = expected_result_y32_x + my_if_else(y32 < 0L & as.integer(x) != 0L, shiftOffset, as.integer64(0L))
      if(is.integer64(expected_result_y64_x) && length(expected_result_y64_x))
        expected_result_y64_x = expected_result_y64_x + my_if_else(y32 < 0L & as.integer(x) != 0L, shiftOffset, as.integer64(0L))
    }
  
    fun = get(func)
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
    func=c("bitwShiftL"),
    type=c(
      NA, "integer64", "double", "logical", "integer", "character", "complex", "factor", "ordered",
      if (getRversion() > "3.6.0") c("POSIXct", "Date")
      ),
    x=I(list(NULL, c(-(10:2), 2:10, seq(-1, 1, 0.25), NA))),
    stringsAsFactors=FALSE
  )
)


test_that(
  "ancient 1",
  {
    x = c(-(10:2), 2:10, seq(-1, 1, 0.25), NA)
    y32 = -3:3
    y64 = as.integer64(y32)
    x_cast = x
    x = x_cast

    fun = base::bitwShiftR
    expected_result_y64_x = tryCatch(as.integer64(fun(y32, x)), error=conditionMessage)

    expect_identical(expected_result_y64_x, as.integer64(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 1073741823L, 0L, 0L, 0L, 0L, 33554431L, 16777215L, 8388607L, 0L, NA, 2L, 3L, -3L, -2L, -1L, 0L, 1L, 1L, NA)))
    
    # because of the way bitwShiftR is defined, it shifts based on unsigned integers
    shiftOffset = bitwShiftL(as.integer64(2L)^32L - 1L, 32L - as.integer(x))
    
    expect_identical(shiftOffset, as.integer64(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, "4611686017353646080", "2305843008676823040", "1152921504338411520", "576460752169205760", "288230376084602880", "144115188042301440", "72057594021150720", "36028797010575360", "18014398505287680", NA, NA, NA, NA, NA, NA, NA, NA, "9223372034707292160", NA)))
    expect_identical(is.integer64(expected_result_y64_x) && length(expected_result_y64_x), TRUE)
    expect_identical(my_if_else(y32 < 0L & as.integer(x) != 0L, shiftOffset, as.integer64(0L)), as.integer64(c(NA, NA, NA, "0", "0", "0", "0", NA, NA, "4611686017353646080", "0", "0", "0", "0", "144115188042301440", "72057594021150720", "36028797010575360", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0")))
    
    if(is.integer64(expected_result_y64_x) && length(expected_result_y64_x))
      expected_result_y64_x = expected_result_y64_x + my_if_else(y32 < 0L & as.integer(x) != 0L, shiftOffset, as.integer64(0L))
      
    expect_identical(expected_result_y64_x, as.integer64(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, "4611686018427387903", "0", "0", "0", "0", "144115188075855871", "72057594037927935", "36028797018963967", "0", NA, "2", "3", "-3", "-2", "-1", "0", "1", "1", NA)))
    
    fun = bitwShiftR
    actual_result_y64_x = tryCatch(fun(y64, x_cast), error=conditionMessage)

    expect_identical(actual_result_y64_x, expected_result_y64_x)
  }
)

