matrix64 = function(x, nrow=1L, ncol=1L, byrow=FALSE) {
  matrix(as.integer64(x), nrow=nrow, ncol=ncol, byrow=byrow)
}

array64 = function(x, dim) {
  array(as.integer64(x), dim=dim)
}

# Test that 'expr' gives the same result whether
#   the input is integer or integer64, in the sense
#   of equivalence after casting between the types.
# expr gets integer names converted to integer64,
#   retaining attributes (esp. for arrays), and
#   we test that the result of evaluating expr
#   is equivalent (after converting back to integer)
# Starting with integer and casting to integer64
#   guarantees representation, where casting integer64
#   to integer might have to stipulate inputs must be
#   representable as integer.
expect_int_32_64_equivalent <- function(expr) {
  # Capture the unevaluated expression
  esub = substitute(expr)
  evar = all.vars(esub)

  # replace all integer values in expr with integer64 equivalents
  #   in a tailored environment
  parent_ = parent.frame()
  int64_env = new.env(parent = parent_)
  for (key in evar) {
    val = get(key, parent_)
    if (!is.integer(val)) next
    assign(key, as.integer64(val), envir=int64_env)
    attributes(int64_env[[key]]) = attributes(val)
  }

  int_result = eval(expr, parent_)
  int64_result = eval(expr, int64_env)
  int64_result_as_int = as(int64_result, typeof(int_result))
  # ignore class (which includes integer64)
  a64 = attributes(int64_result)
  for (a in setdiff(names(a64), "class"))
    attr(int64_result_as_int, a) = a64[[a]]

  expect_identical(int64_result_as_int, int_result)
}

skip_if_not_r_version = function(ver) {
  skip_if(getRversion() < ver, paste("R version >=", ver, "required."))
}

test_with_cases = function(test_statement, cases=data.frame()) {
  for (i in seq_len(nrow(cases))) {
    e = new.env()
    pf = parent.frame()
    for (n in names(pf))
      e[[n]] = pf[[n]]
    for (n in names(cases))
      e[[n]] = cases[[n]][[i]]
    eval(substitute(test_statement), envir=e)
  }
}
