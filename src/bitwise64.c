/*
# C-Code
# S3 atomic 64bit integers for R
# (c) 2026 Michael Chirico
# Licence: GPL2
# Provided 'as is', use at your own risk
#*/

#include <stdbool.h>

#include <R.h>
#include <R_ext/Arith.h>
#include <Rinternals.h>

#include "bitwise64.h"
#include "integer64.h"

static inline long long bitwnot64(long long x) {
  if (is_na64(x)) {
    return NA_INTEGER64;
  }
  return ~x;
}

static inline long long bitwand64(long long x, long long y) {
  if (is_na64(x) || is_na64(y)) {
    return NA_INTEGER64;
  }
  return x & y;
}

static inline long long bitwand64_int(long long x, int y) {
  if (is_na64(x) || y == NA_INTEGER) {
    return NA_INTEGER64;
  }
  return x & (long long) y;
}

static inline long long bitwor64(long long x, long long y) {
  if (is_na64(x) || is_na64(y)) {
    return NA_INTEGER64;
  }
  return x | y;
}

static inline long long bitwor64_int(long long x, int y) {
  if (is_na64(x) || y == NA_INTEGER) {
    return NA_INTEGER64;
  }
  return x | (long long) y;
}

static inline long long bitwxor64(long long x, long long y) {
  if (is_na64(x) || is_na64(y)) {
    return NA_INTEGER64;
  }
  return x ^ y;
}

static inline long long bitwxor64_int(long long x, int y) {
  if (is_na64(x) || y == NA_INTEGER) {
    return NA_INTEGER64;
  }
  return x ^ (long long) y;
}

static inline bool good_shiftl64(long long x, long long y, long long z) {
  if (is_na64(z)) {
    return false;
  }
  if (y == 0 || x == 0) {
    return z == x;
  }
  return (x > 0) ? (z > x) : (z < x);
}

static inline long long bitwshiftl64(long long x, long long y) {
  if (is_na64(x) || is_na64(y) || y < 0 || y > 63) {
    return NA_INTEGER64;
  }
  long long ret = (long long) ((unsigned long long) x << y);
  if (!good_shiftl64(x, y, ret)) {
    return NA_INTEGER64;
  }
  return ret;
}

static inline long long bitwshiftl64_int(long long x, int y) {
  if (is_na64(x) || y == NA_INTEGER || y < 0 || y > 63) {
    return NA_INTEGER64;
  }
  long long ret = (long long) ((unsigned long long) x << y);
  if (!good_shiftl64(x, y, ret)) {
    return NA_INTEGER64;
  }
  return ret;
}

static inline long long bitwshiftr64(long long x, long long y) {
  if (is_na64(x) || is_na64(y) || y < 0 || y > 63) {
    return NA_INTEGER64;
  }
  return (long long) ((unsigned long long) x >> y);
}

static inline long long bitwshiftr64_int(long long x, int y) {
  if (is_na64(x) || y == NA_INTEGER || y < 0 || y > 63) {
    return NA_INTEGER64;
  }
  return (long long) ((unsigned long long) x >> y);
}

SEXP bitwNot_integer64(SEXP x_, SEXP ret_) {
  long long n = LENGTH(ret_);
  long long * x = (long long *) REAL(x_);
  long long * ret = (long long *) REAL(ret_);
  for (long long i = 0; i < n; i++) {
    ret[i] = bitwnot64(x[i]);
  }
  return ret_;
}

SEXP bitwAnd_integer64_integer64(SEXP x_, SEXP y_, SEXP ret_) {
  long long n = LENGTH(ret_);
  long long n1 = LENGTH(x_);
  long long n2 = LENGTH(y_);
  long long * x = (long long *) REAL(x_);
  long long * y = (long long *) REAL(y_);
  long long * ret = (long long *) REAL(ret_);
  for (long long i = 0, i1 = 0, i2 = 0; i < n; i++) {
    ret[i] = bitwand64(x[i1], y[i2]);
    if (++i1 == n1) i1 = 0;
    if (++i2 == n2) i2 = 0;
  }
  return ret_;
}

SEXP bitwAnd_integer64_integer(SEXP x_, SEXP y_, SEXP ret_) {
  long long n = LENGTH(ret_);
  long long n1 = LENGTH(x_);
  long long n2 = LENGTH(y_);
  long long * x = (long long *) REAL(x_);
  int * y = INTEGER(y_);
  long long * ret = (long long *) REAL(ret_);
  for (long long i = 0, i1 = 0, i2 = 0; i < n; i++) {
    ret[i] = bitwand64_int(x[i1], y[i2]);
    if (++i1 == n1) i1 = 0;
    if (++i2 == n2) i2 = 0;
  }
  return ret_;
}

SEXP bitwOr_integer64_integer64(SEXP x_, SEXP y_, SEXP ret_) {
  long long n = LENGTH(ret_);
  long long n1 = LENGTH(x_);
  long long n2 = LENGTH(y_);
  long long * x = (long long *) REAL(x_);
  long long * y = (long long *) REAL(y_);
  long long * ret = (long long *) REAL(ret_);
  for (long long i = 0, i1 = 0, i2 = 0; i < n; i++) {
    ret[i] = bitwor64(x[i1], y[i2]);
    if (++i1 == n1) i1 = 0;
    if (++i2 == n2) i2 = 0;
  }
  return ret_;
}

SEXP bitwOr_integer64_integer(SEXP x_, SEXP y_, SEXP ret_) {
  long long n = LENGTH(ret_);
  long long n1 = LENGTH(x_);
  long long n2 = LENGTH(y_);
  long long * x = (long long *) REAL(x_);
  int * y = INTEGER(y_);
  long long * ret = (long long *) REAL(ret_);
  for (long long i = 0, i1 = 0, i2 = 0; i < n; i++) {
    ret[i] = bitwor64_int(x[i1], y[i2]);
    if (++i1 == n1) i1 = 0;
    if (++i2 == n2) i2 = 0;
  }
  return ret_;
}

SEXP bitwXor_integer64_integer64(SEXP x_, SEXP y_, SEXP ret_) {
  long long n = LENGTH(ret_);
  long long n1 = LENGTH(x_);
  long long n2 = LENGTH(y_);
  long long * x = (long long *) REAL(x_);
  long long * y = (long long *) REAL(y_);
  long long * ret = (long long *) REAL(ret_);
  for (long long i = 0, i1 = 0, i2 = 0; i < n; i++) {
    ret[i] = bitwxor64(x[i1], y[i2]);
    if (++i1 == n1) i1 = 0;
    if (++i2 == n2) i2 = 0;
  }
  return ret_;
}

SEXP bitwXor_integer64_integer(SEXP x_, SEXP y_, SEXP ret_) {
  long long n = LENGTH(ret_);
  long long n1 = LENGTH(x_);
  long long n2 = LENGTH(y_);
  long long * x = (long long *) REAL(x_);
  int * y = INTEGER(y_);
  long long * ret = (long long *) REAL(ret_);
  for (long long i = 0, i1 = 0, i2 = 0; i < n; i++) {
    ret[i] = bitwxor64_int(x[i1], y[i2]);
    if (++i1 == n1) i1 = 0;
    if (++i2 == n2) i2 = 0;
  }
  return ret_;
}

SEXP bitwShiftL_integer64_integer64(SEXP x_, SEXP y_, SEXP ret_) {
  long long n = LENGTH(ret_);
  long long n1 = LENGTH(x_);
  long long n2 = LENGTH(y_);
  long long * x = (long long *) REAL(x_);
  long long * y = (long long *) REAL(y_);
  long long * ret = (long long *) REAL(ret_);
  for (long long i = 0, i1 = 0, i2 = 0; i < n; i++) {
    ret[i] = bitwshiftl64(x[i1], y[i2]);
    if (++i1 == n1) i1 = 0;
    if (++i2 == n2) i2 = 0;
  }
  return ret_;
}

SEXP bitwShiftL_integer64_integer(SEXP x_, SEXP y_, SEXP ret_) {
  long long n = LENGTH(ret_);
  long long n1 = LENGTH(x_);
  long long n2 = LENGTH(y_);
  long long * x = (long long *) REAL(x_);
  int * y = INTEGER(y_);
  long long * ret = (long long *) REAL(ret_);
  for (long long i = 0, i1 = 0, i2 = 0; i < n; i++) {
    ret[i] = bitwshiftl64_int(x[i1], y[i2]);
    if (++i1 == n1) i1 = 0;
    if (++i2 == n2) i2 = 0;
  }
  return ret_;
}

SEXP bitwShiftR_integer64_integer64(SEXP x_, SEXP y_, SEXP ret_) {
  long long n = LENGTH(ret_);
  long long n1 = LENGTH(x_);
  long long n2 = LENGTH(y_);
  long long * x = (long long *) REAL(x_);
  long long * y = (long long *) REAL(y_);
  long long * ret = (long long *) REAL(ret_);
  for (long long i = 0, i1 = 0, i2 = 0; i < n; i++) {
    ret[i] = bitwshiftr64(x[i1], y[i2]);
    if (++i1 == n1) i1 = 0;
    if (++i2 == n2) i2 = 0;
  }
  return ret_;
}

SEXP bitwShiftR_integer64_integer(SEXP x_, SEXP y_, SEXP ret_) {
  long long n = LENGTH(ret_);
  long long n1 = LENGTH(x_);
  long long n2 = LENGTH(y_);
  long long * x = (long long *) REAL(x_);
  int * y = INTEGER(y_);
  long long * ret = (long long *) REAL(ret_);
  for (long long i = 0, i1 = 0, i2 = 0; i < n; i++) {
    ret[i] = bitwshiftr64_int(x[i1], y[i2]);
    if (++i1 == n1) i1 = 0;
    if (++i2 == n2) i2 = 0;
  }
  return ret_;
}
