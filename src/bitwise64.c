/*
# C-Code
# S3 atomic 64bit integers for R
# (c) 2026 Michael Chirico
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2026-03-24
#*/

#define _INTEGER64_C_SRC

// this define before stdio.h removes the warnings
// warning: unknown conversion type character 'l' in format [-Wformat]
// warning: too many arguments for format [-Wformat-extra-args]
#define __USE_MINGW_ANSI_STDIO 1

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>
#include <Rinternals.h>

#include "bitwise64.h"

#define mod_iterate(n1,n2,i1,i2) for (i=i1=i2=0; i<n; i1 = (++i1 == n1) ? 0 : i1, i2 = (++i2 == n2) ? 0 : i2,++i)

SEXP bitwNot_integer64(SEXP x_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long * x = (long long *) REAL(x_);
  long long * ret = (long long *) REAL(ret_);
  for (i = 0; i < n; i++) {
    if (x[i] == NA_INTEGER64)
      ret[i] = NA_INTEGER64;
    else
      ret[i] = ~x[i];
  }
  return ret_;
}

SEXP bitwAnd_integer64_integer64(SEXP x_, SEXP y_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(x_);
  long long i2, n2 = LENGTH(y_);
  long long * x = (long long *) REAL(x_);
  long long * y = (long long *) REAL(y_);
  long long * ret = (long long *) REAL(ret_);
  mod_iterate(n1, n2, i1, i2) {
    BITWAND64(x[i1],y[i2],ret[i])
  }
  return ret_;
}

SEXP bitwAnd_integer64_integer(SEXP x_, SEXP y_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(x_);
  long long i2, n2 = LENGTH(y_);
  long long * x = (long long *) REAL(x_);
  int * y = INTEGER(y_);
  long long * ret = (long long *) REAL(ret_);
  mod_iterate(n1, n2, i1, i2) {
    BITWAND(x[i1],y[i2],ret[i])
  }
  return ret_;
}

SEXP bitwOr_integer64_integer64(SEXP x_, SEXP y_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(x_);
  long long i2, n2 = LENGTH(y_);
  long long * x = (long long *) REAL(x_);
  long long * y = (long long *) REAL(y_);
  long long * ret = (long long *) REAL(ret_);
  mod_iterate(n1, n2, i1, i2) {
    BITWOR64(x[i1],y[i2],ret[i])
  }
  return ret_;
}

SEXP bitwOr_integer64_integer(SEXP x_, SEXP y_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(x_);
  long long i2, n2 = LENGTH(y_);
  long long * x = (long long *) REAL(x_);
  int * y = INTEGER(y_);
  long long * ret = (long long *) REAL(ret_);
  mod_iterate(n1, n2, i1, i2) {
    BITWOR(x[i1],y[i2],ret[i])
  }
  return ret_;
}

SEXP bitwXor_integer64_integer64(SEXP x_, SEXP y_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(x_);
  long long i2, n2 = LENGTH(y_);
  long long * x = (long long *) REAL(x_);
  long long * y = (long long *) REAL(y_);
  long long * ret = (long long *) REAL(ret_);
  mod_iterate(n1, n2, i1, i2) {
    BITWXOR64(x[i1],y[i2],ret[i])
  }
  return ret_;
}

SEXP bitwXor_integer64_integer(SEXP x_, SEXP y_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(x_);
  long long i2, n2 = LENGTH(y_);
  long long * x = (long long *) REAL(x_);
  int * y = INTEGER(y_);
  long long * ret = (long long *) REAL(ret_);
  mod_iterate(n1, n2, i1, i2) {
    BITWXOR(x[i1],y[i2],ret[i])
  }
  return ret_;
}

SEXP bitwShiftL_integer64_integer64(SEXP x_, SEXP y_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(x_);
  long long i2, n2 = LENGTH(y_);
  long long * x = (long long *) REAL(x_);
  long long * y = (long long *) REAL(y_);
  long long * ret = (long long *) REAL(ret_);
  mod_iterate(n1, n2, i1, i2) {
    BITWSHIFTL(x[i1],y[i2],ret[i])
  }
  return ret_;
}

SEXP bitwShiftL_integer64_integer(SEXP x_, SEXP y_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(x_);
  long long i2, n2 = LENGTH(y_);
  long long * x = (long long *) REAL(x_);
  int * y = INTEGER(y_);
  long long * ret = (long long *) REAL(ret_);
  mod_iterate(n1, n2, i1, i2) {
    BITWSHIFTL(x[i1],y[i2],ret[i])
  }
  return ret_;
}

SEXP bitwShiftR_integer64_integer64(SEXP x_, SEXP y_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(x_);
  long long i2, n2 = LENGTH(y_);
  long long * x = (long long *) REAL(x_);
  long long * y = (long long *) REAL(y_);
  long long * ret = (long long *) REAL(ret_);
  mod_iterate(n1, n2, i1, i2) {
    BITWSHIFTR(x[i1],y[i2],ret[i])
  }
  return ret_;
}

SEXP bitwShiftR_integer64_integer(SEXP x_, SEXP y_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(x_);
  long long i2, n2 = LENGTH(y_);
  long long * x = (long long *) REAL(x_);
  int * y = INTEGER(y_);
  long long * ret = (long long *) REAL(ret_);
  mod_iterate(n1, n2, i1, i2) {
    BITWSHIFTR(x[i1],y[i2],ret[i])
  }
  return ret_;
}
