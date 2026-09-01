#ifndef BIT64_SRC_BITWISE64_H_
#define BIT64_SRC_BITWISE64_H_

#include <Rinternals.h>

SEXP bitwNot_integer64(SEXP x_, SEXP ret_);
SEXP bitwAnd_integer64_integer64(SEXP x_, SEXP y_, SEXP ret_);
SEXP bitwAnd_integer64_integer(SEXP x_, SEXP y_, SEXP ret_);
SEXP bitwOr_integer64_integer64(SEXP x_, SEXP y_, SEXP ret_);
SEXP bitwOr_integer64_integer(SEXP x_, SEXP y_, SEXP ret_);
SEXP bitwXor_integer64_integer64(SEXP x_, SEXP y_, SEXP ret_);
SEXP bitwXor_integer64_integer(SEXP x_, SEXP y_, SEXP ret_);
SEXP bitwShiftL_integer64_integer64(SEXP x_, SEXP y_, SEXP ret_);
SEXP bitwShiftL_integer64_integer(SEXP x_, SEXP y_, SEXP ret_);
SEXP bitwShiftR_integer64_integer64(SEXP x_, SEXP y_, SEXP ret_);
SEXP bitwShiftR_integer64_integer(SEXP x_, SEXP y_, SEXP ret_);

#endif  // BIT64_SRC_BITWISE64_H_
