/*
# Header-Code
# S3 atomic 64bit integers for R
# (c) 2011-2024 Jens Oehlschägel
# (c) 2025-2026 Michael Chirico
# Licence: GPL2
# Provided 'as is', use at your own risk
#*/


#ifndef BIT64_SRC_INTEGER64_H_
#define BIT64_SRC_INTEGER64_H_

/*****************************************************************************/
/**                                                                         **/
/**                            MODULES USED                                 **/
/**                                                                         **/
/*****************************************************************************/


/*****************************************************************************/
/**                                                                         **/
/**                      DEFINITIONS AND MACROS                             **/
/**                                                                         **/
/*****************************************************************************/

#include <limits.h>
#include <math.h>
#include <stdbool.h>

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Arith.h>
#include <R_ext/Boolean.h>

#define NA_INTEGER64 LLONG_MIN
static inline bool is_na64(long long x) {
  return x == NA_INTEGER64;
}

#define MIN_INTEGER64 (LLONG_MIN + 1)
#define MAX_INTEGER64 LLONG_MAX
#define MIN_INTEGER32 (INT_MIN+1)
#define MAX_INTEGER32 INT_MAX
#define LEFTBIT_INTEGER64 0x8000000000000000ULL
#define RIGHTBIT_INTEGER64 0x0000000000000001ULL

#define INTEGER32_OVERFLOW_WARNING "NAs produced by integer overflow"
#define INTEGER64_OVERFLOW_WARNING "NAs produced by integer64 overflow"
#define INTEGER64_DIVISION_BY_ZERO_WARNING "NAs produced due to division by zero"
#define INTEGER64_NAN_CREATED_WARNING "NaNs produced"
#define INTEGER64_TODOUBLE_WARNING "integer precision lost while converting to double"
#define BITSTRING_OVERFLOW_WARNING "bitstrings longer than 64 bytes converted to NA, multibyte-characters not allowed"
#define INTEGER64_NA_COERCION_WARNING "NAs introduced by coercion to integer64 range"

#if (defined(__GNUC__) && __GNUC__ >= 5) || (defined(__has_builtin) && __has_builtin(__builtin_add_overflow))
#define HAVE_BUILTIN_OVERFLOW 1
#endif

static inline bool add64_overflow(long long a, long long b, long long *res) {
#ifdef HAVE_BUILTIN_OVERFLOW
  long long r;
  if (__builtin_add_overflow(a, b, &r) || is_na64(r)) {
    return true;
  }
  *res = r;
  return false;
#else
  if ((b > 0 && a > MAX_INTEGER64 - b) || (b < 0 && a < MIN_INTEGER64 - b)) {
    return true;
  }
  *res = a + b;
  return false;
#endif
}

static inline bool sub64_overflow(long long a, long long b, long long *res) {
#ifdef HAVE_BUILTIN_OVERFLOW
  long long r;
  if (__builtin_sub_overflow(a, b, &r) || is_na64(r)) {
    return true;
  }
  *res = r;
  return false;
#else
  if ((b > 0 && a < MIN_INTEGER64 + b) || (b < 0 && a > MAX_INTEGER64 + b)) {
    return true;
  }
  *res = a - b;
  return false;
#endif
}

static inline bool mul64_overflow(long long a, long long b, long long *res) {
#ifdef HAVE_BUILTIN_OVERFLOW
  long long r;
  if (__builtin_mul_overflow(a, b, &r) || is_na64(r)) {
    return true;
  }
  *res = r;
  return false;
#else
  if (a == 0 || b == 0) {
    *res = 0;
    return false;
  }
  if (a > 0) {
    if (b > 0) {
      if (a > MAX_INTEGER64 / b) return true;
    } else {
      if (b < MIN_INTEGER64 / a) return true;
    }
  } else {
    if (b > 0) {
      if (a < MIN_INTEGER64 / b) return true;
    } else {
      if (a < MAX_INTEGER64 / b) return true;
    }
  }
  *res = a * b;
  return false;
#endif
}

static inline bool pow64_overflow(long long base, long long exp, long long *res) {
  // special cases: n^0, 1^m, NA^m, n^NA, n^1, n^2, 0^m, -1^m, n^-m
  if (exp == 0) {
    *res = 1;
    return false;
  }
  if (base == 1) {
    *res = 1;
    return false;
  }
  if (is_na64(base) || is_na64(exp)) {
    *res = NA_INTEGER64;
    return false;
  }
  if (exp == 1) {
    *res = base;
    return false;
  }
  if (exp == 2) {
    if (mul64_overflow(base, base, res)) {
      *res = NA_INTEGER64;
      return true;
    }
    return false;
  }
  if (base == 0) {
    if (exp < 0) {
      *res = NA_INTEGER64;
      return true;
    }
    *res = 0;
    return false;
  }
  if (base == -1) {
    *res = exp & 1 ? -1 : 1;
    return false;
  }
  if (exp < 0) {
    *res = 0;
    return false;
  }
  long long r = 1;
  while (1) {
    if (exp & 1 && mul64_overflow(r, base, &r)) {
      *res = NA_INTEGER64;
      return true;
    }
    exp >>= 1;
    if (!exp) break;
    if (mul64_overflow(base, base, &base)) {
      *res = NA_INTEGER64;
      return true;
    }
  }
  *res = r;
  return false;
}

static inline long long plus64(long long e1, long long e2, Rboolean *naflag) {
  if (is_na64(e1) || is_na64(e2)) {
    return NA_INTEGER64;
  }
  long long ret;
  if (add64_overflow(e1, e2, &ret)) {
    *naflag = TRUE;
    return NA_INTEGER64;
  }
  return ret;
}

static inline long long minus64(long long e1, long long e2, Rboolean *naflag) {
  if (is_na64(e1) || is_na64(e2)) {
    return NA_INTEGER64;
  }
  long long ret;
  if (sub64_overflow(e1, e2, &ret)) {
    *naflag = TRUE;
    return NA_INTEGER64;
  }
  return ret;
}

static inline long long prod64(long long e1, long long e2, Rboolean *naflag) {
  if (is_na64(e1) || is_na64(e2)) {
    return NA_INTEGER64;
  }
  long long ret;
  if (mul64_overflow(e1, e2, &ret)) {
    *naflag = TRUE;
    return NA_INTEGER64;
  }
  return ret;
}

static inline long long prod64real(long long e1, double e2, Rboolean *naflag) {
  if (is_na64(e1) || ISNAN(e2)) {
    return NA_INTEGER64;
  }
  long double longret = e1 * (long double) e2;
  if (isnan(longret) || longret > MAX_INTEGER64) {
    *naflag = TRUE;
    return NA_INTEGER64;
  }
  return llroundl(longret);
}

static inline long long pow64real(long long e1, double e2, Rboolean *naflag) {
  if (is_na64(e1) || ISNAN(e2)) {
    return NA_INTEGER64;
  }
  long double longret = powl((long double) e1, (long double) e2);
  if (isnan(longret)) {
    *naflag = TRUE;
    return NA_INTEGER64;
  }
  return llroundl(longret);
}

static inline double divide64real(long long e1, double e2, Rboolean *naflag) {
  if (is_na64(e1) || ISNAN(e2)) {
    return NA_REAL;
  }
  if (e2 == 0.0) {
    *naflag = TRUE;
    return NA_REAL;
  }
  double ret = (double) ((long double) e1 / (long double) e2);
  if (ISNAN(ret)) {
    *naflag = TRUE;
  }
  return ret;
}

/* Ofek Shilon */
static inline double dividereal64(double e1, long long e2, Rboolean *naflag) {
  if (is_na64(e2) || ISNAN(e1)) {
    return NA_REAL;
  }
  if (e2 == 0) {
    *naflag = TRUE;
    return NA_REAL;
  }
  double ret = (double) ((long double) e1 / (long double) e2);
  if (ISNAN(ret)) {
    *naflag = TRUE;
  }
  return ret;
}

static inline double divide64(long long e1, long long e2, Rboolean *naflag) {
  if (is_na64(e1) || is_na64(e2)) {
    return NA_REAL;
  }
  if (e2 == 0) {
    *naflag = TRUE;
    return NA_REAL;
  }
  double ret = (double) ((long double) e1 / (long double) e2);
  if (ISNAN(ret)) {
    *naflag = TRUE;
  }
  return ret;
}

/* int division truncate to lower */
static inline long long intdiv64(long long e1, long long e2, Rboolean *naflag) {
  if (is_na64(e1) || is_na64(e2)) {
    return NA_INTEGER64;
  }
  if (e2 == 0) {
    *naflag = TRUE;
    return NA_INTEGER64;
  }
  long long ret = e1 / e2;
  if (is_na64(ret)) {
    *naflag = TRUE;
  } else if ((e1 ^ e2) < 0 && ret * e2 != e1) {
    ret -= 1;
  }
  return ret;
}

/* int division truncate to lower */
static inline long long mod64(long long e1, long long e2, Rboolean *naflag) {
  if (is_na64(e1) || is_na64(e2)) {
    return NA_INTEGER64;
  }
  if (e2 == 0) {
    *naflag = TRUE;
    return NA_INTEGER64;
  }
  long long ret = e1 / e2;
  if (is_na64(ret)) {
    *naflag = TRUE;
    return NA_INTEGER64;
  }
  if ((e1 ^ e2) < 0 && ret * e2 != e1) {
    ret -= 1;
  }
  return e1 - e2 * ret;
}

static inline long long min64(long long e1, long long e2) {
  if (is_na64(e1) || is_na64(e2)) {
    return NA_INTEGER64;
  }
  return (e1 < e2) ? e1 : e2;
}

static inline long long max64(long long e1, long long e2) {
  if (is_na64(e1) || is_na64(e2)) {
    return NA_INTEGER64;
  }
  return (e1 < e2) ? e2 : e1;
}

static inline long long abs64(long long e1) {
  if (is_na64(e1)) {
    return NA_INTEGER64;
  }
  return (e1 < 0) ? -e1 : e1;
}

static inline double sqrt64(long long e1, Rboolean *naflag) {
  if (is_na64(e1)) {
    return NA_REAL;
  }
  if (e1 < 0) {
    *naflag = TRUE;
  }
  return (double) sqrtl((long double) e1);
}

static inline double log64(long long e1, Rboolean *naflag) {
  if (is_na64(e1)) {
    return NA_REAL;
  }
  double ret = (double) logl((long double) e1);
  if (isnan(ret)) {
    *naflag = TRUE;
  }
  return ret;
}

// NB: cast to double _after_ dividing in 'long double' for max precision.
static inline double logvect64(long long e1, double e2, Rboolean *naflag) {
  if (is_na64(e1)) {
    return NA_REAL;
  }
  double ret = (double) (logl((long double) e1) / logl((long double) e2));
  if (isnan(ret)) {
    *naflag = TRUE;
  }
  return ret;
}

static inline double logbase64(long long e1, long double log_base, Rboolean *naflag) {
  if (is_na64(e1)) {
    return NA_REAL;
  }
  double ret = (double) (logl((long double) e1) / log_base);
  if (isnan(ret)) {
    *naflag = TRUE;
  }
  return ret;
}

static inline double log10_64(long long e1, Rboolean *naflag) {
  if (is_na64(e1)) {
    return NA_REAL;
  }
  double ret = (double) log10l((long double) e1);
  if (isnan(ret)) {
    *naflag = TRUE;
  }
  return ret;
}

static inline double log2_64(long long e1, Rboolean *naflag) {
  if (is_na64(e1)) {
    return NA_REAL;
  }
  double ret = (double) log2l((long double) e1);
  if (isnan(ret)) {
    *naflag = TRUE;
  }
  return ret;
}

static inline long long sign64(long long e1) {
  if (is_na64(e1)) {
    return NA_INTEGER64;
  }
  return (e1 < 0) ? -1 : ((e1 > 0) ? 1 : 0);
}

static inline int eq64(long long e1, long long e2) {
  if (is_na64(e1) || is_na64(e2)) {
    return NA_LOGICAL;
  }
  return e1 == e2;
}

static inline int ne64(long long e1, long long e2) {
  if (is_na64(e1) || is_na64(e2)) {
    return NA_LOGICAL;
  }
  return e1 != e2;
}

static inline int lt64(long long e1, long long e2) {
  if (is_na64(e1) || is_na64(e2)) {
    return NA_LOGICAL;
  }
  return e1 < e2;
}

static inline int le64(long long e1, long long e2) {
  if (is_na64(e1) || is_na64(e2)) {
    return NA_LOGICAL;
  }
  return e1 <= e2;
}

static inline int gt64(long long e1, long long e2) {
  if (is_na64(e1) || is_na64(e2)) {
    return NA_LOGICAL;
  }
  return e1 > e2;
}

static inline int ge64(long long e1, long long e2) {
  if (is_na64(e1) || is_na64(e2)) {
    return NA_LOGICAL;
  }
  return e1 >= e2;
}

/*****************************************************************************/
/**                                                                         **/
/**                        EXPORTED SEXP FUNCTIONS                          **/
/**                                                                         **/
/*****************************************************************************/

SEXP as_integer64_double(SEXP x_, SEXP ret_);
SEXP as_integer64_integer(SEXP x_, SEXP ret_);
SEXP as_double_integer64(SEXP x_, SEXP ret_);
SEXP as_integer_integer64(SEXP x_, SEXP ret_);
SEXP as_logical_integer64(SEXP x_, SEXP ret_);
SEXP as_character_integer64(SEXP x_, SEXP ret_);
SEXP as_integer64_character(SEXP x_, SEXP ret_);
SEXP as_bitstring_integer64(SEXP x_, SEXP ret_);
SEXP as_integer64_bitstring(SEXP x_, SEXP ret_);
SEXP plus_integer64(SEXP e1_, SEXP e2_, SEXP ret_);
SEXP minus_integer64(SEXP e1_, SEXP e2_, SEXP ret_);
SEXP diff_integer64(SEXP x_, SEXP lag_, SEXP n_, SEXP ret_);
SEXP intdiv_integer64(SEXP e1_, SEXP e2_, SEXP ret_);
SEXP mod_integer64(SEXP e1_, SEXP e2_, SEXP ret_);
SEXP times_integer64_integer64(SEXP e1_, SEXP e2_, SEXP ret_);
SEXP times_integer64_double(SEXP e1_, SEXP e2_, SEXP ret_);
SEXP power_integer64_integer64(SEXP e1_, SEXP e2_, SEXP ret_);
SEXP power_integer64_double(SEXP e1_, SEXP e2_, SEXP ret_);
SEXP divide_integer64_integer64(SEXP e1_, SEXP e2_, SEXP ret_);
SEXP divide_integer64_double(SEXP e1_, SEXP e2_, SEXP ret_);
SEXP divide_double_integer64(SEXP e1_, SEXP e2_, SEXP ret_);
SEXP sign_integer64(SEXP e1_, SEXP ret_);
SEXP abs_integer64(SEXP e1_, SEXP ret_);
SEXP sqrt_integer64(SEXP e1_, SEXP ret_);
SEXP log_integer64(SEXP e1_, SEXP ret_);
SEXP logvect_integer64(SEXP e1_, SEXP e2_, SEXP ret_);
SEXP logbase_integer64(SEXP e1_, SEXP base_, SEXP ret_);
SEXP log10_integer64(SEXP e1_, SEXP ret_);
SEXP log2_integer64(SEXP e1_, SEXP ret_);
SEXP any_integer64(SEXP e1_, SEXP na_rm_, SEXP ret_);
SEXP all_integer64(SEXP e1_, SEXP na_rm_, SEXP ret_);
SEXP sum_integer64(SEXP e1_, SEXP na_rm_, SEXP ret_);
SEXP mean_integer64(SEXP e1_, SEXP na_rm_, SEXP ret_);
SEXP prod_integer64(SEXP e1_, SEXP na_rm_, SEXP ret_);
SEXP min_integer64(SEXP e1_, SEXP na_rm_, SEXP ret_);
SEXP max_integer64(SEXP e1_, SEXP na_rm_, SEXP ret_);
SEXP range_integer64(SEXP e1_, SEXP na_rm_, SEXP ret_);
SEXP lim_integer64(SEXP ret_);
SEXP cummin_integer64(SEXP e1_, SEXP ret_);
SEXP cummax_integer64(SEXP e1_, SEXP ret_);
SEXP cumsum_integer64(SEXP e1_, SEXP ret_);
SEXP cumprod_integer64(SEXP e1_, SEXP ret_);
SEXP seq_integer64(SEXP from_, SEXP by_, SEXP ret_);
SEXP isna_integer64(SEXP e1_, SEXP ret_);
SEXP EQ_integer64(SEXP e1_, SEXP e2_, SEXP ret_);
SEXP NE_integer64(SEXP e1_, SEXP e2_, SEXP ret_);
SEXP LT_integer64(SEXP e1_, SEXP e2_, SEXP ret_);
SEXP LE_integer64(SEXP e1_, SEXP e2_, SEXP ret_);
SEXP GT_integer64(SEXP e1_, SEXP e2_, SEXP ret_);
SEXP GE_integer64(SEXP e1_, SEXP e2_, SEXP ret_);
SEXP runif_integer64(SEXP n_, SEXP min_, SEXP max_);
SEXP as_list_integer64(SEXP x_);
SEXP matmult_integer64_integer64(SEXP x_, SEXP y_, SEXP ret_);
SEXP matmult_double_integer64(SEXP x_, SEXP y_, SEXP ret_);
SEXP matmult_integer64_double(SEXP x_, SEXP y_, SEXP ret_);

#endif  // BIT64_SRC_INTEGER64_H_
