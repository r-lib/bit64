/*
# Header-Code
# S3 atomic 64bit integers for R
# (c) 2011-2024 Jens Oehlschägel
# (c) 2025-2026 Michael Chirico
# Licence: GPL2
# Provided 'as is', use at your own risk
#*/


#ifndef _INTEGER64_INLCUDED
#define _INTEGER64_INLCUDED

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
#include <stdbool.h>

#define NA_INTEGER64 LLONG_MIN
#define ISNA_INTEGER64(X)((X)==NA_INTEGER64)

#define MIN_INTEGER64 (LLONG_MIN+1)
#define MAX_INTEGER64 LLONG_MAX
#define MIN_INTEGER32 (INT_MIN+1)
#define MAX_INTEGER32 INT_MAX
#define LEFTBIT_INTEGER64 ((unsigned long long int)0x8000000000000000)
#define RIGHTBIT_INTEGER64 ((unsigned long long int)0x0000000000000001)
#define NCHARS_BITS_INTEGER64 65
#define NCHARS_DECS_INTEGER64 22
#define COERCE_INTEGER64 "%lli"
#define BITS_INTEGER64 64

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
  if (__builtin_add_overflow(a, b, &r) || r == NA_INTEGER64) {
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
  if (__builtin_sub_overflow(a, b, &r) || r == NA_INTEGER64) {
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
  if (__builtin_mul_overflow(a, b, &r) || r == NA_INTEGER64) {
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
  // special cases: n^0, 1^m, 0^m, -1^m, n^-m, NA^m, n^NA
  if (exp == 0) {
    *res = 1;
    return false;
  }
  if (base == 1) {
    *res = 1;
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
  if (base == NA_INTEGER64 || exp == NA_INTEGER64) {
    *res = NA_INTEGER64;
    return false;
  }
  long long r = 1;
  while (exp > 0) {
    if (exp & 1 && mul64_overflow(r, base, &r)) {
      *res = NA_INTEGER64;
      return true;
    }
    exp >>= 1;
    if (exp > 0 && mul64_overflow(base, base, &base)) {
      *res = NA_INTEGER64;
      return true;
    }
  }
  *res = r;
  return false;
}

#define PLUS64(e1,e2,ret,naflag) \
    if (e1 == NA_INTEGER64 || e2 == NA_INTEGER64) \
        ret = NA_INTEGER64; \
    else if (add64_overflow(e1, e2, &ret)) { \
        ret = NA_INTEGER64; \
        naflag = TRUE; \
    }

#define MINUS64(e1,e2,ret,naflag) \
    if (e1 == NA_INTEGER64 || e2 == NA_INTEGER64) \
        ret = NA_INTEGER64; \
    else if (sub64_overflow(e1, e2, &ret)) { \
        ret = NA_INTEGER64; \
        naflag = TRUE; \
    }

#define PROD64(e1,e2,ret,naflag) \
    if (e1 == NA_INTEGER64 || e2 == NA_INTEGER64) \
        ret = NA_INTEGER64; \
    else if (mul64_overflow(e1, e2, &ret)) { \
        ret = NA_INTEGER64; \
        naflag = TRUE; \
    }

#define PROD64REAL(e1,e2,ret,naflag,longret) \
    if (e1 == NA_INTEGER64 || ISNAN(e2)) \
        ret = NA_INTEGER64; \
    else { \
        longret = e1 * (long double) e2; \
        if (isnan(longret) || longret>MAX_INTEGER64){ \
          naflag = TRUE; \
          ret = NA_INTEGER64; \
        }else \
          ret = llroundl(longret); \
    }

#define POW64REAL(e1,e2,ret,naflag,longret) \
    if (e1 == NA_INTEGER64 || ISNAN(e2)) \
        ret = NA_INTEGER64; \
    else { \
        longret = pow(e1, (long double) e2); \
        if (isnan(longret)){ \
          naflag = TRUE; \
          ret = NA_INTEGER64; \
        }else \
          ret = llroundl(longret); \
    }

#define DIVIDE64REAL(e1,e2,ret,naflag) \
    if (e1 == NA_INTEGER64 || ISNAN(e2)) \
        ret = NA_REAL; \
    else { \
        if (e2==0) \
            ret = NA_REAL; \
        else \
            ret = (double)((long double) e1 / (long double) e2); \
        if (ISNAN(ret)) \
            naflag = TRUE; \
    }


/* Ofek Shilon */
#define DIVIDEREAL64(e1,e2,ret,naflag)                   \
if (e2 == NA_INTEGER64 || ISNAN(e1))                     \
  ret = NA_REAL;                                         \
else {                                                   \
  if (e2==0)                                             \
    ret = NA_REAL;                                       \
  else                                                   \
    ret = (double)((long double) e1 / (long double) e2); \
  if (ISNAN(ret))                                        \
    naflag = TRUE;                                       \
}                                                              \


#define DIVIDE64(e1,e2,ret,naflag) \
    if (e1 == NA_INTEGER64 || e2 == NA_INTEGER64) \
        ret = NA_REAL; \
    else { \
        if (e2==0) \
            ret = NA_REAL; \
        else \
            ret = (double)((long double) e1 / (long double) e2); \
        if (ISNAN(ret)) \
            naflag = TRUE; \
    }

/* int division truncate to lower */
#define INTDIV64(e1,e2,ret,naflag) \
    if (e1 == NA_INTEGER64 || e2 == NA_INTEGER64) \
        ret = NA_INTEGER64; \
    else { \
        if (e2==0) \
            ret = NA_INTEGER64; \
        else \
            ret = e1 / e2; \
        if (ret == NA_INTEGER64) \
            naflag = TRUE; \
        else if ((e1^e2) < 0 && ret*e2 != e1) \
            ret -= 1; \
    }

/* int division truncate to lower */
#define MOD64(e1,e2,ret,naflag) \
    if (e1 == NA_INTEGER64 || e2 == NA_INTEGER64) \
        ret = NA_INTEGER64; \
    else { \
        if (e2==0) \
            ret = NA_INTEGER64; \
        else \
            ret = e1 / e2; \
        if (ret == NA_INTEGER64) \
            naflag = TRUE; \
        else { \
            if ((e1^e2) < 0 && ret*e2 != e1) \
                ret -= 1; \
            ret = e1 - e2 * ret; \
            } \
    }

#define MIN64(e1,e2,ret) \
    if (e1 == NA_INTEGER64 || e2 == NA_INTEGER64) \
        ret = NA_INTEGER64; \
    else { \
        ret = (e1 < e2) ? e1 : e2; \
    }

#define MAX64(e1,e2,ret) \
    if (e1 == NA_INTEGER64 || e2 == NA_INTEGER64) \
        ret = NA_INTEGER64; \
    else { \
        ret = (e1 < e2) ? e2 : e1; \
    }

#define ABS64(e1,ret) \
    if (e1 == NA_INTEGER64) \
        ret = NA_INTEGER64; \
    else { \
        ret = (e1 < 0) ? -e1 : e1; \
    }

#define SQRT64(e1, ret, naflag) \
    if (e1 == NA_INTEGER64) \
        ret = NA_REAL; \
    else { \
        if (e1 < 0) \
            naflag = TRUE; \
        ret = (double) sqrtl((long double)e1); \
    }

#define LOG64(e1, ret, naflag) \
    if (e1 == NA_INTEGER64) \
        ret = NA_REAL; \
    else { \
        ret = (double) logl((long double)e1); \
        if (isnan(ret)) \
            naflag = TRUE; \
    }

// NB: cast to double _after_ dividing in 'long double' for max precision.
#define LOGVECT64(e1, e2, ret, naflag) \
    if (e1 == NA_INTEGER64) \
        ret = NA_REAL; \
    else { \
        ret = (double) (logl((long double)e1) / logl((long double)e2)); \
        if (isnan(ret)) \
            naflag = TRUE; \
    }

#define LOGBASE64(e1, e2, ret, naflag) \
    if (e1 == NA_INTEGER64) \
        ret = NA_REAL; \
    else { \
        ret = (double) (logl((long double)e1) / e2); \
        if (isnan(ret)) \
            naflag = TRUE; \
    }

#define LOG1064(e1, ret, naflag) \
    if (e1 == NA_INTEGER64) \
        ret = NA_REAL; \
    else { \
        ret = (double) log10l((long double)e1); \
        if (isnan(ret)) \
            naflag = TRUE; \
    }

#define LOG264(e1, ret, naflag) \
    if (e1 == NA_INTEGER64) \
        ret = NA_REAL; \
    else { \
        ret = (double) log2l((long double)e1); \
        if (isnan(ret)) \
            naflag = TRUE; \
    }


#define SIGN64(e1,ret) \
    if (e1 == NA_INTEGER64) \
        ret = NA_INTEGER64; \
    else { \
        ret = (e1 < 0) ? -1 : ((e1 > 0) ? 1 : 0); \
    }

#define EQ64(e1,e2,ret) \
    if (e1 == NA_INTEGER64 || e2 == NA_INTEGER64) \
        ret = NA_LOGICAL; \
    else { \
        ret = (e1 == e2) ? TRUE : FALSE; \
    }

#define NE64(e1,e2,ret) \
    if (e1 == NA_INTEGER64 || e2 == NA_INTEGER64) \
        ret = NA_LOGICAL; \
    else { \
        ret = (e1 != e2) ? TRUE : FALSE; \
    }

#define LT64(e1,e2,ret) \
    if (e1 == NA_INTEGER64 || e2 == NA_INTEGER64) \
        ret = NA_LOGICAL; \
    else { \
        ret = (e1 < e2) ? TRUE : FALSE; \
    }

#define LE64(e1,e2,ret) \
    if (e1 == NA_INTEGER64 || e2 == NA_INTEGER64) \
        ret = NA_LOGICAL; \
    else { \
        ret = (e1 <= e2) ? TRUE : FALSE; \
    }

#define GT64(e1,e2,ret) \
    if (e1 == NA_INTEGER64 || e2 == NA_INTEGER64) \
        ret = NA_LOGICAL; \
    else { \
        ret = (e1 > e2) ? TRUE : FALSE; \
    }

#define GE64(e1,e2,ret) \
    if (e1 == NA_INTEGER64 || e2 == NA_INTEGER64) \
        ret = NA_LOGICAL; \
    else { \
        ret = (e1 >= e2) ? TRUE : FALSE; \
    }

#endif  // _INTEGER64_INLCUDED
