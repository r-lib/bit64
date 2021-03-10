/*
# Header-Code
# S3 atomic 64bit integers for R
# (c) 2011 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2011-12-11
# Last changed:  2011-12-11
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

#define NA_INTEGER64 LLONG_MIN
#define ISNA_INTEGER64(X)((X)==NA_INTEGER64)

#define MIN_INTEGER64 LLONG_MIN+1
#define MAX_INTEGER64 LLONG_MAX
#define MIN_INTEGER32 INT_MIN+1
#define MAX_INTEGER32 INT_MAX
#define LEFTBIT_INTEGER64 ((unsigned long long int)0x8000000000000000)
#define RIGHTBIT_INTEGER64 ((unsigned long long int)0x0000000000000001)
#define NCHARS_BITS_INTEGER64 65
#define NCHARS_DECS_INTEGER64 22
#define COERCE_INTEGER64 "%lli"
#define USES_TWOS_COMPLEMENT 1
#define BITS_INTEGER64 64

#if USES_TWOS_COMPLEMENT
# define OPPOSITE_SIGNS(x, y) ((x < 0) ^ (y < 0))
# define GOODISUM64(x, y, z) (((x) > 0) ? ((y) < (z)) : ! ((y) < (z)))
# define GOODIDIFF64(x, y, z) (!(OPPOSITE_SIGNS(x, y) && OPPOSITE_SIGNS(x, z)))
#else
# define GOODISUM64(x, y, z) ((long double) (x) + (long double) (y) == (z))
# define GOODIDIFF64(x, y, z) ((long double) (x) - (long double) (y) == (z))
#endif
#define GOODIPROD64(x, y, z) ((long double) (x) * (long double) (y) == (z))
#define INTEGER32_OVERFLOW_WARNING "NAs produced by integer overflow"
#define INTEGER64_OVERFLOW_WARNING "NAs produced by integer64 overflow"
#define INTEGER64_DIVISION_BY_ZERO_WARNING "NAs produced due to division by zero"
#define INTEGER64_NAN_CREATED_WARNING "NaNs produced"
#define INTEGER64_TODOUBLE_WARNING "integer precision lost while converting to double"
#define BITSTRING_OVERFLOW_WARNING "bitstrings longer than 64 bytes converted to NA, multibyte-characters not allowed"

#define PLUS64(e1,e2,ret,naflag) \
	if (e1 == NA_INTEGER64 || e2 == NA_INTEGER64) \
		ret = NA_INTEGER64; \
	else { \
		ret = e1 + e2; \
		if (!GOODISUM64(e1, e2, ret)) \
		  ret = NA_INTEGER64; \
		if (ret == NA_INTEGER64) \
			naflag = TRUE; \
	}
	
#define MINUS64(e1,e2,ret,naflag) \
	if (e1 == NA_INTEGER64 || e2 == NA_INTEGER64) \
		ret = NA_INTEGER64; \
	else { \
		ret = e1 - e2; \
		if (!GOODIDIFF64(e1, e2, ret)) \
		  ret = NA_INTEGER64; \
		if (ret == NA_INTEGER64) \
			naflag = TRUE; \
	}

#define PROD64(e1,e2,ret,naflag) \
	if (e1 == NA_INTEGER64 || e2 == NA_INTEGER64) \
		ret = NA_INTEGER64; \
	else { \
		ret = e1 * e2; \
		if (!GOODIPROD64(e1, e2, ret)) \
		  ret = NA_INTEGER64; \
		if (ret == NA_INTEGER64) \
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

#define POW64(e1,e2,ret,naflag, longret) \
	if (e1 == NA_INTEGER64 || e2 == NA_INTEGER64) \
		ret = NA_INTEGER64; \
	else { \
		longret = pow(e1, (long double) e2); \
		if (isnan(longret)){ \
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
}
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
	}

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
		else \
			ret = e1 - e2 * ret; \
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
		ret = (double) sqrt((long double)e1); \
	}

#define LOG64(e1, ret, naflag) \
	if (e1 == NA_INTEGER64) \
		ret = NA_REAL; \
	else { \
		ret = (double) logl((long double)e1); \
		if (isnan(ret)) \
			naflag = TRUE; \
	}

#define LOGVECT64(e1, e2, ret, naflag) \
	if (e1 == NA_INTEGER64) \
		ret = NA_REAL; \
	else { \
		ret = (double) logl((long double)e1)/log(e2); \
		if (isnan(ret)) \
			naflag = TRUE; \
	}

#define LOGBASE64(e1, e2, ret, naflag) \
	if (e1 == NA_INTEGER64) \
		ret = NA_REAL; \
	else { \
		ret = (double) logl((long double)e1)/e2; \
		if (isnan(ret)) \
			naflag = TRUE; \
	}

#define LOG1064(e1, ret, naflag) \
	if (e1 == NA_INTEGER64) \
		ret = NA_REAL; \
	else { \
		ret =(double)  log10l((long double)e1); \
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


/*****************************************************************************/
/**                                                                         **/
/**                      TYPEDEFS AND STRUCTURES                            **/
/**                                                                         **/
/*****************************************************************************/

/*****************************************************************************/
/**                                                                         **/
/**                        EXPORTED VARIABLES                               **/
/**                                                                         **/
/*****************************************************************************/


#ifndef _INTEGER64_C_SRC

#endif

/*****************************************************************************/
/**                                                                         **/
/**                        EXPORTED FUNCTIONS                               **/
/**                                                                         **/
/*****************************************************************************/


#endif

/*****************************************************************************/
/**                                                                         **/
/**                                EOF                                      **/
/**                                                                         **/
/*****************************************************************************/

