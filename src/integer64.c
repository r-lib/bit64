/*
# C-Code
# S3 atomic 64bit integers for R
# (c) 2011 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2011-12-11
# Last changed:  2011-12-11
#*/

#define _INTEGER64_C_SRC




/*****************************************************************************/
/**                                                                         **/
/**                            MODULES USED                                 **/
/**                                                                         **/
/*****************************************************************************/

// this define before stdio.h removes the warnings
// warning: unknown conversion type character 'l' in format [-Wformat]
// warning: too many arguments for format [-Wformat-extra-args]
#define __USE_MINGW_ANSI_STDIO 1

#include "ctype.h"
#include "stdio.h"
#include <stdint.h>

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>
#include <Rinternals.h>

# include "integer64.h"


/*****************************************************************************/
/**                                                                         **/
/**                      DEFINITIONS AND MACROS                             **/
/**                                                                         **/
/*****************************************************************************/

#define mod_iterate(n1,n2,i1,i2) for (i=i1=i2=0; i<n; i1 = (++i1 == n1) ? 0 : i1, i2 = (++i2 == n2) ? 0 : i2,++i)

/*****************************************************************************/
/**                                                                         **/
/**                      TYPEDEFS AND STRUCTURES                            **/
/**                                                                         **/
/*****************************************************************************/

typedef struct Unsigned32x2TStruct {
  unsigned int low;
  unsigned int high;
} Unsigned32x2T;

/*****************************************************************************/
/**                                                                         **/
/**                   PROTOTYPYPES OF LOCAL FUNCTIONS                       **/
/**                                                                         **/
/*****************************************************************************/

// static


/*****************************************************************************/
/**                                                                         **/
/**                        EXPORTED VARIABLES                               **/
/**                                                                         **/
/*****************************************************************************/

// no static no extern


/*****************************************************************************/
/**                                                                         **/
/**                          GLOBAL VARIABLES                               **/
/**                                                                         **/
/*****************************************************************************/

// static

/*****************************************************************************/
/**                                                                         **/
/**                        EXPORTED FUNCTIONS                               **/
/**                                                                         **/
/*****************************************************************************/

// no extern

SEXP as_integer64_double(SEXP x_, SEXP ret_){
  long long i, n = LENGTH(x_);
  long long * ret = (long long *) REAL(ret_);
  double * x = REAL(x_);
  double imin = (double) MIN_INTEGER64;
  double imax = (double) MAX_INTEGER64;
  Rboolean naflag = FALSE;
  for (i=0; i<n; i++){
    if (ISNAN(x[i])) 
	  ret[i] = NA_INTEGER64;
	else{
	  if (x[i]<imin || x[i]>imax){
	    ret[i] = NA_INTEGER64;
		naflag = TRUE;
	  }else
        ret[i] = (long long) x[i];
	}
  }
  if (naflag)warning(INTEGER64_OVERFLOW_WARNING);
  return ret_;
}

SEXP as_integer64_integer(SEXP x_, SEXP ret_){
  long long i, n = LENGTH(x_);
  long long * ret = (long long *) REAL(ret_);
  int * x = INTEGER(x_); 
  for (i=0; i<n; i++){
    if (x[i]==NA_INTEGER) 
	  ret[i] = NA_INTEGER64;
	else
      ret[i] = (long long) x[i];
  }
  return ret_;
}


SEXP as_double_integer64(SEXP x_, SEXP ret_){
  long long i, n = LENGTH(x_);
  long long * x = (long long *) REAL(x_); 
  double * ret = REAL(ret_);
  double rmax = pow(FLT_RADIX, DBL_MANT_DIG) - 1;
  double rmin = -rmax;
  Rboolean naflag = FALSE;
  for (i=0; i<n; i++){
    if (x[i]==NA_INTEGER64)
      ret[i] = NA_REAL;
	else{
	  if (x[i]<rmin || x[i]>rmax)
		naflag = TRUE;
	  ret[i] = (double) x[i];
	}
  }
  if (naflag)warning(INTEGER64_TODOUBLE_WARNING);
  return ret_;
}

SEXP as_integer_integer64(SEXP x_, SEXP ret_){
  long long i, n = LENGTH(x_);
  long long * x = (long long *) REAL(x_); 
  int * ret = INTEGER(ret_);
  Rboolean naflag = FALSE;
  for (i=0; i<n; i++){
    if (x[i]==NA_INTEGER64)
      ret[i] = NA_INTEGER;
	else{
	  if (x[i]<MIN_INTEGER32 || x[i]>MAX_INTEGER32){
	    ret[i] = NA_INTEGER;
		naflag = TRUE;
	  }else
	    ret[i] = (int) x[i];
	}
  }
  if (naflag)warning(INTEGER32_OVERFLOW_WARNING);
  return ret_;
}

SEXP as_logical_integer64(SEXP x_, SEXP ret_){
  long long i, n = LENGTH(x_);
  long long * x = (long long *) REAL(x_); 
  int * ret = INTEGER(ret_);
  for (i=0; i<n; i++){
    if (x[i]==NA_INTEGER64)
      ret[i] = NA_INTEGER;
	else{
	  ret[i] = x[i]==0 ? 0: 1;
	}
  }
  return ret_;
}


SEXP as_character_integer64(SEXP x_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long * x = (long long *) REAL(x_);
  static char buff[NCHARS_DECS_INTEGER64];
  for(i=0; i<n; i++){
    if (x[i]==NA_INTEGER64){
	  SET_STRING_ELT(ret_, i, NA_STRING);
	}else{
	  snprintf(buff, NCHARS_DECS_INTEGER64, COERCE_INTEGER64, x[i]); 
	  SET_STRING_ELT(ret_, i, mkChar(buff)); 
	}
  }
  return ret_;
}

SEXP as_integer64_character(SEXP x_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long * ret = (long long *) REAL(ret_);
  const char * str;
  char * endpointer;
  for(i=0; i<n; i++){
	str = CHAR(STRING_ELT(x_, i)); endpointer = (char *)str; // thanks to Murray Stokely 28.1.2012
	ret[i] = strtoll(str, &endpointer, 10);
	if (*endpointer)
	  ret[i] = NA_INTEGER64;
  }
  return ret_;
}

SEXP as_bitstring_integer64(SEXP x_, SEXP ret_){
  int i, n = LENGTH(ret_);
  long long * x = (long long *) REAL(x_);
  unsigned long long mask;
  long long v;
  static char buff[NCHARS_BITS_INTEGER64];
  char * str;
  for(i=0; i<n; i++){
	v = x[i];
	str = buff;
	mask = LEFTBIT_INTEGER64;
    while (mask){
        if (v & mask)
              *str = '1';
          else 
              *str = '0';
        str++;
        mask >>= 1;
    }
    *str = 0;
    SET_STRING_ELT(ret_, i, mkChar(buff)); 
    R_CheckUserInterrupt();
  }
  return ret_;
}

SEXP as_integer64_bitstring(SEXP x_, SEXP ret_){
  Rboolean naflag = FALSE;
  int i, k, l, n = LENGTH(x_);
  long long * ret = (long long *) REAL(ret_);
  unsigned long long mask;
  long long v;
  const char * str;
  for(i=0; i<n; i++){
    str = CHAR(STRING_ELT(x_, i));
    l = strlen(str);
    if (l>BITS_INTEGER64){
      ret[i] = NA_INTEGER64;
      naflag = TRUE;
      break;
    }
    mask = 1;
    v = 0;
    for (k=l-1; k>=0; k--){
      if (str[k] != '0' &&  str[k] != ' '){
        v |= mask;
      }
      mask <<= 1;
    }
    ret[i] = v;
    R_CheckUserInterrupt();
  }
  if (naflag)warning(BITSTRING_OVERFLOW_WARNING);
  return ret_;
}



__attribute__((no_sanitize("signed-integer-overflow"))) SEXP plus_integer64(SEXP e1_, SEXP e2_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(e1_);
  long long i2, n2 = LENGTH(e2_);
  long long * e1 = (long long *) REAL(e1_);
  long long * e2 = (long long *) REAL(e2_);
  long long * ret = (long long *) REAL(ret_);
  Rboolean naflag = FALSE;
	mod_iterate(n1, n2, i1, i2) {
		PLUS64(e1[i1],e2[i2],ret[i],naflag)
	}
	if (naflag)warning(INTEGER64_OVERFLOW_WARNING);
  return ret_;
}

__attribute__((no_sanitize("signed-integer-overflow"))) SEXP minus_integer64(SEXP e1_, SEXP e2_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(e1_);
  long long i2, n2 = LENGTH(e2_);
  long long * e1 = (long long *) REAL(e1_);
  long long * e2 = (long long *) REAL(e2_);
  long long * ret = (long long *) REAL(ret_);
  Rboolean naflag = FALSE;
	mod_iterate(n1, n2, i1, i2) {
		MINUS64(e1[i1],e2[i2],ret[i],naflag)
	}
	if (naflag)warning(INTEGER64_OVERFLOW_WARNING);
  return ret_;
}

__attribute__((no_sanitize("signed-integer-overflow"))) SEXP diff_integer64(SEXP x_, SEXP lag_, SEXP n_, SEXP ret_){
  long long i, n = *((long long *) REAL(n_));
  long long * x = (long long *) REAL(x_);
  long long * lag = (long long *) REAL(lag_);
  long long * ret = (long long *) REAL(ret_);
  long long vlag = *lag;
  long long v;
  Rboolean naflag = FALSE;
	for(i=0; i<n; i++) {
	  v = x[i];
	  MINUS64(x[i+vlag],v,ret[i],naflag)
	}
	if (naflag)warning(INTEGER64_OVERFLOW_WARNING);
  return ret_;
}

SEXP intdiv_integer64(SEXP e1_, SEXP e2_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(e1_);
  long long i2, n2 = LENGTH(e2_);
  long long * e1 = (long long *) REAL(e1_);
  long long * e2 = (long long *) REAL(e2_);
  long long * ret = (long long *) REAL(ret_);
  Rboolean naflag = FALSE;
	mod_iterate(n1, n2, i1, i2) {
		INTDIV64(e1[i1],e2[i2],ret[i],naflag)
	}
	if (naflag)warning(INTEGER64_DIVISION_BY_ZERO_WARNING);
  return ret_;
}

SEXP mod_integer64(SEXP e1_, SEXP e2_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(e1_);
  long long i2, n2 = LENGTH(e2_);
  long long * e1 = (long long *) REAL(e1_);
  long long * e2 = (long long *) REAL(e2_);
  long long * ret = (long long *) REAL(ret_);
  Rboolean naflag = FALSE;
	mod_iterate(n1, n2, i1, i2) {
		MOD64(e1[i1],e2[i2],ret[i],naflag)
	}
	if (naflag)warning(INTEGER64_DIVISION_BY_ZERO_WARNING);
  return ret_;
}


__attribute__((no_sanitize("signed-integer-overflow"))) SEXP times_integer64_integer64(SEXP e1_, SEXP e2_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(e1_);
  long long i2, n2 = LENGTH(e2_);
  long long * e1 = (long long *) REAL(e1_);
  long long * e2 = (long long *) REAL(e2_);
  long long * ret = (long long *) REAL(ret_);
  Rboolean naflag = FALSE;
	mod_iterate(n1, n2, i1, i2) {
		PROD64(e1[i1],e2[i2],ret[i],naflag)
	}
	if (naflag)warning(INTEGER64_OVERFLOW_WARNING);
  return ret_;
}

SEXP times_integer64_double(SEXP e1_, SEXP e2_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(e1_);
  long long i2, n2 = LENGTH(e2_);
  long long * e1 = (long long *) REAL(e1_);
  double * e2 = REAL(e2_);
  long long * ret = (long long *) REAL(ret_);
  long double longret;
  Rboolean naflag = FALSE;
	mod_iterate(n1, n2, i1, i2) {
		PROD64REAL(e1[i1],e2[i2],ret[i],naflag,longret)
	}
	if (naflag)warning(INTEGER64_OVERFLOW_WARNING);
  return ret_;
}

SEXP power_integer64_integer64(SEXP e1_, SEXP e2_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(e1_);
  long long i2, n2 = LENGTH(e2_);
  long long * e1 = (long long *) REAL(e1_);
  long long * e2 = (long long *) REAL(e2_);
  long long * ret = (long long *) REAL(ret_);
  long double longret;
  Rboolean naflag = FALSE;
	mod_iterate(n1, n2, i1, i2) {
		POW64(e1[i1],e2[i2],ret[i],naflag, longret)
	}
	if (naflag)warning(INTEGER64_OVERFLOW_WARNING);
  return ret_;
}

SEXP power_integer64_double(SEXP e1_, SEXP e2_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(e1_);
  long long i2, n2 = LENGTH(e2_);
  long long * e1 = (long long *) REAL(e1_);
  double * e2 = REAL(e2_);
  long long * ret = (long long *) REAL(ret_);
  long double longret;
  Rboolean naflag = FALSE;
	mod_iterate(n1, n2, i1, i2) {
		POW64REAL(e1[i1],e2[i2],ret[i],naflag,longret)
	}
	if (naflag)warning(INTEGER64_OVERFLOW_WARNING);
  return ret_;
}

SEXP divide_integer64_integer64(SEXP e1_, SEXP e2_, SEXP ret_){
   long long i, n = LENGTH(ret_);
   long long i1, n1 = LENGTH(e1_);
   long long i2, n2 = LENGTH(e2_);
   long long * e1 = (long long *) REAL(e1_);
   long long * e2 = (long long *) REAL(e2_);
   double * ret = REAL(ret_);
   Rboolean naflag = FALSE;
	 mod_iterate(n1, n2, i1, i2) {
		 DIVIDE64(e1[i1],e2[i2],ret[i],naflag)
	 }
	 if (naflag)warning(INTEGER64_OVERFLOW_WARNING);
   return ret_;
}
SEXP divide_integer64_double(SEXP e1_, SEXP e2_, SEXP ret_){
   long long i, n = LENGTH(ret_);
   long long i1, n1 = LENGTH(e1_);
   long long i2, n2 = LENGTH(e2_);
   long long * e1 = (long long *) REAL(e1_);
   double * e2 = REAL(e2_);
   double * ret = REAL(ret_);
   Rboolean naflag = FALSE;
	 mod_iterate(n1, n2, i1, i2) {
		 DIVIDE64REAL(e1[i1],e2[i2],ret[i],naflag)
	 }
	 if (naflag)warning(INTEGER64_OVERFLOW_WARNING);
   return ret_;
}

SEXP divide_double_integer64(SEXP e1_, SEXP e2_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(e1_);
  long long i2, n2 = LENGTH(e2_);
  long long * e2 = (long long *) REAL(e2_);
  double * e1 = REAL(e1_);
  double * ret = REAL(ret_);
  Rboolean naflag = FALSE;
  mod_iterate(n1, n2, i1, i2) {
    DIVIDEREAL64(e1[i1],e2[i2],ret[i],naflag)
  }
  if (naflag)warning(INTEGER64_OVERFLOW_WARNING);
  return ret_;
}

SEXP sign_integer64(SEXP e1_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long * e1 = (long long *) REAL(e1_);
  long long * ret = (long long *) REAL(ret_);
	for(i=0; i<n; i++) {
		SIGN64(e1[i],ret[i])
	}
  return ret_;
}

SEXP abs_integer64(SEXP e1_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long * e1 = (long long *) REAL(e1_);
  long long * ret = (long long *) REAL(ret_);
	for(i=0; i<n; i++) {
		ABS64(e1[i],ret[i])
	}
  return ret_;
}

SEXP sqrt_integer64(SEXP e1_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long * e1 = (long long *) REAL(e1_);
  double * ret = REAL(ret_);
  Rboolean naflag = FALSE;
  for(i=0; i<n; i++) {
	SQRT64(e1[i],ret[i],naflag)
  }
  if (naflag)warning(INTEGER64_NAN_CREATED_WARNING);
  return ret_;
}

SEXP log_integer64(SEXP e1_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long * e1 = (long long *) REAL(e1_);
  double * ret = REAL(ret_);
  Rboolean naflag = FALSE;
	for(i=0; i<n; i++) {
		LOG64(e1[i],ret[i],naflag)
	}
	if (naflag)warning(INTEGER64_NAN_CREATED_WARNING);
  return ret_;
}

SEXP logvect_integer64(SEXP e1_, SEXP e2_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(e1_);
  long long i2, n2 = LENGTH(e2_);
  long long * e1 = (long long *) REAL(e1_);
  double * e2 = REAL(e2_);
  double * ret = REAL(ret_);
  Rboolean naflag = FALSE;
	mod_iterate(n1, n2, i1, i2) {
		LOGVECT64(e1[i],e2[i],ret[i],naflag)
	}
	if (naflag)warning(INTEGER64_NAN_CREATED_WARNING);
  return ret_;
}

SEXP logbase_integer64(SEXP e1_, SEXP base_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long * e1 = (long long *) REAL(e1_);
  long double logbase = (long double) log(asReal(base_));
  double * ret = REAL(ret_);
  Rboolean naflag = (asReal(base_)>0) ? FALSE : TRUE;
	for(i=0; i<n; i++) {
		LOGBASE64(e1[i],logbase,ret[i],naflag)
	}
	if (naflag)warning(INTEGER64_NAN_CREATED_WARNING);
  return ret_;
}

SEXP log10_integer64(SEXP e1_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long * e1 = (long long *) REAL(e1_);
  double * ret = REAL(ret_);
  Rboolean naflag = FALSE;
#ifdef HAVE_LOG10
	for(i=0; i<n; i++) {
		LOG1064(e1[i],ret[i],naflag)
	}
#else
  long double logbase = (long double) log(10);
  for(i=0; i<n; i++) {
	LOGBASE64(e1[i],logbase,ret[i],naflag)
  }
#endif	
  if (naflag)warning(INTEGER64_NAN_CREATED_WARNING);
  return ret_;
}

SEXP log2_integer64(SEXP e1_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long * e1 = (long long *) REAL(e1_);
  double * ret = REAL(ret_);
  Rboolean naflag = FALSE;
#ifdef HAVE_LOG2
	for(i=0; i<n; i++) {
		LOG264(e1[i],ret[i],naflag)
	}
#else
  long double logbase = (long double) log(2);
  for(i=0; i<n; i++) {
	LOGBASE64(e1[i],logbase,ret[i],naflag)
  }
#endif	
  if (naflag)warning(INTEGER64_NAN_CREATED_WARNING);
  return ret_;
}

SEXP any_integer64(SEXP e1_, SEXP na_rm_, SEXP ret_){
  long long i, n = LENGTH(e1_);
  long long * e1 = (long long *) REAL(e1_);
  Rboolean * ret = (Rboolean *) LOGICAL(ret_);
  Rboolean hasna=FALSE;
	if (asLogical(na_rm_)){
		for(i=0; i<n; i++){
			if (e1[i]!=NA_INTEGER64 && e1[i]){
				ret[0] = TRUE;
				return ret_;
			}
		}
		ret[0] = FALSE;
	}else{
		for(i=0; i<n; i++){
			if (e1[i]==NA_INTEGER64){
				hasna = TRUE;
			}else if (e1[i]){
				ret[0] = TRUE;
				return ret_;
			}
		}
		ret[0] = hasna ? NA_LOGICAL : FALSE;
	}
  return ret_;
}

SEXP all_integer64(SEXP e1_, SEXP na_rm_, SEXP ret_){
  long long i, n = LENGTH(e1_);
  long long * e1 = (long long *) REAL(e1_);
  Rboolean * ret = (Rboolean *) LOGICAL(ret_);
  Rboolean hasna=FALSE;
	if (asLogical(na_rm_)){
		for(i=0; i<n; i++){
			if (e1[i]!=NA_INTEGER64 && !e1[i]){
				ret[0] = FALSE;
				return ret_;
			}
		}
		ret[0] = TRUE;
	}else{
		for(i=0; i<n; i++){
			if (e1[i]==NA_INTEGER64){
				hasna = TRUE;
			}else if (!e1[i]){
				ret[0] = FALSE;
				return ret_;
			}
		}
		ret[0] = hasna ? NA_LOGICAL : TRUE;
	}
  return ret_;
}


SEXP sum_integer64(SEXP e1_, SEXP na_rm_, SEXP ret_){
  long long i, n = LENGTH(e1_);
  long long * e1 = (long long *) REAL(e1_);
  long long * ret = (long long *) REAL(ret_);
  long long cumsum, tempsum;
  cumsum = 0;
	if (asLogical(na_rm_)){
		for(i=0; i<n; i++){
			if (e1[i]!=NA_INTEGER64){
				tempsum = cumsum + e1[i];
				if (!GOODISUM64(cumsum, e1[i], tempsum)){
					warning(INTEGER64_OVERFLOW_WARNING);
					ret[0] = NA_INTEGER64;
					return ret_;
				}
				cumsum = tempsum;
			}
		}
	}else{
		for(i=0; i<n; i++){
			if (e1[i]==NA_INTEGER64){
				ret[0] = NA_INTEGER64;
				return ret_;
			}else{
				tempsum = cumsum + e1[i];
				if (!GOODISUM64(cumsum, e1[i], tempsum)){
					warning(INTEGER64_OVERFLOW_WARNING);
					ret[0] = NA_INTEGER64;
					return ret_;
				}
				cumsum = tempsum;
			}
		}
	}
  ret[0] = cumsum;
  return ret_;
}

SEXP mean_integer64(SEXP e1_, SEXP na_rm_, SEXP ret_){
	long long i, n = LENGTH(e1_);
	long long * e1 = (long long *) REAL(e1_);
	long long * ret = (long long *) REAL(ret_);
	long double longret = 0;
	if (asLogical(na_rm_)){
		long long nvalid = 0;
		for(i=0; i<n; i++){
			if (e1[i]!=NA_INTEGER64){
				longret += e1[i];
				nvalid++;
			}
		}
		ret[0] = longret / nvalid;
	}else{
		for(i=0; i<n; i++){
			if (e1[i]==NA_INTEGER64){
				ret[0] = NA_INTEGER64;
				return ret_;
			}else{
				longret += e1[i];			
			}
		}
		ret[0] = longret / n;
	}
  return ret_;
}

SEXP prod_integer64(SEXP e1_, SEXP na_rm_, SEXP ret_){
  long long i, n = LENGTH(e1_);
  long long * e1 = (long long *) REAL(e1_);
  long long * ret = (long long *) REAL(ret_);
  long long cumprod, tempprod;
  cumprod = 1;
	if (asLogical(na_rm_)){
		for(i=0; i<n; i++){
			if (e1[i]!=NA_INTEGER64){
				tempprod = cumprod * e1[i];
				if (!GOODIPROD64(cumprod, e1[i], tempprod)){
					warning(INTEGER64_OVERFLOW_WARNING);
					ret[0] = NA_INTEGER64;
					return ret_;
				}
				cumprod = tempprod;
			}
		}
	}else{
		for(i=0; i<n; i++){
			if (e1[i]==NA_INTEGER64){
				ret[0] = NA_INTEGER64;
				return ret_;
			}else{
				tempprod = cumprod * e1[i];
				if (!GOODIPROD64(cumprod, e1[i], tempprod)){
					warning(INTEGER64_OVERFLOW_WARNING);
					ret[0] = NA_INTEGER64;
					return ret_;
				}
				cumprod = tempprod;
			}
		}
	}
  ret[0] = cumprod;
  return ret_;
}


SEXP min_integer64(SEXP e1_, SEXP na_rm_, SEXP ret_){
  long long i, n = LENGTH(e1_);
  long long * e1 = (long long *) REAL(e1_);
  long long * ret = (long long *) REAL(ret_);
  ret[0] = MAX_INTEGER64;
	if (asLogical(na_rm_)){
		for(i=0; i<n; i++){
			if (e1[i]!=NA_INTEGER64 && e1[i]<ret[0]){
				ret[0] = e1[i];
			}
		}
	}else{
		for(i=0; i<n; i++){
			if (e1[i]==NA_INTEGER64){
				ret[0] = NA_INTEGER64;
				return ret_;
			}else{
				if (e1[i]<ret[0])
					ret[0] = e1[i];
			}
		}
	}
  return ret_;
}

SEXP max_integer64(SEXP e1_, SEXP na_rm_, SEXP ret_){
  long long i, n = LENGTH(e1_);
  long long * e1 = (long long *) REAL(e1_);
  long long * ret = (long long *) REAL(ret_);
  ret[0] = MIN_INTEGER64;
	if (asLogical(na_rm_)){
		for(i=0; i<n; i++){
			if (e1[i]!=NA_INTEGER64 && e1[i]>ret[0]){
				ret[0] = e1[i];
			}
		}
	}else{
		for(i=0; i<n; i++){
			if (e1[i]==NA_INTEGER64){
				ret[0] = NA_INTEGER64;
				return ret_;
			}else{
				if (e1[i]>ret[0])
					ret[0] = e1[i];
			}
		}
	}
  return ret_;
}

SEXP range_integer64(SEXP e1_, SEXP na_rm_, SEXP ret_){
  long long i, n = LENGTH(e1_);
  long long * e1 = (long long *) REAL(e1_);
  long long * ret = (long long *) REAL(ret_);
  ret[0] = MAX_INTEGER64;
  ret[1] = MIN_INTEGER64;
	if (asLogical(na_rm_)){
		for(i=0; i<n; i++){
			if (e1[i]!=NA_INTEGER64){
				if (e1[i]<ret[0])
					ret[0] = e1[i];
				if (e1[i]>ret[1])
					ret[1] = e1[i];
			}
		}
	}else{
		for(i=0; i<n; i++){
			if (e1[i]==NA_INTEGER64){
				ret[0] = ret[1] = NA_INTEGER64;
				return ret_;
			}else{
				if (e1[i]<ret[0])
					ret[0] = e1[i];
				if (e1[i]>ret[1])
					ret[1] = e1[i];
			}
		}
	}
  return ret_;
}

SEXP lim_integer64(SEXP ret_){
  long long * ret = (long long *) REAL(ret_);
  ret[0] = MIN_INTEGER64;
  ret[1] = MAX_INTEGER64;
  return ret_;
}


SEXP cummin_integer64(SEXP e1_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long * e1 = (long long *) REAL(e1_);
  long long * ret = (long long *) REAL(ret_);
  if (n>0){
	i=0;
	ret[i] = e1[i];
	if(e1[i]!=NA_INTEGER64)
	for(i=1; i<n; i++){
		if(e1[i]==NA_INTEGER64){
			ret[i] = e1[i];
			break;
		}else{
			ret[i] = e1[i]<ret[i-1] ? e1[i] : ret[i-1];		
		}
	}
	for(i++; i<n; i++){
		ret[i] = NA_INTEGER64;
	}
  }
  return ret_;
}

SEXP cummax_integer64(SEXP e1_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long * e1 = (long long *) REAL(e1_);
  long long * ret = (long long *) REAL(ret_);
  if (n>0){
	i=0;
	ret[i] = e1[i];
	if(e1[i]!=NA_INTEGER64)
	for(i=1; i<n; i++){
		if(e1[i]==NA_INTEGER64){
			ret[i] = e1[i];
			break;
		}else{
			ret[i] = e1[i]>ret[i-1] ? e1[i] : ret[i-1];		
		}
	}
	for(i++; i<n; i++){
		ret[i] = NA_INTEGER64;
	}
  }
  return ret_;
}

SEXP cumsum_integer64(SEXP e1_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long * e1 = (long long *) REAL(e1_);
  long long * ret = (long long *) REAL(ret_);
  Rboolean naflag = FALSE;
    if (n>0)
	  ret[0] = e1[0];
	for(i=1; i<n; i++) {
		PLUS64(e1[i],ret[i-1],ret[i],naflag)
	}
	if (naflag)warning(INTEGER64_OVERFLOW_WARNING);
  return ret_;
}

SEXP cumprod_integer64(SEXP e1_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long * e1 = (long long *) REAL(e1_);
  long long * ret = (long long *) REAL(ret_);
  Rboolean naflag = FALSE;
    if (n>0)
	  ret[0] = e1[0];
	for(i=1; i<n; i++) {
		PROD64(e1[i],ret[i-1],ret[i],naflag)
	}
	if (naflag)warning(INTEGER64_OVERFLOW_WARNING);
  return ret_;
}

SEXP seq_integer64(SEXP from_, SEXP by_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long * from = (long long *) REAL(from_);
  long long * by1 = (long long *) REAL(by_);
  long long by = by1[0];
  long long * ret = (long long *) REAL(ret_);
  if (n>0){
    ret[0] = from[0];
	for(i=1; i<n; i++){
		ret[i] = ret[i-1] + by;
	}
  }
  return ret_;
}

SEXP isna_integer64(SEXP e1_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long * e1 = (long long *) REAL(e1_);
  Rboolean * ret = (Rboolean *) LOGICAL(ret_);
	for(i=0; i<n; i++) {
		ret[i] = (e1[i]==NA_INTEGER64) ? TRUE : FALSE;
	}
  return ret_;
}



SEXP EQ_integer64(SEXP e1_, SEXP e2_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(e1_);
  long long i2, n2 = LENGTH(e2_);
  long long * e1 = (long long *) REAL(e1_);
  long long * e2 = (long long *) REAL(e2_);
  Rboolean * ret = (Rboolean *) LOGICAL(ret_);
	mod_iterate(n1, n2, i1, i2) {
		EQ64(e1[i1],e2[i2],ret[i])
	}
  return ret_;
}

SEXP NE_integer64(SEXP e1_, SEXP e2_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(e1_);
  long long i2, n2 = LENGTH(e2_);
  long long * e1 = (long long *) REAL(e1_);
  long long * e2 = (long long *) REAL(e2_);
  Rboolean * ret = (Rboolean *) LOGICAL(ret_);
	mod_iterate(n1, n2, i1, i2) {
		NE64(e1[i1],e2[i2],ret[i])
	}
  return ret_;
}

SEXP LT_integer64(SEXP e1_, SEXP e2_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(e1_);
  long long i2, n2 = LENGTH(e2_);
  long long * e1 = (long long *) REAL(e1_);
  long long * e2 = (long long *) REAL(e2_);
  Rboolean * ret = (Rboolean *) LOGICAL(ret_);
	mod_iterate(n1, n2, i1, i2) {
		LT64(e1[i1],e2[i2],ret[i])
	}
  return ret_;
}

SEXP LE_integer64(SEXP e1_, SEXP e2_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(e1_);
  long long i2, n2 = LENGTH(e2_);
  long long * e1 = (long long *) REAL(e1_);
  long long * e2 = (long long *) REAL(e2_);
  Rboolean * ret = (Rboolean *) LOGICAL(ret_);
	mod_iterate(n1, n2, i1, i2) {
		LE64(e1[i1],e2[i2],ret[i])
	}
  return ret_;
}

SEXP GT_integer64(SEXP e1_, SEXP e2_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(e1_);
  long long i2, n2 = LENGTH(e2_);
  long long * e1 = (long long *) REAL(e1_);
  long long * e2 = (long long *) REAL(e2_);
  Rboolean * ret = (Rboolean *) LOGICAL(ret_);
	mod_iterate(n1, n2, i1, i2) {
		GT64(e1[i1],e2[i2],ret[i])
	}
  return ret_;
}

SEXP GE_integer64(SEXP e1_, SEXP e2_, SEXP ret_){
  long long i, n = LENGTH(ret_);
  long long i1, n1 = LENGTH(e1_);
  long long i2, n2 = LENGTH(e2_);
  long long * e1 = (long long *) REAL(e1_);
  long long * e2 = (long long *) REAL(e2_);
  Rboolean * ret = (Rboolean *) LOGICAL(ret_);
	mod_iterate(n1, n2, i1, i2) {
		GE64(e1[i1],e2[i2],ret[i])
	}
  return ret_;
}

SEXP runif_integer64(SEXP n_, SEXP min_, SEXP max_){
  int i, n=asInteger(n_);
  long long min = *((long long * ) REAL(min_));
  long long max = *((long long * ) REAL(max_));
  unsigned long long d;
  // max - min can overflow 
  if (min < 0 && max > 0){
     d = ((unsigned long long)(-min)) + ((unsigned long long)max) + 1;
  }else{
    d = (max - min) + 1;
  }
  SEXP ret_;
  PROTECT(ret_ = allocVector(REALSXP, n));
  long long * ret = (long long *) REAL(ret_);
  Unsigned32x2T ii;
  GetRNGstate();
  for (i=0; i<n; i++){
    ii.low = (unsigned int) floor(unif_rand()*4294967296);
    ii.high = (unsigned int) floor(unif_rand()*4294967296);
//#pragma GCC diagnostic push
//#pragma GCC diagnostic ignored "-Wstrict-aliasing"
    while( (*((long long *) &ii)) == NA_INTEGER64){
//#pragma GCC diagnostic pop
      // xx optimisation opportunity: if we know endianess, we only need to replace one of the two
      ii.low = (unsigned int) floor(unif_rand()*4294967296);
      ii.high = (unsigned int) floor(unif_rand()*4294967296);
    }
//#pragma GCC diagnostic push
//#pragma GCC diagnostic ignored "-Wstrict-aliasing"
    ret[i] = min + ( (long long)(*((unsigned long long *)(&ii))) % d);
//#pragma GCC diagnostic pop
  }
  PutRNGstate();  
  UNPROTECT(1);
  return ret_;
}

/*
require(bit64)
require(microbenchmark)
microbenchmark(runif64(1e6))

sort(runif64(1e2))


Unit: milliseconds
           expr      min       lq     mean   median       uq      max neval
 runif64(1e+06) 24.62306 25.60286 25.61903 25.61369 25.62032 26.40202   100
*/


/*****************************************************************************/
/**                                                                         **/
/**                           LOCAL FUNCTIONS                               **/
/**                                                                         **/
/*****************************************************************************/

// static


/*****************************************************************************/
/**                                                                         **/
/**                                EOF                                      **/
/**                                                                         **/
/*****************************************************************************/



	

