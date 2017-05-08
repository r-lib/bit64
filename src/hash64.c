/*
# C-Code for hashing and matching
# S3 atomic 64bit integers for R
# (c) 2011 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2011-12-11
# Last changed:  2012-10-22
#*/

/* for speed (should not really matter in this case as most time is spent in the hashing) */
// #define USE_RINTERNALS 1
#include <Rinternals.h>
#include <R.h>

//#include "timing.h"

// This multiplicator was used in Simon Urbanek's package fastmatch for 32-bit integers
//#define HASH64(X, SHIFT) (314159265358979323ULL * ((unsigned long long)(X)) >> (SHIFT))
// This multiplicator seems to work fine with 64bit integers
#define HASH64(X, SHIFT) (0x9e3779b97f4a7c13ULL * ((unsigned long long)(X)) >> (SHIFT))

SEXP hashfun_integer64(SEXP x_, SEXP bits_, SEXP ret_){
  int i, n = LENGTH(x_);
  long long * x = (long long *) REAL(x_);
  unsigned int * ret = (unsigned int *) INTEGER(ret_);
  int shift = 64 - asInteger(bits_);
  for(i=0; i<n; i++){
	ret[i] = (unsigned int) HASH64(x[i], shift);
  }
  return ret_;
}

// this function is loosely following Simon Urbanek's package 'fastmatch'
SEXP hashmap_integer64(SEXP x_, SEXP bits_, SEXP hashpos_, SEXP nunique_){
  int i, nx = LENGTH(x_);
  int h, nh = LENGTH(hashpos_);
  long long * x = (long long *) REAL(x_);
  unsigned int * hashpos = (unsigned int *) INTEGER(hashpos_);
  int bits = asInteger(bits_);
  int shift = 64 - bits;
  long long v;
  int nunique = 0;
  for(i=0; i<nx; ){
    v = x[i++];
	h = HASH64(v, shift);
	while (hashpos[h] && x[hashpos[h] - 1] != v){
		h++;
		if (h == nh) 
			h = 0;
	  }
	  if (!hashpos[h]){
      hashpos[h] = i;
      nunique++;
	  }
  }
  INTEGER(nunique_)[0] = nunique;
  return R_NilValue;
}

SEXP hashpos_integer64(SEXP x_, SEXP hashdat_, SEXP bits_, SEXP hashpos_, SEXP nomatch_, SEXP ret_){
  int i, nx = LENGTH(x_);
  int h, nh = LENGTH(hashpos_);
  long long * x = (long long *) REAL(x_);
  long long * hashdat = (long long *) REAL(hashdat_);
  unsigned int * hashpos = (unsigned int *) INTEGER(hashpos_);
  int * ret = INTEGER(ret_);
  int bits = asInteger(bits_);
  int shift = 64 - bits;
  int nomatch = asInteger(nomatch_);
  long long v;
  for(i=0; i<nx; i++){
    v = x[i];
	h = HASH64(v, shift);
    for(;;){
	  if (hashpos[h]){  // this is mostly while(hashpos[h]) but we want to catch failure for the nomatch assignment
		  if (hashdat[hashpos[h] - 1] == v){
			ret[i] = hashpos[h];
			break;
		  }
		  h++;
		  if (h == nh) 
			h = 0;
	  }else{
	    ret[i] = nomatch;
		break;
	  }
	}
  }
  return R_NilValue;
}

SEXP hashrev_integer64(SEXP x_, SEXP hashdat_, SEXP bits_, SEXP hashpos_, SEXP nunique_, SEXP nomatch_, SEXP ret_){
  int i, nx = LENGTH(x_);
  int h, nh = LENGTH(hashpos_);
  int nd = LENGTH(hashdat_);
  long long * x = (long long *) REAL(x_);
  long long * hashdat = (long long *) REAL(hashdat_);
  unsigned int * hashpos = (unsigned int *) INTEGER(hashpos_);
  int * ret = INTEGER(ret_);
  int bits = asInteger(bits_);
  int shift = 64 - bits;
  int nomatch = asInteger(nomatch_);
  int nunique = asInteger(nunique_);
  int iunique=0;
  long long v;
  for(i=0; i<nx; ){
	v = x[i++];
	h = HASH64(v, shift);
	while(hashpos[h]){
	  if (hashdat[hashpos[h] - 1] == v){
	    h = hashpos[h] - 1;
		if (!ret[h]){
			ret[h] = i;
			if (++iunique==nunique)
			  i=nx; // break out of for as well
		}
		break;
	  }
	  h++;
	  if (h == nh) 
		h = 0;
	}
  }
  if (iunique<nd){
    if (nunique<nd){ // some gaps are duplicates
	  for(i=0; i<nd; i++){
	    if (!ret[i]){
			v = hashdat[i];
			h = HASH64(v, shift);
			while(hashpos[h]){  // this is mostly while(hashpos[h]) but we want to catch failure for the nomatch assignment
			  if (hashdat[hashpos[h] - 1] == v){
			    h = ret[hashpos[h] - 1];
				if (h)
				  ret[i] = h;
				else
				  ret[i] = nomatch;
				break;
			  }
			  h++;
			  if (h == nh) 
				h = 0;
			}
		}
	  }
	}else{ // no duplicates: all gaps are nomatches
	  for(i=0; i<nd; i++)
	    if (!ret[i])
		  ret[i] = nomatch;
	}
  }
  return R_NilValue;
}

SEXP hashrin_integer64(SEXP x_, SEXP hashdat_, SEXP bits_, SEXP hashpos_, SEXP nunique_, SEXP ret_){
  int i, nx = LENGTH(x_);
  int h, nh = LENGTH(hashpos_);
  int nd = LENGTH(hashdat_);
  long long * x = (long long *) REAL(x_);
  long long * hashdat = (long long *) REAL(hashdat_);
  unsigned int * hashpos = (unsigned int *) INTEGER(hashpos_);
  int * ret = INTEGER(ret_);
  int bits = asInteger(bits_);
  int shift = 64 - bits;
  int nunique = asInteger(nunique_);
  int iunique=0;
  long long v;
  for(i=0; i<nx; ){
	v = x[i++];
	h = HASH64(v, shift);
	while(hashpos[h]){
	  if (hashdat[hashpos[h] - 1] == v){
	    h = hashpos[h] - 1;
		if (!ret[h]){
			ret[h] = TRUE;
			if (++iunique==nunique)
			  i=nx; // break out of for as well
		}
		break;
	  }
	  h++;
	  if (h == nh) 
		h = 0;
	}
  }
    if (nunique<nd){ // some gaps are duplicates
	  for(i=0; i<nd; i++){
	    if (!ret[i]){
			v = hashdat[i];
			h = HASH64(v, shift);
			while(hashpos[h]){  // this is mostly while(hashpos[h]) but we want to catch failure for the nomatch assignment
			  if (hashdat[hashpos[h] - 1] == v){
			    h = ret[hashpos[h] - 1];
				if (h)
				  ret[i] = TRUE;
				break;
			  }
			  h++;
			  if (h == nh) 
				h = 0;
			}
		}
	  }
	}
  return R_NilValue;
}

SEXP hashfin_integer64(SEXP x_, SEXP hashdat_, SEXP bits_, SEXP hashpos_, SEXP ret_){
  int i, nx = LENGTH(x_);
  int h, nh = LENGTH(hashpos_);
  long long * x = (long long *) REAL(x_);
  long long * hashdat = (long long *) REAL(hashdat_);
  unsigned int * hashpos = (unsigned int *) INTEGER(hashpos_);
  int * ret = LOGICAL(ret_);
  int bits = asInteger(bits_);
  int shift = 64 - bits;
  long long v;
  for(i=0; i<nx; i++){
    v = x[i];
	h = HASH64(v, shift);
    for(;;){
	  if (hashpos[h]){  // this is mostly while(hashpos[h]) but we want to catch failure for the nomatch assignment
		  if (hashdat[hashpos[h] - 1] == v){
			ret[i] = TRUE;
			break;
		  }
		  h++;
		  if (h == nh) 
			h = 0;
	  }else{
	    ret[i] = FALSE;
		break;
	  }
	}
  }
  return R_NilValue;
}

SEXP hashdup_integer64(SEXP hashdat_, SEXP bits_, SEXP hashpos_, SEXP nunique_, SEXP ret_){
  int nu = LENGTH(ret_);
  int h, nh = LENGTH(hashpos_);
  //long long * hashdat = (long long *) REAL(hashdat_);
  unsigned int * hashpos = (unsigned int *) INTEGER(hashpos_);
  int * ret = LOGICAL(ret_);
  int nunique = asInteger(nunique_);
  for(h=0; h<nu; h++)
	ret[h] = TRUE;
  for(h=0; h<nh; h++)
    if (hashpos[h]>0){
	  ret[hashpos[h]-1] = FALSE;
	  nunique--;
	  if (nunique<1)
	    break;
	}
  return R_NilValue;
}

SEXP hashuni_integer64(SEXP hashdat_, SEXP bits_, SEXP hashpos_, SEXP keep_order_, SEXP ret_){
  int h, nh = LENGTH(hashpos_);
  int u, nu = LENGTH(ret_);
  long long * hashdat = (long long *) REAL(hashdat_);
  unsigned int * hashpos = (unsigned int *) INTEGER(hashpos_);
  long long * ret = (long long *) REAL(ret_);
  if (asLogical(keep_order_)){
      int i;
	  // int nx = LENGTH(hashdat_);
	  int bits = asInteger(bits_);
	  int shift = 64 - bits;
	  long long v;
	  for(u=0,i=0; u<nu; i++){
		v = hashdat[i];
		h = HASH64(v, shift);
		while(hashpos[h] && hashdat[hashpos[h] - 1] != v){  // this is mostly while(hashpos[h]) but we want to catch failure for the nomatch assignment
		  h++;
		  if (h == nh) 
			h = 0;
		}
		if (i == (hashpos[h] - 1)){
		  ret[u++] = v; /* unique */
		}
	  }
  }else{
	  for(u=0,h=0; u<nu; h++)
		if (hashpos[h]>0){
		  ret[u++] = hashdat[hashpos[h]-1];
		}
  }
  return R_NilValue;
}

SEXP hashmapuni_integer64(SEXP x_, SEXP bits_, SEXP hashpos_, SEXP nunique_){
  int i, nx = LENGTH(x_);
  int h, nh = LENGTH(hashpos_);
  int nu = 0;
  SEXP ret_;
  PROTECT_INDEX idx;
  PROTECT_WITH_INDEX(ret_ = allocVector(REALSXP, nx), &idx);
  long long * ret = (long long *) REAL(ret_);
  long long * x = (long long *) REAL(x_);
  unsigned int * hashpos = (unsigned int *) INTEGER(hashpos_);
  int bits = asInteger(bits_);
  int shift = 64 - bits;
  long long v;
  for(i=0; i<nx; ){
	v = x[i++];
	h = HASH64(v, shift);
	while(hashpos[h] && x[hashpos[h] - 1] != v){
		h++;
		if (h == nh) 
			h = 0;
	}
	if (!hashpos[h]){
		hashpos[h] = i;
		ret[nu++] = v;
	}
  }
  INTEGER(nunique_)[0] = nu;
  REPROTECT(ret_ = lengthgets(ret_, nu), idx);
  UNPROTECT(1);
  return ret_;
}


SEXP hashupo_integer64(SEXP hashdat_, SEXP bits_, SEXP hashpos_, SEXP keep_order_, SEXP ret_){
  int h, nh = LENGTH(hashpos_);
  int u, nu = LENGTH(ret_);
  long long * hashdat = (long long *) REAL(hashdat_);
  unsigned int * hashpos = (unsigned int *) INTEGER(hashpos_);
  int * ret = INTEGER(ret_);
  if (asLogical(keep_order_)){
      int i;
	  // int nx = LENGTH(hashdat_);
	  int bits = asInteger(bits_);
	  int shift = 64 - bits;
	  long long v;
	  for(u=0,i=0; u<nu; i++){
		v = hashdat[i];
		h = HASH64(v, shift);
		while(hashpos[h] && hashdat[hashpos[h] - 1] != v){  // this is mostly while(hashpos[h]) but we want to catch failure for the nomatch assignment
		  h++;
		  if (h == nh) 
			h = 0;
		}
		if (i == (hashpos[h] - 1)){
		  ret[u++] = hashpos[h]; /* unique */
		}
	  }
  }else{
	  for(u=0,h=0; u<nu; h++)
		if (hashpos[h]>0){
		  ret[u++] = hashpos[h];
		}
  }
  return R_NilValue;
}

SEXP hashmapupo_integer64(SEXP x_, SEXP bits_, SEXP hashpos_, SEXP nunique_){
  int i, nx = LENGTH(x_);
  int h, nh = LENGTH(hashpos_);
  int nu = 0;
  SEXP ret_;
  PROTECT_INDEX idx;
  PROTECT_WITH_INDEX(ret_ = allocVector(INTSXP, nx), &idx);
  int * ret = INTEGER(ret_);
  long long * x = (long long *) REAL(x_);
  unsigned int * hashpos = (unsigned int *) INTEGER(hashpos_);
  int bits = asInteger(bits_);
  int shift = 64 - bits;
  long long v;
  for(i=0; i<nx; ){
	v = x[i++];
	h = HASH64(v, shift);
	while(hashpos[h] && x[hashpos[h] - 1] != v){
		h++;
		if (h == nh) 
			h = 0;
	}
	if (!hashpos[h]){
		hashpos[h] = i;
		ret[nu++] = hashpos[h];
	}
  }
  INTEGER(nunique_)[0] = nu;
  REPROTECT(ret_ = lengthgets(ret_, nu), idx);
  UNPROTECT(1);
  return ret_;
}



SEXP hashtab_integer641(SEXP hashdat_, SEXP bits_, SEXP hashpos_, SEXP nunique_){
  int i, nx = LENGTH(hashdat_);
  int h, nh = LENGTH(hashpos_);
  int u;
  long long * hashdat = (long long *) REAL(hashdat_);
  unsigned int * hashpos = (unsigned int *) INTEGER(hashpos_);
  //int * pos = INTEGER(pos_);
  SEXP ret_;
  PROTECT_INDEX idx;
  PROTECT_WITH_INDEX(ret_ = allocVector(INTSXP, nh), &idx);
  int * ret = INTEGER(ret_);
  int bits = asInteger(bits_);
  int shift = 64 - bits;
  long long v;
  for(i=0; i<nh; i++)
	ret[i]=0;
  for(i=0; i<nx; i++){
	v = hashdat[i];
	h = HASH64(v, shift);
	while(hashpos[h]){  // this is mostly while(hashpos[h]) but we want to catch failure for the nomatch assignment
	  if (hashdat[hashpos[h] - 1] == v){
	    ret[h]++;
		break;
	  }
	  h++;
	  if (h == nh) 
		h = 0;
	}
  }
  for (u=0,h=0;h<nh;h++){
    if (hashpos[h]){
	  //pos[u]=hashpos[h];
	  ret[u++]=ret[h];
	}
  }
  REPROTECT(ret_ = lengthgets(ret_, u), idx);
  UNPROTECT(1);
  return ret_;
}



SEXP hashtab_integer64(SEXP x_, SEXP bits_, SEXP hashpos_, SEXP nunique_){
  int i, nx = LENGTH(x_);
  int h, nh = LENGTH(hashpos_);
  long long * x = (long long *) REAL(x_);
  unsigned int * hashpos = (unsigned int *) INTEGER(hashpos_);
  SEXP hashtab_;
  PROTECT_INDEX idx;
  PROTECT_WITH_INDEX(hashtab_ = allocVector(INTSXP, nh), &idx);
  int *hashtab = INTEGER(hashtab_);
  int bits = asInteger(bits_);
  int shift = 64 - bits;
  long long v;
  int u, nu = INTEGER(nunique_)[0];

  for(i=0; i<nh; i++)
	hashtab[i]=0;
  for(i=0; i<nx; ){
    v = x[i++];
	h = HASH64(v, shift);
	while (hashpos[h] && x[hashpos[h] - 1] != v){
		h++;
		if (h == nh) 
			h = 0;
	}
	hashtab[h]++;
  }
  SEXP tabval_;
  PROTECT(tabval_ = allocVector(REALSXP, nu));
  long long * tabval = (long long *) REAL(tabval_);
  for (u=0,h=0;u<nu;h++){
    if (hashpos[h]){
	  tabval[u] = x[hashpos[h]-1];
	  hashtab[u]=hashtab[h];
	  u++;
	}
  }
  REPROTECT(hashtab_ = lengthgets(hashtab_, nu), idx);
  
  SEXP class;
  PROTECT(class = allocVector(STRSXP, 1));
  SET_STRING_ELT(class, 0, mkChar("integer64"));
  classgets(tabval_, class);
  
  SEXP ret_;
  PROTECT(ret_ = allocVector(VECSXP, 2));
  SET_VECTOR_ELT(ret_, 0, tabval_);
  SET_VECTOR_ELT(ret_, 1, hashtab_);
  
  UNPROTECT(4);
  return ret_;
}



SEXP hashmaptab_integer64(SEXP x_, SEXP bits_, SEXP hashpos_, SEXP nunique_){
  int i, nx = LENGTH(x_);
  int h, nh = LENGTH(hashpos_);
  long long * x = (long long *) REAL(x_);
  unsigned int * hashpos = (unsigned int *) INTEGER(hashpos_);
  SEXP hashtab_;
  PROTECT_INDEX idx;
  PROTECT_WITH_INDEX(hashtab_ = allocVector(INTSXP, nh), &idx);
  int *hashtab = INTEGER(hashtab_);
  int bits = asInteger(bits_);
  int shift = 64 - bits;
  long long v;
  int u, nu=0;
  for(i=0; i<nh; i++)
	hashtab[i]=0;
  for(i=0; i<nx; ){
    v = x[i++];
	h = HASH64(v, shift);
	while (hashpos[h] && x[hashpos[h] - 1] != v){
		h++;
		if (h == nh) 
			h = 0;
	}
	if (!hashpos[h]){
		hashpos[h] = i;
		nu++;
	}
	hashtab[h]++;
  }
  SEXP tabval_;
  PROTECT(tabval_ = allocVector(REALSXP, nu));
  long long * tabval = (long long *) REAL(tabval_);
  for (u=0,h=0;u<nu;h++){
    if (hashpos[h]){
	  tabval[u] = x[hashpos[h]-1];
	  hashtab[u]=hashtab[h];
	  u++;
	}
  }
  INTEGER(nunique_)[0] = nu;
  REPROTECT(hashtab_ = lengthgets(hashtab_, nu), idx);
  
  SEXP class;
  PROTECT(class = allocVector(STRSXP, 1));
  SET_STRING_ELT(class, 0, mkChar("integer64"));
  classgets(tabval_, class);
  
  SEXP ret_;
  PROTECT(ret_ = allocVector(VECSXP, 2));
  SET_VECTOR_ELT(ret_, 0, tabval_);
  SET_VECTOR_ELT(ret_, 1, hashtab_);
  
  UNPROTECT(4);
  return ret_;
}
