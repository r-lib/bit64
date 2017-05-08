/*
# C-Code for searching and merging
# S3 atomic 64bit integers for R
# (c) 2011 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2011-12-11
# Last changed:  2011-12-11
*/

#include <R.h>
#include <Rdefines.h>
//#include <Rinternals.h>

#include "integer64.h"
#include "bsearch.h"

void R_Busy (int which);

SEXP r_ram_integer64_nacount(
  SEXP x_
)
{
  int i,n = LENGTH(x_);
  ValueT *x = (ValueT *) REAL(x_);
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP, 1) );
  int ret = 0;
  if (n){
	R_Busy(1);
	for(i=0;i<n;i++)
		if (x[i]==NA_INTEGER64)
			ret++;
  }  
  INTEGER(ret_)[0]=ret;
  R_Busy(0);
  UNPROTECT(1);
  return ret_;
}


SEXP r_ram_integer64_issorted_asc(
  SEXP x_
)
{
  int i,n = LENGTH(x_);
  ValueT *x = (ValueT *) REAL(x_);
  SEXP ret_;
  PROTECT( ret_ = allocVector(LGLSXP, 1) );
  Rboolean ret = TRUE;
  if (n){
	R_Busy(1);
	for(i=1;i<n;i++)
		if (x[i]<x[i-1]){
			ret = FALSE;
			goto wrapup;
		}
  }  
wrapup:  
  INTEGER(ret_)[0]=ret;
  R_Busy(0);
  UNPROTECT(1);
  return ret_;
}

SEXP r_ram_integer64_sortnut(
  SEXP sorted_            /* somehow sorted table vector */
, SEXP ret_
)
{
  int i,lasti,ities,nties=0,nunique=0,n = LENGTH(sorted_);
  ValueT *sorted = (ValueT *) REAL(sorted_);
  PROTECT( ret_ = allocVector(INTSXP, 2) );
  if (n){
	R_Busy(1);
	nunique=1;
	lasti = 0;
	for(i=1;i<n;i++){
		if (sorted[i]!=sorted[lasti]){
			ities = i - lasti;
			if (ities>1)
				nties += ities;
			nunique++;
			lasti=i;
		}
	}
	if (lasti<(n-1))
		nties += n - lasti;
	R_Busy(0);
  }  
  INTEGER(ret_)[0]=nunique;
  INTEGER(ret_)[1]=nties;
  UNPROTECT(1);
  return ret_;
}

SEXP r_ram_integer64_ordernut(
  SEXP table_
, SEXP order_
, SEXP ret_
)
{
  int i,lasti,ities,nties=0,nunique=0,n = LENGTH(table_);
  ValueT *table;
  table = (ValueT *) REAL(table_);
  IndexT *index = INTEGER(order_);
  PROTECT( ret_ = allocVector(INTSXP, 2) );
  if (n){
	R_Busy(1);
	nunique=1;
	lasti = 0;
	for(i=1;i<n;i++){
		if (table[index[i]-1]!=table[index[lasti]-1]){
			ities = i - lasti;
			if (ities>1)
				nties += ities;
			nunique++;
			lasti=i;
		}
	}
	if (lasti<(n-1))
		nties += n - lasti;
	R_Busy(0);
  }  
  INTEGER(ret_)[0]=nunique;
  INTEGER(ret_)[1]=nties;
  UNPROTECT(1);
  return ret_;
}



SEXP r_ram_integer64_sortfin_asc(
  SEXP x_            /* data vector */
, SEXP sorted_            /* sorted table vector */
, SEXP method_
, SEXP ret_
)
{
  int i,n = LENGTH(x_);
  int pos,nt = LENGTH(sorted_);
  int n1 = nt-1;
  int method = asInteger(method_);

  ValueT *data;
  data = (ValueT *) REAL(x_);
  ValueT *sorted;
  sorted = (ValueT *) REAL(sorted_);

  int *ret = LOGICAL(ret_);

  R_Busy(1);
  DEBUG_INIT
  
  switch (method){
    case 1:{
		for(i=0;i<n;i++)
			ret[i] = integer64_bsearch_asc_EQ(sorted, 0, n1, data[i])<0 ? FALSE : TRUE;
		break;
	}
    case 2:{
	    pos = 0;
		for(i=0;i<n;i++){
			pos = integer64_lsearch_asc_GE(sorted, pos, n1, data[i]);
			if (pos>n1){
			  for (;i<n;i++)
			    ret[i] = FALSE;
			}else{
				ret[i] = data[i]==sorted[pos] ? TRUE : FALSE;
			}
		}
		break;
	}
    case 3:{
	  pos = 0;
	  for (i=0;i<n;i++){
		while(LESS(sorted[pos], data[i])){
		  pos++;
		  if (pos==nt){
			for (;i<n;i++)
			  ret[i] = FALSE;
			goto wrapup;
		  }
		}
		ret[i] = data[i]==sorted[pos] ? TRUE : FALSE;
	  }
	  break;
	}
    default:
	  method=0;
  }	

wrapup:  
	  R_Busy(0);
  if (method==0)
    error("unimplemented method");
  return ret_;
}

SEXP r_ram_integer64_orderfin_asc(
  SEXP x_            /* data vector */
, SEXP table_            /* table vector */
, SEXP order_            /* order vector that makes table_ sorted */
, SEXP method_
, SEXP ret_
)
{
  int i,n = LENGTH(x_);
  int pos,nt = LENGTH(table_);
  int n1 = nt-1;
  int method = asInteger(method_);

  ValueT *data;
  data = (ValueT *) REAL(x_);
  ValueT *table;
  table = (ValueT *) REAL(table_);
  IndexT *index = INTEGER(order_);

  int *ret = LOGICAL(ret_);

  R_Busy(1);
  DEBUG_INIT

  for(i=0;i<nt;i++)
    index[i]--;
  
  switch (method){
    case 1:{
		for(i=0;i<n;i++){
			ret[i] = integer64_bosearch_asc_EQ(table, index, 0, n1, data[i])<0 ? FALSE : TRUE;
		}
		break;
	}
    case 2:{
	    pos = 0;
		for(i=0;i<n;i++){
			pos = integer64_losearch_asc_GE(table, index, pos, n1, data[i]);
			if (pos>n1){
			  for (;i<n;i++)
			    ret[i] = FALSE;
			}else{
				ret[i] = data[i]==table[index[pos]] ? TRUE : FALSE;
			}
		}
		break;
	}
    case 3:{
	  pos = 0;
	  for (i=0;i<n;i++){
		while(LESS(table[index[pos]], data[i])){
		  pos++;
		  if (pos==nt){
			for (;i<n;i++)
			  ret[i] = FALSE;
			goto wrapup;
		  }
		}
		ret[i] = data[i]==table[index[pos]] ? TRUE : FALSE;
	  }
	  break;
	}
    default:
	  method=0;
  }	

wrapup:  
  for(i=0;i<nt;i++)
    index[i]++;
  
	  R_Busy(0);
  if (method==0)
    error("unimplemented method");
  return ret_;
}



SEXP r_ram_integer64_orderpos_asc(
  SEXP x_            /* data vector */
, SEXP table_            /* table vector */
, SEXP order_            /* order vector that makes table_ sorted */
, SEXP nomatch_
, SEXP method_
, SEXP ret_
)
{
  int i,n = LENGTH(x_);
  int pos,nt = LENGTH(table_);
  int n1 = nt-1;
  int method = asInteger(method_);
  int nomatch = asInteger(nomatch_);

  ValueT *data;
  data = (ValueT *) REAL(x_);
  ValueT *table;
  table = (ValueT *) REAL(table_);
  IndexT *index = INTEGER(order_);

  int *ret = INTEGER(ret_);

  R_Busy(1);
  DEBUG_INIT

  for(i=0;i<nt;i++)
    index[i]--;
  
  switch (method){
    case 1:{
		for(i=0;i<n;i++){
			pos = integer64_bosearch_asc_EQ(table, index, 0, n1, data[i]);
			ret[i] = pos<0 ? nomatch : index[pos]+ 1;
		}
		break;
	}
    case 2:{
	    pos = 0;
		for(i=0;i<n;i++){
			pos = integer64_losearch_asc_GE(table, index, pos, n1, data[i]);
			if (pos>n1){
			  for (;i<n;i++)
			    ret[i] = nomatch;
			}else{
				ret[i] = data[i]==table[index[pos]] ? index[pos]+1 : nomatch;
			}
		}
		break;
	}
    case 3:{
	  pos = 0;
	  for (i=0;i<n;i++){
		while(LESS(table[index[pos]], data[i])){
		  pos++;
		  if (pos==nt){
			for (;i<n;i++)
			  ret[i] = nomatch;
			goto wrapup;
		  }
		}
		ret[i] = data[i]==table[index[pos]] ? index[pos]+1 : nomatch;
	  }
	  break;
	}
    default:
	  method=0;
  }	

wrapup:  
  for(i=0;i<nt;i++)
    index[i]++;
  
	  R_Busy(0);
  if (method==0)
    error("unimplemented method");
  return ret_;
}

/* 1= simple binary search of unsorted in sorted
   2= double exponential search of sorted in sorted 
   3= merge-search of sorted in sorted
*/ 
SEXP r_ram_integer64_sortorderpos_asc(
  SEXP x_            /* data vector */
, SEXP sorted_            /* sorted table vector */
, SEXP order_            /* order vector that makes table_ sorted */
, SEXP nomatch_
, SEXP method_  
, SEXP ret_
)
{
  int i,n = LENGTH(x_);
  int pos,nt = LENGTH(sorted_);
  int n1 = nt-1;
  int method = asInteger(method_);
  int nomatch = asInteger(nomatch_);

  ValueT *data;
  data = (ValueT *) REAL(x_);
  ValueT *sorted;
  sorted = (ValueT *) REAL(sorted_);
  IndexT *index = INTEGER(order_);

  int *ret = INTEGER(ret_);

  R_Busy(1);
  DEBUG_INIT

  switch (method){
    case 1:{
		for(i=0;i<n;i++){
			pos = integer64_bsearch_asc_EQ(sorted, 0, n1, data[i]);
			ret[i] = pos<0 ? nomatch : index[pos];
		}
		break;
	}
    case 2:{
	    pos = 0;
		for(i=0;i<n;i++){
			pos = integer64_lsearch_asc_GE(sorted, pos, n1, data[i]);
			if (pos>n1){
			  for (;i<n;i++)
			    ret[i] = nomatch;
			}else{
				ret[i] = data[i]==sorted[pos] ? index[pos] : nomatch;
			}
		}
		break;
	}
    case 3:{
	  pos = 0;
	  for (i=0;i<n;i++){
		while(LESS(sorted[pos], data[i])){
		  pos++;
		  if (pos==nt){
			for (;i<n;i++)
			  ret[i] = nomatch;
			goto wrapup;
		  }
		}
		ret[i] = data[i]==sorted[pos] ? index[pos] : nomatch;
	  }
		break;
	}
    default:
	  method=0;
  }	

wrapup:  
	  R_Busy(0);
  if (method==0)
    error("unimplemented method");
  return ret_;
}



SEXP r_ram_integer64_sortuni_asc(
  SEXP sorted_            /* somehow sorted table vector */
, SEXP ret_
)
{
  int i,pos,n = LENGTH(sorted_);
  ValueT *sorted = (ValueT *) REAL(sorted_);
  ValueT * ret = (ValueT *) REAL(ret_);
  if (n){
	  R_Busy(1);
	  pos = 0;
	  ret[0] = sorted[0];
	  for(i=1;i<n;i++)
	    if (sorted[i]!=ret[pos])
			ret[++pos] = sorted[i];
	  R_Busy(0);
  }
  return ret_;

}

SEXP r_ram_integer64_sortorderuni_asc(
  SEXP table_            /* table vector */
, SEXP sorted_            /* somehow sorted table vector */
, SEXP order_            /* sorted table vector */
, SEXP ret_
)
{
  int i,pos,n = LENGTH(table_);
  ValueT *table = (ValueT *) REAL(table_);
  ValueT *sorted = (ValueT *) REAL(sorted_);
  IndexT *index = INTEGER(order_);
  ValueT * ret = (ValueT *) REAL(ret_);
  ValueT lastval;
  if (n){
	  R_Busy(1);
	  IndexT nbitflags = n/BITS_INTEGER64+(n%BITS_INTEGER64 ? 1 : 0);
	  ValueT *bitflags;
	  bitflags = (ValueT *) R_alloc(nbitflags, sizeof(ValueT));
	  for (i=0;i<nbitflags;i++)
		bitflags[i]=0;
	  lastval = sorted[0]; 
	  bitflags[(index[0]-1)/BITS_INTEGER64] |= (RIGHTBIT_INTEGER64 << ((index[0]-1) % BITS_INTEGER64));
	  for(i=1;i<n;i++)
	    if (sorted[i]!=lastval){
			bitflags[(index[i]-1)/BITS_INTEGER64] |= (RIGHTBIT_INTEGER64 << ((index[i]-1) % BITS_INTEGER64));
			lastval = sorted[i];
		}
	  pos = 0;
	  for(i=0;i<n;i++)
		if ((bitflags[i/BITS_INTEGER64] & (RIGHTBIT_INTEGER64 << (i % BITS_INTEGER64))))
			ret[pos++] = table[i];
	  R_Busy(0);
  }
  return ret_;
}


SEXP r_ram_integer64_orderuni_asc(
  SEXP table_            /* sorted table vector */
, SEXP order_            /* sorted table vector */
, SEXP keep_order_            /* sorted table vector */
, SEXP ret_
)
{
  int i,pos,n = LENGTH(table_);
  ValueT *table = (ValueT *) REAL(table_);
  IndexT *index = INTEGER(order_);
  ValueT * ret = (ValueT *) REAL(ret_);
  ValueT val, lastval;
  if (n){
	  R_Busy(1);
	  if (asLogical(keep_order_)){
	      IndexT nbitflags = n/BITS_INTEGER64+(n%BITS_INTEGER64 ? 1 : 0);
		  ValueT *bitflags;
		  bitflags = (ValueT *) R_alloc(nbitflags, sizeof(ValueT));
		  for (i=0;i<nbitflags;i++)
		    bitflags[i]=0;
		  lastval = table[index[0]-1];
		  bitflags[(index[0]-1)/BITS_INTEGER64] |= (RIGHTBIT_INTEGER64 << ((index[0]-1) % BITS_INTEGER64));
		  for(i=1;i<n;i++){
		    pos = index[i]-1;
			if (table[pos]!=lastval){
				bitflags[pos/BITS_INTEGER64] |= (RIGHTBIT_INTEGER64 << (pos % BITS_INTEGER64));
				lastval = table[pos];
			}
		  }
		  pos = 0;
		  for(i=0;i<n;i++)
			if ((bitflags[i/BITS_INTEGER64] & (RIGHTBIT_INTEGER64 << (i % BITS_INTEGER64))))
				ret[pos++] = table[i];
	  }else{
		  lastval = table[index[0]-1];
		  ret[0] = lastval;
		  pos=1;
		  for(i=1;i<n;i++){
			val = table[index[i]-1];
			if (val!=lastval){
				ret[pos++] = val;
				lastval = val;
			}
		  }
	  }
	  R_Busy(0);
  }
  return ret_;
}


SEXP r_ram_integer64_sortorderupo_asc(
  SEXP sorted_            /* somehow sorted table vector */
, SEXP order_            /* sorted table vector */
, SEXP keep_order_            /* sorted table vector */
, SEXP ret_
)
{
  int i,pos,n = LENGTH(sorted_);
  ValueT *sorted = (ValueT *) REAL(sorted_);
  IndexT *index = INTEGER(order_);
  IndexT *ret = INTEGER(ret_);
  ValueT lastval;
  
  if (n){
	R_Busy(1);
	if (asLogical(keep_order_)){
		IndexT nbitflags = n/BITS_INTEGER64+(n%BITS_INTEGER64 ? 1 : 0);
		ValueT *bitflags;
		bitflags = (ValueT *) R_alloc(nbitflags, sizeof(ValueT));
		for (i=0;i<nbitflags;i++)
			bitflags[i]=0;
		lastval = sorted[0];
		bitflags[(index[0]-1)/BITS_INTEGER64] |= (RIGHTBIT_INTEGER64 << ((index[0]-1) % BITS_INTEGER64));
		for(i=1;i<n;i++)
			if (sorted[i]!=lastval){
				bitflags[(index[i]-1)/BITS_INTEGER64] |= (RIGHTBIT_INTEGER64 << ((index[i]-1) % BITS_INTEGER64));
				lastval = sorted[i];
			}
		pos = 0;
		for(i=0;i<n;i++)
			if ((bitflags[i/BITS_INTEGER64] & (RIGHTBIT_INTEGER64 << (i % BITS_INTEGER64))))
				ret[pos++] = i+1;
	}else{
		ret[0] = index[0];
		pos=1;
		for(i=1;i<n;i++){
			if (sorted[i]!=sorted[i-1]){
				ret[pos++] = index[i];
			}
		}
	}
	R_Busy(0);
  }
  return ret_;
}


SEXP r_ram_integer64_orderupo_asc(
  SEXP table_            /* sorted table vector */
, SEXP order_            /* sorted table vector */
, SEXP keep_order_            /* sorted table vector */
, SEXP ret_
)
{
  int i,pos,n = LENGTH(table_);
  ValueT *table = (ValueT *) REAL(table_);
  IndexT *index = INTEGER(order_);
  IndexT * ret = INTEGER(ret_);
  ValueT lastval;
  if (n){
	  R_Busy(1);
	  if (asLogical(keep_order_)){
	      IndexT nbitflags = n/BITS_INTEGER64+(n%BITS_INTEGER64 ? 1 : 0);
		  ValueT *bitflags;
		  bitflags = (ValueT *) R_alloc(nbitflags, sizeof(ValueT));
		  for (i=0;i<nbitflags;i++)
		    bitflags[i]=0;
		  lastval = table[index[0]-1];
		  bitflags[(index[0]-1)/BITS_INTEGER64] |= (RIGHTBIT_INTEGER64 << ((index[0]-1) % BITS_INTEGER64));
		  for(i=1;i<n;i++){
		    pos = index[i]-1;
			if (table[pos]!=lastval){
				bitflags[pos/BITS_INTEGER64] |= (RIGHTBIT_INTEGER64 << (pos % BITS_INTEGER64));
				lastval = table[pos];
			}
		  }
		  pos = 0;
		  for(i=0;i<n;i++)
			if ((bitflags[i/BITS_INTEGER64] & (RIGHTBIT_INTEGER64 << (i % BITS_INTEGER64))))
				ret[pos++] = i+1;
	  }else{
		  ret[0] = index[0];
		  pos=1;
		  for(i=1;i<n;i++){
			if ((table[index[i]-1])!=(table[index[i-1]-1])){
				ret[pos++] = index[i];
			}
		  }
	  }
	  R_Busy(0);
  }
  return ret_;
}




SEXP r_ram_integer64_sorttab_asc(
  SEXP sorted_            /* somehow sorted table vector */
, SEXP ret_
)
{
  int i,pos,n = LENGTH(sorted_);
  ValueT *sorted = (ValueT *) REAL(sorted_);
  IndexT * ret = INTEGER(ret_);
  if (n){
	  R_Busy(1);
	  pos = 0;
	  ret[0] = 1;
	  for(i=1;i<n;i++){
	    if (sorted[i]!=sorted[i-1])
			ret[++pos] = 1;
		else
			ret[pos]++;
	  }
	  R_Busy(0);
  }
  return ret_;

}

SEXP r_ram_integer64_ordertab_asc(
  SEXP table_            /* sorted table vector */
, SEXP order_            /* sorted table vector */
, SEXP denormalize_            /* sorted table vector */
, SEXP keep_order_            
, SEXP ret_
)
{
  int i,j,pos,n = LENGTH(table_);
  ValueT *table = (ValueT *) REAL(table_);
  IndexT *index = INTEGER(order_);
  IndexT * ret = INTEGER(ret_);
  int cnt;
  if (n){
	  PROTECT(ret_); /* because of R_Busy wee need PROTECT, according to Thomas Kalibera */  
	  R_Busy(1);
	  if (asLogical(denormalize_)){
	      j = 0;
		  cnt=1;
		  pos = index[j]-1;
		  for(i=1;i<n;i++){
			if (table[pos]!=table[index[i]-1]){
				for (;j<i;j++)
					ret[index[j]-1] = cnt;
				cnt = 1;
				j = i;
				pos = index[j]-1;
			}else{
				cnt++;
			}
		  }
		for (;j<i;j++)
			ret[index[j]-1] = cnt;
	  }else if (asLogical(keep_order_)){
		  pos = index[0]-1;
		  ret[pos] = 1;
		  for(i=1;i<n;i++){
		    j = index[i]-1;
			if (table[pos]!=table[j]){
				pos = j;
				ret[pos] = 1;
			}else{
			    ret[pos]++;
				ret[j]=0;
			}
		  }
		  pos = 0;
		  for(i=0;i<n;i++)
		    if (ret[i])
			  ret[pos++] = ret[i];
		  SET_LENGTH(ret_, pos);
	  }else{
		  j = 0;
		  ret[j] = 1;
		  pos = index[j]-1;
		  for(i=1;i<n;i++){
			if (table[index[i]-1]!=table[pos]){
				pos = index[i]-1;
				ret[++j] = 1;
			}else{
			    ret[j]++;
			}
		  }
	  }
	  R_Busy(0);
	  UNPROTECT(1);
  }
  return ret_;
}


SEXP r_ram_integer64_sortordertab_asc(
  SEXP sorted_            /* somehow sorted table vector */
, SEXP order_            /* sorted table vector */
, SEXP denormalize_            
, SEXP ret_
)
{
  int i,pos,n = LENGTH(sorted_);
  ValueT *sorted = (ValueT *) REAL(sorted_);
  IndexT *index = INTEGER(order_);
  IndexT * ret = INTEGER(ret_);
  int cnt;
  if (n){
          PROTECT(ret_); /* because of R_Busy wee need PROTECT, according to Thomas Kalibera */
	  R_Busy(1);
	  if (asLogical(denormalize_)){
			  pos = 0;
			  cnt = 1;
			  for(i=1;i<n;i++){
				if (sorted[i]!=sorted[pos]){
					for (;pos<i;pos++)
						ret[index[pos]-1] = cnt;
					cnt = 1;
					pos = i;
				}else{
					cnt++;
				}
			  }
			for (;pos<i;pos++)
				ret[index[pos]-1] = cnt;
	  }else{
			  pos = index[0]-1;
			  ret[pos] = 1;
			  for(i=1;i<n;i++){
				if (sorted[i]!=sorted[i-1]){
					pos = index[i]-1;
					ret[pos] = 1;
				}else{
					ret[pos]++;
					ret[index[i]-1]=0;
				}
			  }
			  pos = 0;
			  for(i=0;i<n;i++)
				if (ret[i])
				  ret[pos++] = ret[i];
			  SET_LENGTH(ret_, pos);
	  }
	  R_Busy(0);
          UNPROTECT(1);
  }
  return ret_;
}


// with na_skip_num==0 this is the proper version doing proper star schema modelling: 
// NAs receive a key value like all other values, such that they can be joined with the dimension table
// with na_skip_num==na_count this is the sick version needed for as.factor/as.ordered: 
// NAs are propagated and can not be joined with the dimension table / NAs are not in levels
// this breaks all previous consistent modelling
SEXP r_ram_integer64_orderkey_asc(
  SEXP table_            /* sorted table vector */
, SEXP order_            /* sorted table vector */
, SEXP na_skip_num_     /* number of NAs to be skiped before keying, must be 0 or na_count */
, SEXP ret_
)
{
  int i,j,pos,n = LENGTH(table_);
  ValueT *table = (ValueT *) REAL(table_);
  IndexT *index = INTEGER(order_);
  IndexT na_skip_num = asInteger(na_skip_num_);
  IndexT * ret = INTEGER(ret_);
  IndexT key;
  if (n){
	  R_Busy(1);
		  for (i=0;i<na_skip_num;i++)
			ret[index[i]-1] = NA_INTEGER;
		  if (na_skip_num<n){
			  key = 1;
			  pos = index[na_skip_num]-1;
			  ret[pos]= key;
			  for(i=na_skip_num+1;i<n;i++){
				j = index[i]-1;
				if (table[pos]!=table[j]){
					pos = j;
					key++;	
				}
				ret[j] = key;
			  }
		  }
	  R_Busy(0);
  }
  return ret_;
}


SEXP r_ram_integer64_sortorderkey_asc(
  SEXP sorted_            /* somehow sorted table vector */
, SEXP order_            /* sorted table vector */
, SEXP na_skip_num_     /* number of NAs to be skiped before keying, must be 0 or na_count */
, SEXP ret_
)
{
  int i,n = LENGTH(sorted_);
  ValueT *sorted = (ValueT *) REAL(sorted_);
  IndexT *index = INTEGER(order_);
  IndexT na_skip_num = asInteger(na_skip_num_);
  IndexT * ret = INTEGER(ret_);
  IndexT key;
  if (n){
	R_Busy(1);
	for (i=0;i<na_skip_num;i++)
		ret[index[i]-1] = NA_INTEGER;
	if (na_skip_num<n){
	  key = 1;
	  ret[index[na_skip_num]-1]= key;
	  for(i=na_skip_num+1;i<n;i++){
		if (sorted[i]!=sorted[i-1]){
			key++;	
		}
		ret[index[i]-1] = key;
	  }
	}
	R_Busy(0);
  }
  return ret_;
}


SEXP r_ram_integer64_orderrnk_asc(
  SEXP table_            /* sorted table vector */
, SEXP order_            /* sorted table vector */
, SEXP nacount_
, SEXP ret_
)
{
  int i,j,pos,n = LENGTH(table_);
  ValueT *table = (ValueT *) REAL(table_);
  IndexT *index = INTEGER(order_);
  double * ret = REAL(ret_);
  double avgrank;
  int nacount = asInteger(nacount_);
  int lasti;
  if (n){
	  R_Busy(1);
		  for (i=0;i<nacount;i++)
			ret[index[i]-1] = NA_REAL;
		  index += nacount;
		  n -= nacount;
	      lasti = 0;
		  pos = index[0]-1;
		  for(i=1;i<n;i++){
		    j = index[i]-1;
			if (table[pos]!=table[j]){
				pos = j;
				avgrank = (lasti + 1 + i)/2.0;	
				for (j=i-1;j>=lasti;j--)
				  ret[index[j]-1] = avgrank;
			    lasti = i;
			}
		  }
		  avgrank = (lasti + 1 + i)/2.0;	
		  for (j=i-1;j>=lasti;j--)
		    ret[index[j]-1] = avgrank;
		  
	  R_Busy(0);
  }
  return ret_;
}


SEXP r_ram_integer64_sortorderrnk_asc(
  SEXP sorted_            /* somehow sorted table vector */
, SEXP order_            /* sorted table vector */
, SEXP nacount_
, SEXP ret_
)
{
  int i,j,n = LENGTH(sorted_);
  ValueT *sorted = (ValueT *) REAL(sorted_);
  IndexT *index = INTEGER(order_);
  double * ret = REAL(ret_);
  double avgrank;
  int nacount = asInteger(nacount_);
  int lasti;
  if (n){
	  R_Busy(1);
		  for (i=0;i<nacount;i++)
			ret[index[i]-1] = NA_REAL;
		  index += nacount;
		  sorted += nacount;
		  n -= nacount;
	      lasti = 0;
		  for(i=1;i<n;i++){
			if (sorted[i]!=sorted[i-1]){
				avgrank = (lasti + 1 + i)/2.0;	
				for (j=i-1;j>=lasti;j--)
				  ret[index[j]-1] = avgrank;
			    lasti = i;
			}
		  }
		  avgrank = (lasti + 1 + i)/2.0;	
		  for (j=i-1;j>=lasti;j--)
		    ret[index[j]-1] = avgrank;
	  R_Busy(0);
  }
  return ret_;
}




SEXP r_ram_integer64_orderdup_asc(
  SEXP table_            /* sorted table vector */
, SEXP order_            /* sorted table vector */
, SEXP method_
, SEXP ret_
)
{
  int i,pos,n = LENGTH(table_);
  ValueT *table = (ValueT *) REAL(table_);
  IndexT *index = INTEGER(order_);
  int method = asInteger(method_);
  int * ret = LOGICAL(ret_);
  ValueT lastval;
  if (n){
	  R_Busy(1);
	  switch (method){
		case 1:{
		  for (i=0;i<n;i++)
			ret[i]=TRUE;
		  lastval = table[index[0]-1];
		  ret[index[0]-1] = FALSE;
		  for(i=1;i<n;i++){
			pos = index[i]-1;
			if (table[pos]!=lastval){
				ret[pos] = FALSE;
				lastval = table[pos];
			}
		  }
		  break;
		}
		case 2:{
		  IndexT nbitflags = n/BITS_INTEGER64+(n%BITS_INTEGER64 ? 1 : 0);
		  ValueT *bitflags;
		  bitflags = (ValueT *) R_alloc(nbitflags, sizeof(ValueT));
		  for (i=0;i<nbitflags;i++)
			bitflags[i]=0;
		  lastval = table[index[0]-1];
		  bitflags[(index[0]-1)/BITS_INTEGER64] |= (RIGHTBIT_INTEGER64 << ((index[0]-1) % BITS_INTEGER64));
		  for(i=1;i<n;i++){
			pos = index[i]-1;
			if (table[pos]!=lastval){
				bitflags[pos/BITS_INTEGER64] |= (RIGHTBIT_INTEGER64 << (pos % BITS_INTEGER64));
				lastval = table[pos];
			}
		  }
		  for(i=0;i<n;i++)
			ret[i] = ((bitflags[i/BITS_INTEGER64] & (RIGHTBIT_INTEGER64 << (i % BITS_INTEGER64)))) ? FALSE : TRUE;
		  break;
		}
		default:
		  method=0;
	  }	
	  R_Busy(0);
  }
  if (method==0)
    error("unimplemented method");
  return ret_;
}


SEXP r_ram_integer64_sortorderdup_asc(
  SEXP sorted_            /* somehow sorted table vector */
, SEXP order_            /* sorted table vector */
, SEXP method_
, SEXP ret_
)
{
  int i,n = LENGTH(sorted_);
  ValueT *sorted = (ValueT *) REAL(sorted_);
  IndexT *index = INTEGER(order_);
  int method = asInteger(method_);
  int * ret = LOGICAL(ret_);
  if (n){
	  R_Busy(1);
	  switch (method){
		case 1:{
		  for (i=0;i<n;i++)
			ret[i]=TRUE;
		  ret[index[0]-1] = FALSE;
		  for(i=1;i<n;i++){
			if (sorted[i]!=sorted[i-1]){
				ret[index[i]-1] = FALSE;
			}
		  }
		  break;
		}
		case 2:{
		  IndexT nbitflags = n/BITS_INTEGER64+(n%BITS_INTEGER64 ? 1 : 0);
		  ValueT *bitflags;
		  bitflags = (ValueT *) R_alloc(nbitflags, sizeof(ValueT));
		  for (i=0;i<nbitflags;i++)
			bitflags[i]=0;
		  bitflags[(index[0]-1)/BITS_INTEGER64] |= (RIGHTBIT_INTEGER64 << ((index[0]-1) % BITS_INTEGER64));
		  for(i=1;i<n;i++){
			if (sorted[i]!=sorted[i-1]){
				bitflags[(index[i]-1)/BITS_INTEGER64] |= (RIGHTBIT_INTEGER64 << ((index[i]-1) % BITS_INTEGER64));
			}
		  }
		  for(i=0;i<n;i++)
			ret[i] = ((bitflags[i/BITS_INTEGER64] & (RIGHTBIT_INTEGER64 << (i % BITS_INTEGER64)))) ? FALSE : TRUE;
		  break;
		}
		default:
		  method=0;
	  }	
	  R_Busy(0);
  }
  if (method==0)
    error("unimplemented method");
  return ret_;
}


/* experimental: all origpos at which we have ties */
SEXP r_ram_integer64_sortordertie_asc(
  SEXP sorted_            /* somehow sorted table vector */
, SEXP order_            /* sorted table vector */
, SEXP ret_
)
{
  int i,j,n = LENGTH(sorted_);
  ValueT *sorted = (ValueT *) REAL(sorted_);
  IndexT *index = INTEGER(order_);
  IndexT * ret = INTEGER(ret_);
  
  if (n){
	  R_Busy(1);
	  IndexT nbitflags = n/BITS_INTEGER64+(n%BITS_INTEGER64 ? 1 : 0);
	  ValueT *bitflags;
	  bitflags = (ValueT *) R_alloc(nbitflags, sizeof(ValueT));
	  for (i=0;i<nbitflags;i++)
		bitflags[i]=0;
	  j = 0;
	  for(i=1;i<n;i++)
	    if (sorted[i]!=sorted[j]){
			if (i>j+1){
				for (;j<i;j++)
					bitflags[(index[j]-1)/BITS_INTEGER64] |= (RIGHTBIT_INTEGER64 << ((index[j]-1) % BITS_INTEGER64));
			}else{
				j = i;
			}
		}
			if (i>j+1){
				for (;j<i;j++)
					bitflags[(index[j]-1)/BITS_INTEGER64] |= (RIGHTBIT_INTEGER64 << ((index[j]-1) % BITS_INTEGER64));
			}
	  j = 0;
	  for(i=0;i<n;i++)
		if ((bitflags[i/BITS_INTEGER64] & (RIGHTBIT_INTEGER64 << (i % BITS_INTEGER64))))
			ret[j++] = i+1;
	  R_Busy(0);
  }
  return ret_;
}


SEXP r_ram_integer64_ordertie_asc(
  SEXP table_            /* sorted table vector */
, SEXP order_            /* sorted table vector */
, SEXP ret_
)
{
  int i,j,pos,n = LENGTH(table_);
  ValueT *table = (ValueT *) REAL(table_);
  IndexT *index = INTEGER(order_);
  IndexT * ret = INTEGER(ret_);
  if (n){
	  R_Busy(1);
	  IndexT nbitflags = n/BITS_INTEGER64+(n%BITS_INTEGER64 ? 1 : 0);
	  ValueT *bitflags;
	  bitflags = (ValueT *) R_alloc(nbitflags, sizeof(ValueT));
	  for (i=0;i<nbitflags;i++)
		bitflags[i]=0;
	  j = 0;
	  pos = index[0]-1;
	  for(i=1;i<n;i++)
	    if (table[index[i]-1]!=table[pos]){
			if (i>j+1){
				for (;j<i;j++)
					bitflags[(index[j]-1)/BITS_INTEGER64] |= (RIGHTBIT_INTEGER64 << ((index[j]-1) % BITS_INTEGER64));
			}else{
				j = i;
			}
		pos = index[i]-1;
		}
			if (i>j+1){
				for (;j<i;j++)
					bitflags[(index[j]-1)/BITS_INTEGER64] |= (RIGHTBIT_INTEGER64 << ((index[j]-1) % BITS_INTEGER64));
			}
	  j = 0;
	  for(i=0;i<n;i++)
		if ((bitflags[i/BITS_INTEGER64] & (RIGHTBIT_INTEGER64 << (i % BITS_INTEGER64))))
			ret[j++] = i+1;

	  R_Busy(0);
  }
  return ret_;
}

SEXP r_ram_integer64_sortsrt(
  SEXP x_            /* sorted data vector */
, SEXP na_count_     /* logical scalar */
, SEXP na_last_      /* logical scalar */
, SEXP decreasing_   /* logical scalar */
, SEXP ret_   		 /* logical scalar */
)
{
  R_Busy(1);
  DEBUG_INIT
  
  int i,j,l,r,n = LENGTH(x_);
  Rboolean na_count   = asInteger(na_count_);
  Rboolean na_last    = asLogical(na_last_);
  Rboolean decreasing = asLogical(decreasing_);
  
  ValueT *sorted;
  sorted = (ValueT *) REAL(x_);
  ValueT *ret;
  ret = (ValueT *) REAL(ret_);
  
  if (na_last){
	for (i=0,j=n-na_count;i<na_count;i++,j++)
		ret[j] = sorted[i];
  }else{
	for (i=0,j=0;i<na_count;i++,j++)
		ret[j] = sorted[i];
	ret += na_count;
  }
  sorted += na_count;
  n = n - na_count;

  if (decreasing){
	  for(l=n-2,r=n-1,j=0;l>=0;l--)
	    if (sorted[l]!=sorted[r]){
			for (i=l+1;i<=r;i++,j++)
				ret[j] = sorted[i];
			r=l;
		}
			for (i=l+1;i<=r;i++,j++)
				ret[j] = sorted[i];
  }else{
	for (i=0,j=0;i<n;i++,j++)
		ret[j] = sorted[i];
  }
  
  R_Busy(0);
  return ret_;
}

SEXP r_ram_integer64_sortorderord(
  SEXP x_            /* sorted data vector */
, SEXP index_        /* index vector */
, SEXP na_count_     /* logical scalar */
, SEXP na_last_      /* logical scalar */
, SEXP decreasing_   /* logical scalar */
, SEXP ret_   		 /* logical scalar */
)
{
  R_Busy(1);
  DEBUG_INIT
  
  int i,j,l,r,n = LENGTH(x_);
  Rboolean na_count   = asInteger(na_count_);
  Rboolean na_last    = asLogical(na_last_);
  Rboolean decreasing = asLogical(decreasing_);
  
  ValueT *sorted;
  sorted = (ValueT *) REAL(x_);
  IndexT *index = INTEGER(index_);
  IndexT *ret = INTEGER(ret_);
  
  if (na_last){
	for (i=0,j=n-na_count;i<na_count;i++,j++)
		ret[j] = index[i];
  }else{
	for (i=0,j=0;i<na_count;i++,j++)
		ret[j] = index[i];
	ret += na_count;
  }
  index += na_count;
  n = n - na_count;

  if (decreasing){
	  sorted += na_count;
	  for(l=n-2,r=n-1,j=0;l>=0;l--)
		if (sorted[l]!=sorted[r]){
			for (i=l+1;i<=r;i++,j++)
				ret[j] = index[i];
			r=l;
		}
			for (i=l+1;i<=r;i++,j++)
				ret[j] = index[i];
  }else{
	for (i=0,j=0;i<n;i++,j++)
		ret[j] = index[i];
  }
  
  R_Busy(0);
  return ret_;
}

SEXP r_ram_integer64_orderord(
  SEXP x_            /* sorted data vector */
, SEXP index_        /* index vector */
, SEXP na_count_     /* logical scalar */
, SEXP na_last_      /* logical scalar */
, SEXP decreasing_   /* logical scalar */
, SEXP ret_   		 /* logical scalar */
)
{
  R_Busy(1);
  DEBUG_INIT
  
  int i,j,l,r,n = LENGTH(x_);
  Rboolean na_count   = asInteger(na_count_);
  Rboolean na_last    = asLogical(na_last_);
  Rboolean decreasing = asLogical(decreasing_);
  
  ValueT *data;
  data = (ValueT *) REAL(x_);
  IndexT *index = INTEGER(index_);
  IndexT *ret = INTEGER(ret_);
  
  if (na_last){
	for (i=0,j=n-na_count;i<na_count;i++,j++)
		ret[j] = index[i];
  }else{
	for (i=0,j=0;i<na_count;i++,j++)
		ret[j] = index[i];
	ret += na_count;
  }
  index += na_count;
  n = n - na_count;

  if (decreasing){
	  data += na_count;
	  for(l=n-2,r=n-1,j=0;l>=0;l--)
		if (data[index[l]]!=data[index[r]]){
			for (i=l+1;i<=r;i++,j++)
				ret[j] = index[i];
			r=l;
		}
			for (i=l+1;i<=r;i++,j++)
				ret[j] = index[i];
  }else{
	for (i=0,j=0;i<n;i++,j++)
		ret[j] = index[i];
  }
  
  R_Busy(0);
  return ret_;
}


