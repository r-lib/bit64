/*
# C-Code for sorting and ordering
# S3 atomic 64bit integers for R
# (c) 2011-2024 Jens Oehlschägel
# (c) 2025-2026 Michael Chirico
# Licence: GPL2
# Provided 'as is', use at your own risk
*/

#include <R_ext/Random.h> // unif_rand
#include <Rinternals.h> // asLogical
#include "integer64.h" // NA_INTEGER64
#include "sort64.h"

#define SHELLARRAYSIZE 16

// static
// returns uniform random index in range 0..(n-1)
static IndexT randIndex(
  IndexT n    // number of positions to random select from
);

// returns one of {a,b,c} such that it represents the median of data[{a,b,c}]
static IndexT ram_integer64_median3(
ValueT *data  // pointer to data
, IndexT a    // pos in data
, IndexT b    // pos in data
, IndexT c    // pos in data
);

// returns one of {a,b,c} such that it represents the median of data[index[{a,b,c}]]
static IndexT ram_integer64_median3index(
  ValueT *data    // pointer to data
, IndexT *index   // index positions into data
, IndexT a        // pos in index
, IndexT b        // pos in index
, IndexT c        // pos in index
);



static const ValueT shellincs[SHELLARRAYSIZE] = {1073790977, 268460033, 67121153, 16783361, 4197377,
           1050113, 262913, 65921, 16577, 4193, 1073, 281, 77,
           23, 8, 1};


/* { === NA handling for integer64 ================================================ */

int ram_integer64_fixsortNA(ValueT *data, IndexT n, int has_na, int na_last, int decreasing) {
  if (!has_na)
    return 0;
  IndexT i, nNA = 0;
  if (decreasing) {
    for (i = n - 1; i >= 0; i--) {
      if (is_na64(data[i]))
        nNA++;
      else
        break;
    }
    if (!na_last) {
      for (; i >= 0; i--)
        data[i + nNA] = data[i];
      for (i = nNA - 1; i >= 0; i--)
        data[i] = NA_INTEGER64;
    }
  } else {
    for (i = 0; i < n; i++) {
      if (is_na64(data[i]))
        nNA++;
      else
        break;
    }
    if (na_last) {
      for (; i < n; i++)
        data[i - nNA] = data[i];
      for (i = n - nNA; i < n; i++)
        data[i] = NA_INTEGER64;
    }
  }
  return nNA;
}

// post sortordering NA handling
int ram_integer64_fixsortorderNA(
  ValueT *data      // RETURNED: pointer to data vector
, IndexT *index      // RETURNED: pointer to index vector
, IndexT n           // length of vectors
, int has_na         // 0 for pure doubles, 1 if NA or NaN can be present
, int na_last        // 0 for placing NA NaN left, 1 for placing NA NaN right
, int decreasing     // 0 for ascending, 1 for descending (must match the same parameter in sorting)
, IndexT *auxindex  // MODIFIED: pointer to auxilliary index vector
)
{
  if (has_na) {
    IndexT i,offset, nNA = 0 ;
    if (decreasing) {
    for (i = n - 1; i >= 0; i--) {
      if (is_na64(data[i]))
      nNA++;
    else
      break;
    }
    if (!na_last) {
      if (!auxindex)
         auxindex = (IndexT *) R_alloc(nNA, sizeof(IndexT));
       offset = n-nNA;
       for (i = nNA - 1; i >= 0; i--)
         auxindex[i] = index[offset + i];
       for (i=offset-1;i>=0;i--) {
       index[i + nNA] = index[i];
       data[i + nNA] = data[i];
       }
       for (i = nNA - 1; i >= 0; i--) {
         index[i] = auxindex[i];
       data[i] = NA_INTEGER64;
       }
    }
  } else{
    for (i = 0; i < n; i++) {
      if (is_na64(data[i]))
      nNA++;
    else
      break;
    }
    if (na_last) {
      if (!auxindex)
         auxindex = (IndexT *) R_alloc(nNA, sizeof(IndexT));
       for (i=0;i<nNA;i++)
         auxindex[i] = index[i];
       for (i=nNA;i<n; i++) {
       index[i - nNA] = index[i];
       data[i - nNA] = data[i];
       }
       offset = n-nNA;
       for (i=offset;i<n; i++) {
         index[i] = auxindex[i-offset];
       data[i] = NA_INTEGER64;
       }
    }
  }
  return nNA;
  } else{
    return 0;
  }
}

// post ordering NA handling
int ram_integer64_fixorderNA(
  ValueT *data          // UNCHANGED: pointer to data vector
, IndexT *index         // RETURNED: pointer to index vector
, IndexT n              // length of vectors
, int has_na            // 0 for pure doubles, 1 if NA or NaN can be present
, int na_last           // 0 for placing NA NaN left, 1 for placing NA NaN right
, int decreasing        // 0 for ascending, 1 for descending (must match the same parameter in sorting)
, IndexT *auxindex      // MODIFIED: pointer to auxilliary index vector
)
{
  if (has_na) {
    IndexT i,offset, nNA = 0 ;
    if (decreasing) {
    for (i = n - 1; i >= 0; i--) {
      if (is_na64(data[index[i]]))
      nNA++;
    else
      break;
    }
    if (!na_last) {
      if (!auxindex)
         auxindex = (IndexT *) R_alloc(nNA, sizeof(IndexT));
       offset = n-nNA;
       for (i = nNA - 1; i >= 0; i--)
         auxindex[i] = index[offset + i];
       for (i=offset-1;i>=0;i--) {
       index[i + nNA] = index[i];
       }
       for (i = nNA - 1; i >= 0; i--) {
         index[i] = auxindex[i];
       }
    }
  } else{
    for (i = 0; i < n; i++) {
      if (is_na64(data[index[i]]))
      nNA++;
    else
      break;
    }
    if (na_last) {
      if (!auxindex)
         auxindex = (IndexT *) R_alloc(nNA, sizeof(IndexT));
       for (i=0;i<nNA;i++)
         auxindex[i] = index[i];
       for (i=nNA;i<n; i++)
       index[i - nNA] = index[i];
       offset = n-nNA;
       for (i=offset;i<n; i++)
         index[i] = auxindex[i-offset];
    }
  }
  return nNA;
  } else{
    return 0;
  }
}



/* } === NA handling for integer64 ================================================ */


/* { === pure C stable insertion sort for integer64 ================================================ */

// ascending insertion sorting
void ram_integer64_insertionsort_asc(
  ValueT *data    // RETURNED: pointer to data vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
)
{
  IndexT i;
  for (i = r; i > l; i--) {
    if (data[i] < data[i - 1]) {
      exch64(&data[i - 1], &data[i]);
    }
  }
  for (i = l + 2; i <= r; i++) {
    IndexT j = i;
    ValueT v = data[i];
    while (v < data[j - 1]) {
      data[j] = data[j - 1];
      j--;
    }
    data[j] = v;
  }
}

// ascending insertion sortordering
void ram_integer64_insertionsortorder_asc(
  ValueT *data    // RETURNED: pointer to data vector
, IndexT *index   // RETURNED: pointer to index vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
)
{
  IndexT i;
  for (i = r; i > l; i--) {
    if (data[i] < data[i - 1]) {
      exch64(&data[i - 1], &data[i]);
      exch_idx(&index[i - 1], &index[i]);
    }
  }
  for (i = l + 2; i <= r; i++) {
    IndexT j = i;
    IndexT vi = index[i];
    ValueT v = data[i];
    while (v < data[j - 1]) {
      index[j] = index[j - 1];
      data[j] = data[j - 1];
      j--;
    }
    index[j] = vi;
    data[j] = v;
  }
}

// ascending insertion sortordering
void ram_integer64_insertionorder_asc(
  ValueT *data    // UNCHANGED: pointer to data vector
, IndexT *index   // RETURNED: pointer to index vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
)
{
  IndexT i;
  for (i = r; i > l; i--) {
    if (data[index[i]] < data[index[i - 1]]) {
      exch_idx(&index[i - 1], &index[i]);
    }
  }
  for (i = l + 2; i <= r; i++) {
    IndexT j = i;
    IndexT vi = index[i];
    ValueT v = data[vi];
    while (v < data[index[j - 1]]) {
      index[j] = index[j - 1];
      j--;
    }
    index[j] = vi;
  }
}



// descending insertion sorting
void ram_integer64_insertionsort_desc(
  ValueT *data    // RETURNED: pointer to data vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
)
{
  IndexT i;
  for (i = l; i < r; i++) {
    if (data[i] < data[i + 1]) {
      exch64(&data[i + 1], &data[i]);
    }
  }
  for (i = r - 2; i >= l; i--) {
    IndexT j = i;
    ValueT v = data[i];
    while (v < data[j + 1]) {
      data[j] = data[j + 1];
      j++;
    }
    data[j] = v;
  }
}

// descending insertion sortordering
void ram_integer64_insertionsortorder_desc(
  ValueT *data    // RETURNED: pointer to data vector
, IndexT *index   // RETURNED: pointer to index vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
)
{
  IndexT i;
  for (i = l; i < r; i++) {
    if (data[i] < data[i + 1]) {
      exch64(&data[i + 1], &data[i]);
      exch_idx(&index[i + 1], &index[i]);
    }
  }
  for (i = r - 2; i >= l; i--) {
    IndexT j = i;
    IndexT vi = index[i];
    ValueT v = data[i];
    while (v < data[j + 1]) {
      index[j] = index[j + 1];
      data[j] = data[j + 1];
      j++;
    }
    index[j] = vi;
    data[j] = v;
  }
}

// descending insertion sortordering
void ram_integer64_insertionorder_desc(
  ValueT *data    // UNCHANGED: pointer to data vector
, IndexT *index   // RETURNED: pointer to index vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
)
{
  IndexT i;
  for (i = l; i < r; i++) {
    if (data[index[i]] < data[index[i + 1]]) {
      exch_idx(&index[i + 1], &index[i]);
    }
  }
  for (i = r - 2; i >= l; i--) {
    IndexT j = i;
    IndexT vi = index[i];
    ValueT v = data[vi];
    while (v < data[index[j + 1]]) {
      index[j] = index[j + 1];
      j++;
    }
    index[j] = vi;
  }
}

/* } === pure C stable insertion sort for integer64 ================================================ */





/* { === pure C stable shell sort for integer64 ================================================ */


void ram_integer64_shellsort_asc(ValueT *data, IndexT l, IndexT r)
{
    ValueT v;
    IndexT i, j, h, lh, t, n=r-l+1;
    if (n < 2) return;
    for (t = 0; shellincs[t] > n; t++);
    for (; t < SHELLARRAYSIZE; t++) {
      h = shellincs[t];
      lh = l+h;
      for (i = lh; i <= r; i++) {
        v = data[i];
        j = i;
        while (j >= lh && v < data[j - h]) {
          data[j] = data[j - h];
          j -= h;
        }
        data[j] = v;
      }
    }
}
void ram_integer64_shellsort_desc(ValueT *data, IndexT l, IndexT r)
{
    ValueT v;
    IndexT i, j, h, lh, t, n=r-l+1;
    if (n < 2) return;
    for (t = 0; shellincs[t] > n; t++);
    for (; t < SHELLARRAYSIZE; t++) {
      h = shellincs[t];
      lh = l+h;
      for (i = lh; i <= r; i++) {
        v = data[i];
        j = i;
        while (j >= lh && data[j - h] < v) {
          data[j] = data[j - h];
          j -= h;
        }
        data[j] = v;
      }
    }
}

void ram_integer64_shellsortorder_asc(ValueT *data, IndexT *index, IndexT l, IndexT r)
{
    ValueT v;
    IndexT vi, i, j, h, lh, t, n=r-l+1;
    if (n < 2) return;
    for (t = 0; shellincs[t] > n; t++);
    for (; t < SHELLARRAYSIZE; t++) {
      h = shellincs[t];
      lh = l+h;
      for (i = lh; i <= r; i++) {
        vi = index[i];
        v = data[i];
        j = i;
        while (j >= lh && v < data[j - h]) {
          index[j] = index[j - h];
          data[j] = data[j - h];
          j -= h;
        }
        index[j] = vi;
        data[j] = v;
      }
    }
}
void ram_integer64_shellsortorder_desc(ValueT *data, IndexT *index, IndexT l, IndexT r)
{
    ValueT v;
    IndexT vi, i, j, h, lh, t, n=r-l+1;
    if (n < 2) return;
    for (t = 0; shellincs[t] > n; t++);
    for (; t < SHELLARRAYSIZE; t++) {
      h = shellincs[t];
      lh = l+h;
      for (i = lh; i <= r; i++) {
        vi = index[i];
        v = data[i];
        j = i;
        while (j >= lh && data[j - h] < v) {
          index[j] = index[j - h];
          data[j] = data[j - h];
          j -= h;
        }
        index[j] = vi;
        data[j] = v;
      }
    }
}

void ram_integer64_shellorder_asc(ValueT *data, IndexT *index, IndexT l, IndexT r)
{
    ValueT v;
    IndexT vi, i, j, h, lh, t, n=r-l+1;
    if (n < 2) return;
    for (t = 0; shellincs[t] > n; t++);
    for (; t < SHELLARRAYSIZE; t++) {
      h = shellincs[t];
      lh = l+h;
      for (i = lh; i <= r; i++) {
        vi = index[i];
        v = data[vi];
        j = i;
        while (j >= lh && v < data[index[j - h]]) {
          index[j] = index[j - h];
          j -= h;
        }
        index[j] = vi;
      }
    }
}
void ram_integer64_shellorder_desc(ValueT *data, IndexT *index, IndexT l, IndexT r)
{
    ValueT v;
    IndexT vi, i, j, h, lh, t, n=r-l+1;
    if (n < 2) return;
    for (t = 0; shellincs[t] > n; t++);
    for (; t < SHELLARRAYSIZE; t++) {
      h = shellincs[t];
      lh = l+h;
      for (i = lh; i <= r; i++) {
        vi = index[i];
        v = data[vi];
        j = i;
        while (j >= lh && data[index[j - h]] < v) {
          index[j] = index[j - h];
          j -= h;
        }
        index[j] = vi;
      }
    }
}


/* } === pure C stable shellsort sort for integer64 ================================================ */




/* { === pure C stable merge sort for integer64 ================================================ */

/* Sedgewick 8.1 Merging
   stable merge c=a+b where na=len(a) and nb=len(b) */

// ascending merge for sorting
void ram_integer64_sortmerge_asc(
  ValueT *c   // pointer to merge target data vector
, ValueT *a   // pointer to merge source data vector a
, ValueT *b   // pointer to merge source data vector b
, IndexT na   // number of elements in merge source vector a
, IndexT nb   // number of elements in merge source vector b
)
{
  IndexT i,j,k,K=na+nb;
  for (i=0,j=0,k=0;k<K;k++) {
    if (i==na) {
      for (;k<K;k++)
        c[k] = b[j++];
      break;
    }
    if (j==nb) {
      for (;k<K;k++)
        c[k] = a[i++];
      break;
    }
    if (b[j] < a[i])
      c[k] = b[j++];
    else
      c[k] = a[i++];
  }
}

// ascending merge for ordering
void ram_integer64_ordermerge_asc(
ValueT *data  // data vector
, IndexT *c   // pointer to merge target index vector
, IndexT *a   // pointer to merge source index vector a
, IndexT *b   // pointer to merge source index vector b
, IndexT na   // number of elements in merge source vector a
, IndexT nb   // number of elements in merge source vector b
)
{
  IndexT i,j,k,K=na+nb;
  for (i=0,j=0,k=0;k<K;k++) {
    if (i==na) {
      for (;k<K;k++)
        c[k] = b[j++];
      break;
    }
    if (j==nb) {
      for (;k<K;k++)
        c[k] = a[i++];
      break;
    }
    if (data[b[j]] < data[a[i]])
      c[k] = b[j++];
    else
      c[k] = a[i++];
  }
}

// ascending merge for sortordering
void ram_integer64_sortordermerge_asc(
  ValueT *c     // pointer to merge target data vector
, ValueT *a     // pointer to merge source data vector a
, ValueT *b     // pointer to merge source data vector b
, IndexT *ci    // pointer to merge target index vector
, IndexT *ai    // pointer to merge source index vector a
, IndexT *bi    // pointer to merge source index vector b
, IndexT na     // number of elements in merge source vector a
, IndexT nb     // number of elements in merge source vector b
)
{
  IndexT i,j,k,K=na+nb;
  for (i=0,j=0,k=0;k<K;k++) {
    if (i==na) {
      for (;k<K;k++) {
        ci[k] = bi[j];
        c[k] = b[j++];
      }
      break;
    }
    if (j==nb) {
      for (;k<K;k++) {
        ci[k] = ai[i];
        c[k] = a[i++];
      }
      break;
    }
    if (b[j] < a[i]) {
      ci[k] = bi[j];
      c[k] = b[j++];
    } else{
      ci[k] = ai[i];
      c[k] = a[i++];
    }
  }
}

void ram_integer64_sortmerge_desc(ValueT *c, ValueT *a, ValueT *b, IndexT na, IndexT nb)
{
  IndexT i,j,k,K=na+nb-1;
  for (i=na-1,j=nb-1,k=K;k>=0;k--) {
    if (i<0) {
      for (;k>=0;k--)
        c[k] = b[j--];
      break;
    }
    if (j<0) {
      for (;k>=0;k--)
        c[k] = a[i--];
      break;
    }
    if (a[i] < b[j])
      c[k] = a[i--];
    else
      c[k] = b[j--];
  }
}

void ram_integer64_ordermerge_desc(ValueT *data, IndexT *c, IndexT *a, IndexT *b, IndexT na, IndexT nb)
{
  IndexT i,j,k,K=na+nb-1;
  for (i=na-1,j=nb-1,k=K;k>=0;k--) {
    if (i<0) {
      for (;k>=0;k--)
        c[k] = b[j--];
      break;
    }
    if (j<0) {
      for (;k>=0;k--)
        c[k] = a[i--];
      break;
    }
    if (data[a[i]] < data[b[j]])
      c[k] = a[i--];
    else
      c[k] = b[j--];
  }
}

void ram_integer64_sortordermerge_desc(ValueT *c, ValueT *a, ValueT *b, IndexT *ci, IndexT *ai, IndexT *bi, IndexT na, IndexT nb)
{
  IndexT i,j,k,K=na+nb-1;
  for (i=na-1,j=nb-1,k=K;k>=0;k--) {
    if (i<0) {
      for (;k>=0;k--) {
        ci[k] = bi[j];
        c[k] = b[j--];
      }
      break;
    }
    if (j<0) {
      for (;k>=0;k--) {
        ci[k] = ai[i];
        c[k] = a[i--];
      }
      break;
    }
    if (a[i] < b[j]) {
      ci[k] = ai[i];
      c[k] = a[i--];
    } else{
      ci[k] = bi[j];
      c[k] = b[j--];
    }
  }
}


// merge sorting b ascending leaving result in a (following Sedgewick 8.4 Mergesort with no copying)
void ram_integer64_mergesort_asc_rec(
  ValueT *a   // pointer to target data vector
, ValueT *b   // pointer to source data vector
, IndexT l    // leftmost position to be sorted
, IndexT r    // rightmost position to be sorted
)
{
  IndexT m;
  if (r-l <= INSERTIONSORT_LIMIT_MERGE) {
    ram_integer64_insertionsort_asc(a, l, r);
    return;
  }
  m = (l+r)/2;
  ram_integer64_mergesort_asc_rec(b, a, l, m);
  ram_integer64_mergesort_asc_rec(b, a, m+1, r);
  ram_integer64_sortmerge_asc(a+l, b+l, b+m+1, m-l+1, r-m);
}
// merge ordering b ascending leaving result in a (following Sedgewick 8.4 Mergesort with no copying)
void ram_integer64_mergeorder_asc_rec(
ValueT *data  // pointer to data vector
, IndexT *a   // pointer to target index vector
, IndexT *b   // pointer to source index vector
, IndexT l    // leftmost position to be sorted
, IndexT r    // rightmost position to be sorted
)
{
  IndexT m;
  if (r-l <= INSERTIONSORT_LIMIT_MERGE) {
    ram_integer64_insertionorder_asc(data, a, l, r);
    return;
  }
  m = (l+r)/2;
  ram_integer64_mergeorder_asc_rec(data, b, a, l, m);
  ram_integer64_mergeorder_asc_rec(data, b, a, m+1, r);
  ram_integer64_ordermerge_asc(data, a+l, b+l, b+m+1, m-l+1, r-m);
}
// merge sortordering b ascending leaving result in a (following Sedgewick 8.4 Mergesort with no copying)
void ram_integer64_mergesortorder_asc_rec(
  ValueT *a   // pointer to target data vector
, ValueT *b   // pointer to source data vector
, IndexT *ai  // pointer to target index vector
, IndexT *bi  // pointer to source index vector
, IndexT l    // leftmost position to be sorted
, IndexT r    // rightmost position to be sorted
)
{
  IndexT m;
  if (r-l <= INSERTIONSORT_LIMIT_MERGE) {
    ram_integer64_insertionsortorder_asc(a, ai, l, r);
    return;
  }
  m = (l+r)/2;
  ram_integer64_mergesortorder_asc_rec(b, a, bi, ai, l, m);
  ram_integer64_mergesortorder_asc_rec(b, a, bi, ai, m+1, r);
  ram_integer64_sortordermerge_asc(a+l, b+l, b+m+1, ai+l, bi+l, bi+m+1, m-l+1, r-m);
}


void ram_integer64_mergesort_desc_rec(ValueT *a, ValueT *b, IndexT l, IndexT r)
{
  IndexT m;
  if (r-l <= INSERTIONSORT_LIMIT_MERGE) {
    ram_integer64_insertionsort_desc(a, l, r);
    return;
  }
  m = (l+r)/2;
  ram_integer64_mergesort_desc_rec(b, a, l, m);
  ram_integer64_mergesort_desc_rec(b, a, m+1, r);
  ram_integer64_sortmerge_desc(a+l, b+l, b+m+1, m-l+1, r-m);
}

void ram_integer64_mergeorder_desc_rec(ValueT *data, IndexT *a, IndexT *b, IndexT l, IndexT r)
{
  IndexT m;
  if (r-l <= INSERTIONSORT_LIMIT_MERGE) {
    ram_integer64_insertionorder_desc(data, a, l, r);
    return;
  }
  m = (l+r)/2;
  ram_integer64_mergeorder_desc_rec(data, b, a, l, m);
  ram_integer64_mergeorder_desc_rec(data, b, a, m+1, r);
  ram_integer64_ordermerge_desc(data, a+l, b+l, b+m+1, m-l+1, r-m);
}

void ram_integer64_mergesortorder_desc_rec(ValueT *a, ValueT *b, IndexT *ai, IndexT *bi, IndexT l, IndexT r)
{
  IndexT m;
  if (r-l <= INSERTIONSORT_LIMIT_MERGE) {
    ram_integer64_insertionsortorder_desc(a, ai, l, r);
    return;
  }
  m = (l+r)/2;
  ram_integer64_mergesortorder_desc_rec(b, a, bi, ai, l, m);
  ram_integer64_mergesortorder_desc_rec(b, a, bi, ai, m+1, r);
  ram_integer64_sortordermerge_desc(a+l, b+l, b+m+1, ai+l, bi+l, bi+m+1, m-l+1, r-m);
}





/* } === pure C stable merge sort for integer64 ================================================ */


// ascending partitioning of data between l and r around pivot in r
IndexT ram_integer64_quicksortpart_asc_no_sentinels(
ValueT *data    // pointer to data
, IndexT l      // leftmost position to be sorted
, IndexT r      // rightmost position to be sorted
)
{
  IndexT i = l - 1, j = r;
  ValueT v = data[r];
  for (;;) {
    ++i; while(data[i] < v) {if (j<=i)break; ++i;}; // explicit stop condition
    --j; while(v < data[j]) {if (j<=i)break; --j;};  // explicit stop condition
    if (j<=i)break;
    exch64(&data[i], &data[j]);
  }
  exch64(&data[i], &data[r]);
  return i;
}
IndexT ram_integer64_quicksortpart_desc_no_sentinels(ValueT *data, IndexT l, IndexT r) {
  IndexT i = l - 1, j = r;
  ValueT v = data[r];
  for (;;) {
    ++i; while(v < data[i]) {if (j<=i)break; ++i;}; // explicit stop condition
    --j; while(data[j] < v) {if (j<=i)break; --j;};  // explicit stop condition
    if (j<=i)break;
    exch64(&data[i], &data[j]);
  }
  exch64(&data[i], &data[r]);
  return i;
}
IndexT ram_integer64_quicksortorderpart_asc_no_sentinels(ValueT *data, IndexT *index, IndexT l, IndexT r) {
  IndexT i = l - 1, j = r;
  ValueT v = data[r];
  for (;;) {
    ++i; while(data[i] < v) {if (j<=i)break; ++i;}; // explicit stop condition
    --j; while(v < data[j]) {if (j<=i)break; --j;};  // explicit stop condition
    if (j<=i)break;
    exch_idx(&index[i], &index[j]);
    exch64(&data[i], &data[j]);
  }
  exch_idx(&index[i], &index[r]);
  exch64(&data[i], &data[r]);
  return i;
}
IndexT ram_integer64_quicksortorderpart_desc_no_sentinels(ValueT *data, IndexT *index, IndexT l, IndexT r) {
  IndexT i = l - 1, j = r;
  ValueT v = data[r];
  for (;;) {
    ++i; while(v < data[i]) {if (j<=i)break; ++i;}; // explicit stop condition
    --j; while(data[j] < v) {if (j<=i)break; --j;};  // explicit stop condition
    if (j<=i)break;
    exch_idx(&index[i], &index[j]);
    exch64(&data[i], &data[j]);
  }
  exch_idx(&index[i], &index[r]);
  exch64(&data[i], &data[r]);
  return i;
}
IndexT ram_integer64_quickorderpart_asc_no_sentinels(ValueT *data, IndexT *index, IndexT l, IndexT r) {
  IndexT i = l - 1, j = r;
  ValueT v = data[index[r]];
  for (;;) {
    ++i; while(data[index[i]] < v) {if (j<=i)break; ++i;}; // explicit stop condition
    --j; while(v < data[index[j]]) {if (j<=i)break; --j;};  // explicit stop condition
    if (j<=i)break;
    exch_idx(&index[i], &index[j]);
  }
  exch_idx(&index[i], &index[r]);
  return i;
}
IndexT ram_integer64_quickorderpart_desc_no_sentinels(ValueT *data, IndexT *index, IndexT l, IndexT r) {
  IndexT i = l - 1, j = r;
  ValueT v = data[index[r]];
  for (;;) {
    ++i; while(v < data[index[i]]) {if (j<=i)break; ++i;}; // explicit stop condition
    --j; while(data[index[j]] < v) {if (j<=i)break; --j;};  // explicit stop condition
    if (j<=i)break;
    exch_idx(&index[i], &index[j]);
  }
  exch_idx(&index[i], &index[r]);
  return i;
}




void ram_integer64_quicksort_asc_mdr3_no_sentinels(
ValueT *data
, IndexT l, IndexT r
) {
  if (INSERTIONSORT_LIMIT_QUICK < r-l) {
      IndexT m=(l+r)/2;
      m = ram_integer64_median3(data, l+randIndex((r-l)/2), m, r-randIndex((r-l)/2));
      exch64(&data[m], &data[r]);
      m = ram_integer64_quicksortpart_asc_no_sentinels(data, l, r);
      ram_integer64_quicksort_asc_mdr3_no_sentinels(data, l, m-1);
      ram_integer64_quicksort_asc_mdr3_no_sentinels(data, m+1, r);
  }
  else  ram_integer64_insertionsort_asc(data, l, r);
}
void ram_integer64_quicksortorder_asc_mdr3_no_sentinels(ValueT *data, IndexT *index, IndexT l, IndexT r) {
  if (INSERTIONSORT_LIMIT_QUICK < r-l) {
      IndexT m=(l+r)/2;
      m = ram_integer64_median3(data, l+randIndex((r-l)/2), m, r-randIndex((r-l)/2));
      exch_idx(&index[m], &index[r]);
      exch64(&data[m], &data[r]);
      m = ram_integer64_quicksortorderpart_asc_no_sentinels(data, index, l, r);
      ram_integer64_quicksortorder_asc_mdr3_no_sentinels(data, index, l, m-1);
      ram_integer64_quicksortorder_asc_mdr3_no_sentinels(data, index, m+1, r);
  }
  else  ram_integer64_insertionsortorder_asc(data, index, l, r);
}
void ram_integer64_quickorder_asc_mdr3_no_sentinels(ValueT *data, IndexT *index, IndexT l, IndexT r) {
  if (INSERTIONSORT_LIMIT_QUICK < r-l) {
      IndexT m=(l+r)/2;
      m = ram_integer64_median3(data, l+randIndex((r-l)/2), m, r-randIndex((r-l)/2));
      exch_idx(&index[m], &index[r]);
      exch64(&data[m], &data[r]);
      m = ram_integer64_quickorderpart_asc_no_sentinels(data, index, l, r);
      ram_integer64_quickorder_asc_mdr3_no_sentinels(data, index, l, m-1);
      ram_integer64_quickorder_asc_mdr3_no_sentinels(data, index, m+1, r);
  }
  else  ram_integer64_insertionorder_asc(data, index, l, r);
}

void ram_integer64_quicksort_desc_mdr3_no_sentinels(ValueT *data, IndexT l, IndexT r) {
  if (INSERTIONSORT_LIMIT_QUICK < r-l) {
      IndexT m=(l+r)/2;
      m = ram_integer64_median3(data, l+randIndex((r-l)/2), m, r-randIndex((r-l)/2));
      exch64(&data[m], &data[r]);
      m = ram_integer64_quicksortpart_desc_no_sentinels(data, l, r);
      ram_integer64_quicksort_desc_mdr3_no_sentinels(data, l, m-1);
      ram_integer64_quicksort_desc_mdr3_no_sentinels(data, m+1, r);
  }
  else  ram_integer64_insertionsort_desc(data, l, r);
}
void ram_integer64_quicksortorder_desc_mdr3_no_sentinels(ValueT *data, IndexT *index, IndexT l, IndexT r) {
  if (INSERTIONSORT_LIMIT_QUICK < r-l) {
      IndexT m=(l+r)/2;
      m = ram_integer64_median3(data, l+randIndex((r-l)/2), m, r-randIndex((r-l)/2));
      exch_idx(&index[m], &index[r]);
      exch64(&data[m], &data[r]);
      m = ram_integer64_quicksortorderpart_desc_no_sentinels(data, index, l, r);
      ram_integer64_quicksortorder_desc_mdr3_no_sentinels(data, index, l, m-1);
      ram_integer64_quicksortorder_desc_mdr3_no_sentinels(data, index, m+1, r);
  }
  else  ram_integer64_insertionsortorder_desc(data, index, l, r);
}
void ram_integer64_quickorder_desc_mdr3_no_sentinels(ValueT *data, IndexT *index, IndexT l, IndexT r) {
  if (INSERTIONSORT_LIMIT_QUICK < r-l) {
      IndexT m=(l+r)/2;
      m = ram_integer64_median3(data, l+randIndex((r-l)/2), m, r-randIndex((r-l)/2));
      exch_idx(&index[m], &index[r]);
      exch64(&data[m], &data[r]);
      m = ram_integer64_quickorderpart_desc_no_sentinels(data, index, l, r);
      ram_integer64_quickorder_desc_mdr3_no_sentinels(data, index, l, m-1);
      ram_integer64_quickorder_desc_mdr3_no_sentinels(data, index, m+1, r);
  }
  else  ram_integer64_insertionorder_desc(data, index, l, r);
}


void ram_integer64_quicksort_asc_intro(ValueT *data, IndexT l, IndexT r, int restlevel)
{
  IndexT m;
  if (restlevel>0) {
    if (INSERTIONSORT_LIMIT_QUICK < r-l) {
      m=(l+r)/2;
      m = ram_integer64_median3(data, l+randIndex((r-l)/2), m, r-randIndex((r-l)/2));
      exch64(&data[m], &data[r]);
      m = ram_integer64_quicksortpart_asc_no_sentinels(data, l, r);
      restlevel--;
      ram_integer64_quicksort_asc_intro(data, l, m-1, restlevel);
      ram_integer64_quicksort_asc_intro(data, m+1, r, restlevel);
    }
    else  ram_integer64_insertionsort_asc(data, l, r);
  } else{
    ram_integer64_shellsort_asc(data, l, r);
  }
}
void ram_integer64_quicksortorder_asc_intro(ValueT *data, IndexT *index, IndexT l, IndexT r, int restlevel)
{
  IndexT m;
  if (restlevel>0) {
    if (INSERTIONSORT_LIMIT_QUICK < r-l) {
      m=(l+r)/2;
      m = ram_integer64_median3(data, l+randIndex((r-l)/2), m, r-randIndex((r-l)/2));
      exch_idx(&index[m], &index[r]);
      exch64(&data[m], &data[r]);
      m = ram_integer64_quicksortorderpart_asc_no_sentinels(data, index, l, r);
      restlevel--;
      ram_integer64_quicksortorder_asc_intro(data, index, l, m-1, restlevel);
      ram_integer64_quicksortorder_asc_intro(data, index, m+1, r, restlevel);
    }
    else  ram_integer64_insertionsortorder_asc(data, index, l, r);
  } else{
    ram_integer64_shellsortorder_asc(data, index, l, r);
  }
}
void ram_integer64_quickorder_asc_intro(ValueT *data, IndexT *index, IndexT l, IndexT r, int restlevel)
{
  IndexT m;
  if (restlevel>0) {
    if (INSERTIONSORT_LIMIT_QUICK < r-l) {
      m=(l+r)/2;
      m = ram_integer64_median3index(data, index, l+randIndex((r-l)/2), m, r-randIndex((r-l)/2));
      exch_idx(&index[m], &index[r]);
      m = ram_integer64_quickorderpart_asc_no_sentinels(data, index, l, r);
      restlevel--;
      ram_integer64_quickorder_asc_intro(data, index, l, m-1, restlevel);
      ram_integer64_quickorder_asc_intro(data, index, m+1, r, restlevel);
    }
    else  ram_integer64_insertionorder_asc(data, index, l, r);
  } else{
    ram_integer64_shellorder_asc(data, index, l, r);
  }
}

void ram_integer64_quicksort_desc_intro(ValueT *data, IndexT l, IndexT r, int restlevel)
{
  IndexT m;
  if (restlevel>0) {
    if (INSERTIONSORT_LIMIT_QUICK < r-l) {
      m=(l+r)/2;
      m = ram_integer64_median3(data, l+randIndex((r-l)/2), m, r-randIndex((r-l)/2));
      exch64(&data[m], &data[r]);
      m = ram_integer64_quicksortpart_desc_no_sentinels(data, l, r);
      restlevel--;
      ram_integer64_quicksort_desc_intro(data, l, m-1, restlevel);
      ram_integer64_quicksort_desc_intro(data, m+1, r, restlevel);
    }
    else  ram_integer64_insertionsort_desc(data, l, r);
  } else{
    ram_integer64_shellsort_desc(data, l, r);
  }
}
void ram_integer64_quicksortorder_desc_intro(ValueT *data, IndexT *index, IndexT l, IndexT r, int restlevel)
{
  IndexT m;
  if (restlevel>0) {
    if (INSERTIONSORT_LIMIT_QUICK < r-l) {
      m=(l+r)/2;
      m = ram_integer64_median3(data, l+randIndex((r-l)/2), m, r-randIndex((r-l)/2));
      exch_idx(&index[m], &index[r]);
      exch64(&data[m], &data[r]);
      m = ram_integer64_quicksortorderpart_desc_no_sentinels(data, index, l, r);
      restlevel--;
      ram_integer64_quicksortorder_desc_intro(data, index, l, m-1, restlevel);
      ram_integer64_quicksortorder_desc_intro(data, index, m+1, r, restlevel);
    }
    else  ram_integer64_insertionsortorder_desc(data, index, l, r);
  } else{
    ram_integer64_shellsortorder_desc(data, index, l, r);
  }
}
void ram_integer64_quickorder_desc_intro(ValueT *data, IndexT *index, IndexT l, IndexT r, int restlevel)
{
  IndexT m;
  if (restlevel>0) {
    if (INSERTIONSORT_LIMIT_QUICK < r-l) {
      m=(l+r)/2;
      m = ram_integer64_median3(data, l+randIndex((r-l)/2), m, r-randIndex((r-l)/2));
      exch_idx(&index[m], &index[r]);
      m = ram_integer64_quickorderpart_desc_no_sentinels(data, index, l, r);
      restlevel--;
      ram_integer64_quickorder_desc_intro(data, index, l, m-1, restlevel);
      ram_integer64_quickorder_desc_intro(data, index, m+1, r, restlevel);
    }
    else  ram_integer64_insertionorder_desc(data, index, l, r);
  } else{
    ram_integer64_shellorder_desc(data, index, l, r);
  }
}


// LSB radix sorting
void ram_integer64_radixsort(
  UValueT * data        // RETURNED: pointer to data vector coerced to unsigned
, UValueT * auxdata     // MODIFIED: pointer to auxilliary data vector coerced to unsigned
, IndexT * stats        // MODIFIED: pointer to counting vector with nradixes*(pow(2, radixbits)+1) elements
, IndexT ** pstats      // MODIFIED: pointer to vector of pointers with nradixes elements
, IndexT n              // number of elements in data and auxdata
, int nradixes          // number of radixes where nradixes*radixbits==total number of bits
, int radixbits         // number of bits in radix where nradixes*radixbits==total number of bits
, Rboolean decreasing   // one of {0=ascending, 1=descending}
)
{
  IndexT w,b,b2,i;
  int nbuckets = pow(2, radixbits);
  int nbuckets1 = nbuckets - 1;
  UValueT bitmask, signmask, tmppatt;
  int nradixes1 = nradixes-1;
  int wradixbits;
  // Rprintf("nradixes=%d radixbits=%d nbuckets=%d\n", nradixes, radixbits, nbuckets); R_FlushConsole();

  // initialize bitmasks
  bitmask = 1;
  for (b=1;b<radixbits;b++)
    bitmask = bitmask<<1 | 1;
  signmask = bitmask ^ (bitmask >> 1);

  // initialize pstats pointer
  for (w=0;w<nradixes;w++)
    pstats[w] = stats + w * (nbuckets+1);
  // initialize stats
  for (w=0;w<nradixes;w++) {
    stats = pstats[w];
    for (i = 0; i < nbuckets; i++)
    stats[i] = 0;
    stats[nbuckets] = 1; // radix-noskip-flag
  }
  // count all buckets
  for (i = 0; i < n; i++) {
    tmppatt = data[i];
    pstats[0][tmppatt & bitmask]++;
    for (w=1;w<nradixes1;w++)
      pstats[w][(tmppatt >>= radixbits) & bitmask]++;
    pstats[nradixes1][ (((tmppatt >> radixbits) & bitmask) ^ signmask) ]++;
  }
  // cumulate stats and set skip-radix-flag
  if (decreasing) {
    for (w=0;w<nradixes;w++) {
      stats = pstats[w];
      b = stats[nbuckets1];
      if (b==n)
      stats[nbuckets] = 0; // radix-noskip-flag
      stats[nbuckets1] = 0;
      for (i=nbuckets1-1; i>=0; i--) {
      b2 = stats[i];
      if (b2==n)
        stats[nbuckets] = 0; // radix-noskip-flag
      stats[i] = b;
      b += b2;
      }
    }
  } else{
    for (w=0;w<nradixes;w++) {
      stats = pstats[w];
      b = stats[0];
      if (b==n)
      stats[nbuckets] = 0; // radix-noskip-flag
      stats[0] = 0;
      for (i=1; i<nbuckets; i++) {
      b2 = stats[i];
      if (b2==n)
        stats[nbuckets] = 0; // radix-noskip-flag
      stats[i] = b;
      b += b2;
      }
    }
  }
  // move the data
  for (b=0,w=0;w<nradixes;w++) {
    stats=pstats[w];
    // Rprintf("w=%d need=%d\n", w, stats[nbuckets]); R_FlushConsole();
    if (stats[nbuckets]) { // radix-noskip-flag
      wradixbits = w*radixbits;
      if (b % 2) {
          if (w == 0) {
          for (i = 0; i < n; i++) {
            data[stats[ auxdata[i] & bitmask]++ ] = auxdata[i];
          }
        } else if (w < nradixes1) {
          for (i = 0; i < n; i++) {
            data[stats[ auxdata[i]>>wradixbits & bitmask ]++] = auxdata[i];
          }
        } else{
          for (i = 0; i < n; i++) {
            data[stats[ (((auxdata[i] >> wradixbits) & bitmask) ^ signmask) ]++] = auxdata[i];
          }
        }
      } else{
          if (w == 0) {
          for (i = 0; i < n; i++) {
            auxdata[stats[ data[i] & bitmask ]++] = data[i];
          }
        } else if (w < nradixes1) {
          for (i = 0; i < n; i++) {
            auxdata[stats[ data[i]>>wradixbits & bitmask ]++] = data[i];
          }
        } else{
          for (i = 0; i < n; i++) {
            auxdata[stats[ (((data[i] >> wradixbits) & bitmask) ^ signmask) ]++] = data[i];
          }
        }
      }
    b++;
    }
  }
  // copy back in case of odd number of copies
    if (b % 2) {
    for (i = 0; i < n; i++)
      data[i] = auxdata[i];
    b++;
  }
  return;
}

// LSB radix sortordering
void ram_integer64_radixsortorder(
  UValueT * data          // RETURNED: pointer to data vector coerced to unsigned
, UValueT * auxdata       // MODIFIED: pointer to auxilliary data vector coerced to unsigned
, IndexT * index          // RETURNED: pointer to index vector
, IndexT * auxindex       // MODIFIED: pointer to auxilliary index vector
, IndexT * stats          // MODIFIED: pointer to counting vector with nradixes*(pow(2, radixbits)+1) elements
, IndexT ** pstats        // MODIFIED: pointer to vector of pointers with nradixes elements
, IndexT n                // number of elements in data and auxdata
, int nradixes            // number of radixes where nradixes*radixbits==total number of bits
, int radixbits           // number of bits in radix where nradixes*radixbits==total number of bits
, Rboolean decreasing     // one of {0=ascending, 1=descending}
)
{
  IndexT w,b,b2,i;
  int nbuckets = pow(2, radixbits);
  int nbuckets1 = nbuckets - 1;
  UValueT bitmask, signmask, tmppatt;
  int nradixes1 = nradixes-1;
  int wradixbits;
  //Rprintf("nradixes=%d radixbits=%d nbuckets=%d\n", nradixes, radixbits, nbuckets); R_FlushConsole();


  // initialize bitmasks
  bitmask = 1;
  for (b=1;b<radixbits;b++)
    bitmask = bitmask<<1 | 1;
  signmask = bitmask ^ (bitmask >> 1);

  // initialize pstats pointer
  for (w=0;w<nradixes;w++)
    pstats[w] = stats + w * (nbuckets+1);
  // initialize stats
  for (w=0;w<nradixes;w++) {
    stats = pstats[w];
    for (i = 0; i < nbuckets; i++)
    stats[i] = 0;
    stats[nbuckets] = 1; // radix-noskip-flag
  }
  // count all buckets
  for (i = 0; i < n; i++) {
    tmppatt = data[i];
    pstats[0][tmppatt & bitmask]++;
    for (w=1;w<nradixes1;w++)
      pstats[w][(tmppatt >>= radixbits) & bitmask]++;
    pstats[nradixes1][ (((tmppatt >> radixbits) & bitmask) ^ signmask) ]++;
  }
  // cumulate stats and set skip-radix-flag
  if (decreasing) {
    for (w=0;w<nradixes;w++) {
      stats = pstats[w];
      b = stats[nbuckets1];
      if (b==n)
      stats[nbuckets] = 0; // radix-noskip-flag
      stats[nbuckets1] = 0;
      for (i=nbuckets1-1; i>=0; i--) {
      b2 = stats[i];
      if (b2==n)
        stats[nbuckets] = 0; // radix-noskip-flag
      stats[i] = b;
      b += b2;
      }
    }
  } else{
    for (w=0;w<nradixes;w++) {
      stats = pstats[w];
      b = stats[0];
      if (b==n)
      stats[nbuckets] = 0; // radix-noskip-flag
      stats[0] = 0;
      for (i=1; i<nbuckets; i++) {
      b2 = stats[i];
      if (b2==n)
        stats[nbuckets] = 0; // radix-noskip-flag
      stats[i] = b;
      b += b2;
      }
    }
  }
  // move the data
  for (b=0,w=0;w<nradixes;w++) {
    stats=pstats[w];
    if (stats[nbuckets]) { // radix-noskip-flag
      wradixbits = w*radixbits;
      if (b % 2) {
          if (w == 0) {
          for (i = 0; i < n; i++) {
            b2 = stats[ auxdata[i] & bitmask ]++;
            index[b2] = auxindex[i];
            data[b2] = auxdata[i];
          }
        } else if (w < nradixes1) {
          for (i = 0; i < n; i++) {
            b2 = stats[ auxdata[i]>>wradixbits & bitmask ]++;
            index[b2] = auxindex[i];
            data[b2] = auxdata[i];
          }
        } else{
          for (i = 0; i < n; i++) {
            b2 = stats[ (((auxdata[i] >> wradixbits) & bitmask) ^ signmask) ]++;
            index[b2] = auxindex[i];
            data[b2] = auxdata[i];
          }
        }
      } else{
          if (w == 0) {
          for (i = 0; i < n; i++) {
            b2 = stats[ data[i] & bitmask ]++;
            auxindex[b2] = index[i];
            auxdata[b2] = data[i];
          }
        } else if (w < nradixes1) {
          for (i = 0; i < n; i++) {
            b2 = stats[ data[i]>>wradixbits & bitmask ]++;
            auxindex[b2] = index[i];
            auxdata[b2] = data[i];
          }
        } else{
          for (i = 0; i < n; i++) {
            b2 = stats[ (((data[i] >> wradixbits) & bitmask) ^ signmask) ]++;
            auxindex[b2] = index[i];
            auxdata[b2] = data[i];
          }
        }
      }
    b++;
    }
  }
  // copy back in case of odd number of copies
    if (b % 2) {
    for (i = 0; i < n; i++) {
      index[i] = auxindex[i];
      data[i] = auxdata[i];
    }
    b++;
  }
  return;
}

// LSB radix ordering
void ram_integer64_radixorder(
  UValueT * data          // UNCHANGED: pointer to data vector
, IndexT * index          // RETURNED: pointer to index vector
, IndexT * auxindex       // MODIFIED: pointer to auxilliary index vector
, IndexT * stats          // MODIFIED: pointer to counting vector with nradixes*(pow(2, radixbits)+1) elements
, IndexT ** pstats        // MODIFIED: pointer to vector of pointers with nradixes elements
, IndexT n                // number of elements in data and auxdata
, int nradixes            // number of radixes where nradixes*radixbits==total number of bits
, int radixbits           // number of bits in radix where nradixes*radixbits==total number of bits
, Rboolean decreasing     // one of {0=ascending, 1=descending}
)
{
  IndexT w,b,b2,i;
  int nbuckets = pow(2, radixbits);
  int nbuckets1 = nbuckets - 1;
  UValueT bitmask, signmask, tmppatt;
  int nradixes1 = nradixes-1;
  int wradixbits;
  //Rprintf("nradixes=%d radixbits=%d nbuckets=%d\n", nradixes, radixbits, nbuckets); R_FlushConsole();


  // initialize bitmasks
  bitmask = 1;
  for (b=1;b<radixbits;b++)
    bitmask = bitmask<<1 | 1;
  signmask = bitmask ^ (bitmask >> 1);

  // initialize pstats pointer
  for (w=0;w<nradixes;w++)
    pstats[w] = stats + w * (nbuckets+1);
  // initialize stats
  for (w=0;w<nradixes;w++) {
    stats = pstats[w];
    for (i = 0; i < nbuckets; i++)
    stats[i] = 0;
    stats[nbuckets] = 1; // radix-noskip-flag
  }
  // count all buckets
  for (i = 0; i < n; i++) {
    tmppatt = data[i];  // xx here we save the indirection through index
    pstats[0][tmppatt & bitmask]++;
    for (w=1;w<nradixes1;w++)
      pstats[w][(tmppatt >>= radixbits) & bitmask]++;
    pstats[nradixes1][ (((tmppatt >> radixbits) & bitmask) ^ signmask) ]++;
  }
  // cumulate stats and set skip-radix-flag
  if (decreasing) {
    for (w=0;w<nradixes;w++) {
      stats = pstats[w];
      b = stats[nbuckets1];
      if (b==n)
      stats[nbuckets] = 0; // radix-noskip-flag
      stats[nbuckets1] = 0;
      for (i=nbuckets1-1; i>=0; i--) {
      b2 = stats[i];
      if (b2==n)
        stats[nbuckets] = 0; // radix-noskip-flag
      stats[i] = b;
      b += b2;
      }
    }
  } else{
    for (w=0;w<nradixes;w++) {
      stats = pstats[w];
      b = stats[0];
      if (b==n)
      stats[nbuckets] = 0; // radix-noskip-flag
      stats[0] = 0;
      for (i=1; i<nbuckets; i++) {
      b2 = stats[i];
      if (b2==n)
        stats[nbuckets] = 0; // radix-noskip-flag
      stats[i] = b;
      b += b2;
      }
    }
  }
  // move the data
  for (b=0,w=0;w<nradixes;w++) {
    stats=pstats[w];
    if (stats[nbuckets]) { // radix-noskip-flag
      wradixbits = w*radixbits;
      if (b % 2) {
          if (w == 0) {
          for (i = 0; i < n; i++) {
            b2 = stats[ data[auxindex[i]] & bitmask ]++;
            index[b2] = auxindex[i];
          }
        } else if (w < nradixes1) {
          for (i = 0; i < n; i++) {
            b2 = stats[ data[auxindex[i]]>>wradixbits & bitmask ]++;
            index[b2] = auxindex[i];
          }
        } else{
          for (i = 0; i < n; i++) {
            b2 = stats[ (((data[auxindex[i]] >> wradixbits) & bitmask) ^ signmask) ]++;
            index[b2] = auxindex[i];
          }
        }
      } else{
          if (w == 0) {
          for (i = 0; i < n; i++) {
            b2 = stats[ data[index[i]] & bitmask ]++;
            auxindex[b2] = index[i];
          }
        } else if (w < nradixes1) {
          for (i = 0; i < n; i++) {
            b2 = stats[ data[index[i]]>>wradixbits & bitmask ]++;
            auxindex[b2] = index[i];
          }
        } else{
          for (i = 0; i < n; i++) {
            b2 = stats[ (((data[index[i]] >> wradixbits) & bitmask) ^ signmask) ]++;
            auxindex[b2] = index[i];
          }
        }
      }
    b++;
    }
  }
  // copy back in case of odd number of copies
    if (b % 2) {
    for (i = 0; i < n; i++) {
      index[i] = auxindex[i];
    }
    b++;
  }
  return;
}



/*****************************************************************************/
/**                                                                         **/
/**                           LOCAL FUNCTIONS                               **/
/**                                                                         **/
/*****************************************************************************/

// static

// returns uniform random index in range 0..(n-1)
static IndexT randIndex(
  IndexT n    // number of positions to random select from
) {
  IndexT r;
  //CRAN disallows rand: while(n <= (r=(((double)rand())*n) /RAND_MAX));
  // this is by factor 3 slower as long as we keep GetRNGstate(); PutRNGstate(); here.
  GetRNGstate();
    while((r = ((IndexT)(unif_rand()*n))) >= n) {}
    ;
  PutRNGstate();
  return r;
}

// returns one of {a,b,c} such that it represents the median of data[{a,b,c}]
static IndexT ram_integer64_median3(
ValueT *data  // pointer to data
, IndexT a    // pos in data
, IndexT b    // pos in data
, IndexT c    // pos in data
)
{
  ValueT da = data[a], db = data[b], dc = data[c];
  return (da < db) ?
      ((db < dc) ? b : (da < dc) ? c : a)
    : ((dc < db) ? b : (dc < da) ? c : a);
}

// returns one of {a,b,c} such that it represents the median of data[index[{a,b,c}]]
static IndexT ram_integer64_median3index(
  ValueT *data    // pointer to data
, IndexT *index   // index positions into data
, IndexT a        // pos in index
, IndexT b        // pos in index
, IndexT c        // pos in index
)
{
  ValueT da = data[index[a]], db = data[index[b]], dc = data[index[c]];
  return (da < db) ?
      ((db < dc) ? b : (da < dc) ? c : a)
    : ((dc < db) ? b : (dc < da) ? c : a);
}



/*****************************************************************************/
/**                                                                         **/
/**                            R/C INTERFACE                                **/
/**                                                                         **/
/*****************************************************************************/

SEXP r_ram_integer64_shellsort(
  SEXP x_            /* data vector */
, SEXP has_na_       /* logical scalar */
, SEXP na_last_      /* logical scalar */
, SEXP decreasing_   /* logical scalar */
)
{
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP, 1) );
  int ret;

  int n = LENGTH(x_);
  int has_na     = asLogical(has_na_);
  int na_last    = asLogical(na_last_);
  int decreasing = asLogical(decreasing_);

  R_Busy(1);
  ValueT *data;
  data = (ValueT *) REAL(x_);

  if (decreasing)
      ram_integer64_shellsort_desc(data, 0, n-1);
  else
      ram_integer64_shellsort_asc(data, 0, n-1);

  ret = ram_integer64_fixsortNA(data, n
  , has_na     // 0 for pure doubles, 1 if NA or NaN can be present
  , na_last    // 0 for NA NaN left, 1 for NA NaN right
  , decreasing // 0 for ascending, 1 for descending
  );

  INTEGER(ret_)[0] = ret;
  R_Busy(0);
  UNPROTECT(1);
  return ret_;
}

SEXP r_ram_integer64_shellsortorder(
  SEXP x_            /* data vector */
, SEXP index_        /* index vector */
, SEXP has_na_       /* logical scalar */
, SEXP na_last_      /* logical scalar */
, SEXP decreasing_   /* logical scalar */
)
{
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP, 1) );
  int ret;

  int n = LENGTH(x_);
  int has_na     = asLogical(has_na_);
  int na_last    = asLogical(na_last_);
  int decreasing = asLogical(decreasing_);

    R_Busy(1);
    ValueT *data;
    data = (ValueT *) REAL(x_);
    IndexT *index = INTEGER(index_);

    if (decreasing)
        ram_integer64_shellsortorder_desc(data, index, 0, n-1);
    else
        ram_integer64_shellsortorder_asc(data, index, 0, n-1);

    ret = ram_integer64_fixsortorderNA(data, index, n
    , has_na     // 0 for pure doubles, 1 if NA or NaN can be present
    , na_last    // 0 for NA NaN left, 1 for NA NaN right
    , decreasing // 0 for ascending, 1 for descending
    , 0  // no auxindex
    );

    INTEGER(ret_)[0] = ret;
    R_Busy(0);

  UNPROTECT(1);
  return ret_;
}

SEXP r_ram_integer64_shellorder(
  SEXP x_            /* data vector */
, SEXP index_        /* index vector */
, SEXP has_na_       /* logical scalar */
, SEXP na_last_      /* logical scalar */
, SEXP decreasing_   /* logical scalar */
)
{
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP, 1) );
  int ret;

  int i,n = LENGTH(x_);
  int has_na     = asLogical(has_na_);
  int na_last    = asLogical(na_last_);
  int decreasing = asLogical(decreasing_);

    R_Busy(1);
    ValueT *data;
    data = (ValueT *) REAL(x_);
    IndexT *index = INTEGER(index_);

    for (i = 0; i < n; i++)
    index[i]--;

    if (decreasing)
        ram_integer64_shellorder_desc(data, index, 0, n-1);
    else
        ram_integer64_shellorder_asc(data, index, 0, n-1);

    ret = ram_integer64_fixorderNA(data, index, n
    , has_na     // 0 for pure doubles, 1 if NA or NaN can be present
    , na_last    // 0 for NA NaN left, 1 for NA NaN right
    , decreasing // 0 for ascending, 1 for descending
    , 0  // no auxindex
    );

    for (i = 0; i < n; i++)
    index[i]++;

    INTEGER(ret_)[0] = ret;
    R_Busy(0);

  UNPROTECT(1);
  return ret_;
}



SEXP r_ram_integer64_mergesort(
  SEXP x_            /* data vector */
, SEXP has_na_       /* logical scalar */
, SEXP na_last_      /* logical scalar */
, SEXP decreasing_   /* logical scalar */
)
{
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP, 1) );
  int ret;

  int i,n = LENGTH(x_);
  int has_na     = asLogical(has_na_);
  int na_last    = asLogical(na_last_);
  int decreasing = asLogical(decreasing_);

  R_Busy(1);
  ValueT *data;
  data = (ValueT *) REAL(x_);
  ValueT *auxdata;
  auxdata = (ValueT *) R_alloc(n, sizeof(ValueT));

  for (i = 0; i < n; i++) {
    auxdata[i] = data[i];
  }

  if (decreasing)
      ram_integer64_mergesort_desc_rec(data, auxdata, 0, n-1);
  else
      ram_integer64_mergesort_asc_rec(data, auxdata, 0, n-1);
  ret = ram_integer64_fixsortNA(data, n
  , has_na     // 0 for pure doubles, 1 if NA or NaN can be present
  , na_last    // 0 for NA NaN left, 1 for NA NaN right
  , decreasing // 0 for ascending, 1 for descending
  );

  INTEGER(ret_)[0] = ret;
  R_Busy(0);
  UNPROTECT(1);
  return ret_;
}

SEXP r_ram_integer64_mergesortorder(
  SEXP x_            /* data vector */
, SEXP index_            /* index vector */
, SEXP has_na_       /* logical scalar */
, SEXP na_last_      /* logical scalar */
, SEXP decreasing_   /* logical scalar */
)
{
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP, 1) );
  int ret;

  int i,n = LENGTH(x_);
  int has_na     = asLogical(has_na_);
  int na_last    = asLogical(na_last_);
  int decreasing = asLogical(decreasing_);

  R_Busy(1);

  IndexT *index = INTEGER(index_);
  IndexT *auxindex;
  auxindex = (IndexT *) R_alloc(n, sizeof(IndexT));

      ValueT *data;
      data = (ValueT *) REAL(x_);
      ValueT *auxdata;
      auxdata = (ValueT *) R_alloc(n, sizeof(ValueT));

    for (i = 0; i < n; i++) {
      auxindex[i] = index[i];
      auxdata[i] = data[i];
    }

    if (decreasing)
        ram_integer64_mergesortorder_desc_rec(data, auxdata, index, auxindex, 0, n-1);
    else
        ram_integer64_mergesortorder_asc_rec(data, auxdata, index, auxindex, 0, n-1);
    ret = ram_integer64_fixsortorderNA(data, index, n
    , has_na     // 0 for pure doubles, 1 if NA or NaN can be present
    , na_last    // 0 for NA NaN left, 1 for NA NaN right
    , decreasing // 0 for ascending, 1 for descending
    , auxindex
    );

  INTEGER(ret_)[0] = ret;
  R_Busy(0);
  UNPROTECT(1);
  return ret_;
}

SEXP r_ram_integer64_mergeorder(
  SEXP x_            /* data vector */
, SEXP index_        /* index vector */
, SEXP has_na_       /* logical scalar */
, SEXP na_last_      /* logical scalar */
, SEXP decreasing_   /* logical scalar */
)
{
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP, 1) );
  int ret;

  int i,n = LENGTH(x_);
  int has_na     = asLogical(has_na_);
  int na_last    = asLogical(na_last_);
  int decreasing = asLogical(decreasing_);

  R_Busy(1);

  ValueT *data;
  data = (ValueT *) REAL(x_);
  IndexT *index = INTEGER(index_);
  IndexT *auxindex;
  auxindex = (IndexT *) R_alloc(n, sizeof(IndexT));

  for (i = 0; i < n; i++)
    index[i]--;

  for (i = 0; i < n; i++) {
    auxindex[i] = index[i];
  }

  if (decreasing)
      ram_integer64_mergeorder_desc_rec(data, index, auxindex, 0, n-1);
  else
      ram_integer64_mergeorder_asc_rec(data, index, auxindex, 0, n-1);

  ret = ram_integer64_fixorderNA(data, index, n
  , has_na     // 0 for pure doubles, 1 if NA or NaN can be present
  , na_last    // 0 for NA NaN left, 1 for NA NaN right
  , decreasing // 0 for ascending, 1 for descending
  , auxindex
  );

  for (i = 0; i < n; i++)
    index[i]++;

  INTEGER(ret_)[0] = ret;
  R_Busy(0);
  UNPROTECT(1);
  return ret_;
}

SEXP r_ram_integer64_quicksort(
  SEXP x_            /* data vector */
, SEXP has_na_       /* logical scalar */
, SEXP na_last_      /* logical scalar */
, SEXP decreasing_   /* logical scalar */
, SEXP restlevel_    /* logical scalar */
)
{
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP, 1) );
  int ret;

  int n = LENGTH(x_);
  int has_na     = asLogical(has_na_);
  int na_last    = asLogical(na_last_);
  int decreasing = asLogical(decreasing_);
  int restlevel = asInteger(restlevel_);

  R_Busy(1);
  ValueT *data;
  data = (ValueT *) REAL(x_);

  if (decreasing)
      ram_integer64_quicksort_desc_intro(data, 0, n-1, restlevel);
  else
      ram_integer64_quicksort_asc_intro(data, 0, n-1, restlevel);

  ret = ram_integer64_fixsortNA(data, n
  , has_na     // 0 for pure doubles, 1 if NA or NaN can be present
  , na_last    // 0 for NA NaN left, 1 for NA NaN right
  , decreasing // 0 for ascending, 1 for descending
  );

  INTEGER(ret_)[0] = ret;
  R_Busy(0);
  UNPROTECT(1);
  return ret_;
}

SEXP r_ram_integer64_quicksortorder(
  SEXP x_            /* data vector */
, SEXP index_        /* index vector */
, SEXP has_na_       /* logical scalar */
, SEXP na_last_      /* logical scalar */
, SEXP decreasing_   /* logical scalar */
, SEXP restlevel_    /* logical scalar */
)
{
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP, 1) );
  int ret;

  int n = LENGTH(x_);
  int has_na     = asLogical(has_na_);
  int na_last    = asLogical(na_last_);
  int decreasing = asLogical(decreasing_);
  int restlevel = asInteger(restlevel_);

    R_Busy(1);
    ValueT *data;
    data = (ValueT *) REAL(x_);
    IndexT *index = INTEGER(index_);

    if (decreasing)
      ram_integer64_quicksortorder_desc_intro(data, index, 0, n-1, restlevel);
    else
      ram_integer64_quicksortorder_asc_intro(data, index, 0, n-1, restlevel);

    ret = ram_integer64_fixsortorderNA(data, index, n
    , has_na     // 0 for pure doubles, 1 if NA or NaN can be present
    , na_last    // 0 for NA NaN left, 1 for NA NaN right
    , decreasing // 0 for ascending, 1 for descending
    , 0  // no auxindex
    );

    INTEGER(ret_)[0] = ret;
    R_Busy(0);

  UNPROTECT(1);
  return ret_;
}

SEXP r_ram_integer64_quickorder(
  SEXP x_            /* data vector */
, SEXP index_        /* index vector */
, SEXP has_na_       /* logical scalar */
, SEXP na_last_      /* logical scalar */
, SEXP decreasing_   /* logical scalar */
, SEXP restlevel_    /* logical scalar */
)
{
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP, 1) );
  int ret;

  int i,n = LENGTH(x_);
  int has_na     = asLogical(has_na_);
  int na_last    = asLogical(na_last_);
  int decreasing = asLogical(decreasing_);
  int restlevel = asInteger(restlevel_);

    R_Busy(1);
    ValueT *data;
    data = (ValueT *) REAL(x_);
    IndexT *index = INTEGER(index_);

    for (i = 0; i < n; i++)
      index[i]--;

    if (decreasing)
      ram_integer64_quickorder_desc_intro(data, index, 0, n-1, restlevel);
    else
      ram_integer64_quickorder_asc_intro(data, index, 0, n-1, restlevel);

    ret = ram_integer64_fixorderNA(data, index, n
    , has_na     // 0 for pure doubles, 1 if NA or NaN can be present
    , na_last    // 0 for NA NaN left, 1 for NA NaN right
    , decreasing // 0 for ascending, 1 for descending
    , 0  // no auxindex
    );

    for (i = 0; i < n; i++)
      index[i]++;

    INTEGER(ret_)[0] = ret;
    R_Busy(0);

  UNPROTECT(1);
  return ret_;
}

SEXP r_ram_integer64_radixsort(
  SEXP x_            /* data vector */
, SEXP has_na_       /* logical scalar */
, SEXP na_last_      /* logical scalar */
, SEXP decreasing_   /* logical scalar */
, SEXP radixbits_
)
{
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP, 1) );
  int ret;

  R_Busy(1);

  IndexT n = LENGTH(x_);
  int has_na     = asLogical(has_na_);
  int na_last    = asLogical(na_last_);
  int decreasing = asLogical(decreasing_);
  int radixbits = asInteger(radixbits_);
  int nradixes = 64 / radixbits;


  ValueT *data;
  data = (ValueT *) REAL(x_);
  ValueT *auxdata;
  auxdata = (ValueT *) R_alloc(n, sizeof(ValueT));

  IndexT *stats;
  stats = (IndexT *) R_alloc(nradixes*(pow(2, radixbits)+1), sizeof(IndexT));
  IndexT **pstats;
  pstats = (IndexT **) R_alloc(nradixes, sizeof(IndexT*));

  ram_integer64_radixsort(
    (UValueT *) data
  , (UValueT *) auxdata
  , stats
  , pstats
  , n
  , nradixes
  , radixbits
  , decreasing
  );
  ret = ram_integer64_fixsortNA(data, n
  , has_na     // 0 for pure doubles, 1 if NA or NaN can be present
  , na_last    // 0 for NA NaN left, 1 for NA NaN right
  , decreasing // 0 for ascending, 1 for descending
  );

  INTEGER(ret_)[0] = ret;
  R_Busy(0);
  UNPROTECT(1);
  return ret_;
}
SEXP r_ram_integer64_radixsortorder(
  SEXP x_            /* data vector */
, SEXP index_            /* index vector */
, SEXP has_na_       /* logical scalar */
, SEXP na_last_      /* logical scalar */
, SEXP decreasing_   /* logical scalar */
, SEXP radixbits_
)
{
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP, 1) );
  int ret;
  R_Busy(1);
  IndexT n = LENGTH(x_);
  int has_na     = asLogical(has_na_);
  int na_last    = asLogical(na_last_);
  int decreasing = asLogical(decreasing_);
  int radixbits = asInteger(radixbits_);
  int nradixes = 64 / radixbits;

  IndexT *index = INTEGER(index_);
  IndexT *auxindex;
  auxindex = (IndexT *) R_alloc(n, sizeof(IndexT));
  ValueT *data;
  data = (ValueT *) REAL(x_);
  ValueT *auxdata;
  auxdata = (ValueT *) R_alloc(n, sizeof(ValueT));

  IndexT *stats;
  stats = (IndexT *) R_alloc(nradixes*(pow(2, radixbits)+1), sizeof(IndexT));
  IndexT **pstats;
  pstats = (IndexT **) R_alloc(nradixes, sizeof(IndexT*));

    ram_integer64_radixsortorder(
    (UValueT *) data
  , (UValueT *) auxdata
  , index
  , auxindex
  , stats
  , pstats
  , n
  , nradixes
  , radixbits
  , decreasing
  );
  ret = ram_integer64_fixsortorderNA(data, index, n
  , has_na     // 0 for pure doubles, 1 if NA or NaN can be present
  , na_last    // 0 for NA NaN left, 1 for NA NaN right
  , decreasing // 0 for ascending, 1 for descending
  , auxindex
  );

  INTEGER(ret_)[0] = ret;
  R_Busy(0);
  UNPROTECT(1);
  return ret_;
}

SEXP r_ram_integer64_radixorder(
  SEXP x_            /* data vector */
, SEXP index_            /* index vector */
, SEXP has_na_       /* logical scalar */
, SEXP na_last_      /* logical scalar */
, SEXP decreasing_   /* logical scalar */
, SEXP radixbits_
)
{
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP, 1) );
  int ret;
  R_Busy(1);
  IndexT i,n = LENGTH(x_);
  int has_na     = asLogical(has_na_);
  int na_last    = asLogical(na_last_);
  int decreasing = asLogical(decreasing_);
  int radixbits = asInteger(radixbits_);
  int nradixes = 64 / radixbits;

  IndexT *index = INTEGER(index_);
  IndexT *auxindex;
  auxindex = (IndexT *) R_alloc(n, sizeof(IndexT));
  ValueT *data;
  data = (ValueT *) REAL(x_);

  IndexT *stats;
  stats = (IndexT *) R_alloc(nradixes*(pow(2, radixbits)+1), sizeof(IndexT));
  IndexT **pstats;
  pstats = (IndexT **) R_alloc(nradixes, sizeof(IndexT*));

  for (i = 0; i < n; i++) {
    index[i]--;
  }
  ram_integer64_radixorder(
    (UValueT *) data
  , index
  , auxindex
  , stats
  , pstats
  , n
  , nradixes
  , radixbits
  , decreasing
  );
  ret = ram_integer64_fixorderNA(data, index, n
  , has_na     // 0 for pure doubles, 1 if NA or NaN can be present
  , na_last    // 0 for NA NaN left, 1 for NA NaN right
  , decreasing // 0 for ascending, 1 for descending
  , auxindex
  );

  for (i = 0; i < n; i++)
    index[i]++;

  INTEGER(ret_)[0] = ret;
  R_Busy(0);
  UNPROTECT(1);
  return ret_;
}


/*****************************************************************************/
/**                                                                         **/
/**                                EOF                                      **/
/**                                                                         **/
/*****************************************************************************/
