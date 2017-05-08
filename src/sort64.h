/*
# C-Header for sorting and ordering
# S3 atomic 64bit integers for R
# (c) 2011, 2012 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2011-12-11
# Last changed:  2012-10-03
*/


#ifndef _SORT64_INLCUDED
#define _SORT64_INLCUDED

/*****************************************************************************/
/**                                                                         **/
/**                            MODULES USED                                 **/
/**                                                                         **/
/*****************************************************************************/

#include <R.h>
#include <Rdefines.h>
//#include <Rinternals.h>
//CRAN disallows rand: #include <stdlib.h> // rand

#include "integer64.h"
//#include "timing.h"


/*****************************************************************************/
/**                                                                         **/
/**                      DEFINITIONS AND MACROS                             **/
/**                                                                         **/
/*****************************************************************************/


#define DEBUG_COUNTING 0
#define DEBUG_INIT // compare_counter = 0; move_counter = 0; //initTicks();
//#define DEBUG_RETURN getNewTicks()
#define DEBUG_RETURN ret;
// #define DEBUG_RETURN move_counter;
#define DEBUG_DONE Rprintf("compare_counter=%d  move_counter=%d\n", compare_counter, move_counter); R_FlushConsole(); //doneTicks();

#if defined(WIN32) || defined(WIN64) || defined(_WIN32_) || defined(_WIN64_) || defined(__WIN32__) || defined(__WIN64__) 
  #define MULTI_THREADING 0
#else
  #define MULTI_THREADING 1
#endif

#if MULTI_THREADING
#include <pthread.h>
#endif

// dummy for counting comp ops
#define COUNTLESS 

#define LESS(A,B) ((A)<(B))
#define GREATER(A, B) LESS((B), (A))

//#define MOVE(TO,FROM){move_counter++; TO=FROM;}
#define MOVE(TO,FROM) TO=FROM; 
#define EXCH(A,B,t) {MOVE(t,A) MOVE(A,B) MOVE(B,t)}
#define COMPEXCH(A,B,t) if (LESS(B,A)) EXCH(A,B,t)

#define KEY(A) (data[A])
#define KEYLESS(A,B) (LESS(KEY(A),KEY(B)))
#define KEYCOMPEXCH(A,B,t) if (KEYLESS(B,A)) EXCH(A,B,t)

#define COMPEXCHi(A,B,t,Ai,Bi,ti) if (LESS(B,A)) {EXCH(A,B,t) EXCH(Ai,Bi,ti)}

#define INSERTIONSORT_LIMIT_MERGE 16
#define INSERTIONSORT_LIMIT_QUICK 16


/*****************************************************************************/
/**                                                                         **/
/**                      TYPEDEFS AND STRUCTURES                            **/
/**                                                                         **/
/*****************************************************************************/

typedef int IndexT;
typedef long long ValueT;
typedef unsigned long long UValueT;


/*****************************************************************************/
/**                                                                         **/
/**                        EXPORTED VARIABLES                               **/
/**                                                                         **/
/*****************************************************************************/


#ifndef _SORT64_C_SRC

extern IndexT compare_counter;
extern IndexT move_counter;

#endif

/*****************************************************************************/
/**                                                                         **/
/**                        EXPORTED FUNCTIONS                               **/
/**                                                                         **/
/*****************************************************************************/

void R_Busy (int which);

// post sorting NA handling 
int ram_integer64_fixsortNA(
  ValueT *data     // RETURNED: pointer to data vector
, IndexT n         // length of data vector
, int has_na       // 0 for pure doubles, 1 if NA or NaN can be present
, int na_last      // 0 for placing NA NaN left, 1 for placing NA NaN right
, int decreasing   // 0 for ascending, 1 for descending (must match the same parameter in sorting)
);

// post sortordering NA handling 
int ram_integer64_fixsortorderNA(
  ValueT *data       // RETURNED: pointer to data vector
, IndexT *index      // RETURNED: pointer to index vector
, IndexT n           // length of vectors
, int has_na         // 0 for pure doubles, 1 if NA or NaN can be present
, int na_last        // 0 for placing NA NaN left, 1 for placing NA NaN right
, int decreasing     // 0 for ascending, 1 for descending (must match the same parameter in sorting)
, IndexT *auxindex   // MODIFIED: pointer to auxilliary index vector
);

// post ordering NA handling 
int ram_integer64_fixorderNA(
  ValueT *data          // UNCHANGED: pointer to data vector
, IndexT *index         // RETURNED: pointer to index vector
, IndexT n              // length of vectors
, int has_na            // 0 for pure doubles, 1 if NA or NaN can be present
, int na_last           // 0 for placing NA NaN left, 1 for placing NA NaN right
, int decreasing        // 0 for ascending, 1 for descending (must match the same parameter in sorting)
, IndexT *auxindex      // MODIFIED: pointer to auxilliary index vector
);


// ascending insertion sorting
void ram_integer64_insertionsort_asc(
  ValueT *data    // RETURNED: pointer to data vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
);

// ascending insertion sortordering
void ram_integer64_insertionsortorder_asc(
  ValueT *data    // RETURNED: pointer to data vector
, IndexT *index   // RETURNED: pointer to index vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
);

// ascending insertion sortordering
void ram_integer64_insertionorder_asc(
  ValueT *data    // UNCHANGED: pointer to data vector
, IndexT *index   // RETURNED: pointer to index vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
);

// descending insertion sorting
void ram_integer64_insertionsort_desc(
  ValueT *data    // RETURNED: pointer to data vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
);

// descending insertion sortordering
void ram_integer64_insertionsortorder_desc(
  ValueT *data    // RETURNED: pointer to data vector
, IndexT *index   // RETURNED: pointer to index vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
);

// descending insertion sortordering
void ram_integer64_insertionorder_desc(
  ValueT *data    // UNCHANGED: pointer to data vector
, IndexT *index   // RETURNED: pointer to index vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
);


// ascending shell sorting
void ram_integer64_shellsort_asc(
  ValueT *data    // RETURNED: pointer to data vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
);

// ascending shell sortordering
void ram_integer64_shellsortorder_asc(
  ValueT *data    // RETURNED: pointer to data vector
, IndexT *index   // RETURNED: pointer to index vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
);

// ascending shell sortordering
void ram_integer64_shellorder_asc(
  ValueT *data    // UNCHANGED: pointer to data vector
, IndexT *index   // RETURNED: pointer to index vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
);

// descending shell sorting
void ram_integer64_shellsort_desc(
  ValueT *data    // RETURNED: pointer to data vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
);

// descending shell sortordering
void ram_integer64_shellsortorder_desc(
  ValueT *data    // RETURNED: pointer to data vector
, IndexT *index   // RETURNED: pointer to index vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
);

// descending shell sortordering
void ram_integer64_shellorder_desc(
  ValueT *data    // UNCHANGED: pointer to data vector
, IndexT *index   // RETURNED: pointer to index vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
);


// ascending merge for sorting
void ram_integer64_sortmerge_asc(
  ValueT *c   // RETURNED: pointer to merge target data vector
, ValueT *a   // UNCHANGED: pointer to merge source data vector a
, ValueT *b   // UNCHANGED: pointer to merge source data vector b
, IndexT na   // number of elements in merge source vector a
, IndexT nb   // number of elements in merge source vector b
);

// ascending merge for ordering
void ram_integer64_ordermerge_asc(
ValueT *data  // UNCHANGED: pointer to data vector
, IndexT *c   // RETURNED: pointer to merge target index vector
, IndexT *a   // UNCHANGED: pointer to merge source index vector a
, IndexT *b   // UNCHANGED: pointer to merge source index vector b
, IndexT na   // number of elements in merge source vector a
, IndexT nb   // number of elements in merge source vector b
);

// ascending merge for sortordering
void ram_integer64_sortordermerge_asc(
  ValueT *c     // RETURNED: pointer to merge target data vector
, ValueT *a     // UNCHANGED: pointer to merge source data vector a
, ValueT *b     // UNCHANGED: pointer to merge source data vector b
, IndexT *ci    // RETURNED: pointer to merge target index vector
, IndexT *ai    // UNCHANGED: pointer to merge source index vector a
, IndexT *bi    // UNCHANGED: pointer to merge source index vector b
, IndexT na     // number of elements in merge source vector a
, IndexT nb     // number of elements in merge source vector b
);

// descending merge for sorting
void ram_integer64_sortmerge_desc(
  ValueT *c   // RETURNED: pointer to merge target data vector
, ValueT *a   // UNCHANGED: pointer to merge source data vector a
, ValueT *b   // UNCHANGED: pointer to merge source data vector b
, IndexT na   // number of elements in merge source vector a
, IndexT nb   // number of elements in merge source vector b
);

// descending merge for ordering
void ram_integer64_ordermerge_desc(
ValueT *data  // UNCHANGED: pointer to data vector
, IndexT *c   // RETURNED: pointer to merge target index vector
, IndexT *a   // UNCHANGED: pointer to merge source index vector a
, IndexT *b   // UNCHANGED: pointer to merge source index vector b
, IndexT na   // number of elements in merge source vector a
, IndexT nb   // number of elements in merge source vector b
);

// descending merge for sortordering
void ram_integer64_sortordermerge_desc(
  ValueT *c     // RETURNED: pointer to merge target data vector
, ValueT *a     // UNCHANGED: pointer to merge source data vector a
, ValueT *b     // UNCHANGED: pointer to merge source data vector b
, IndexT *ci    // RETURNED: pointer to merge target index vector
, IndexT *ai    // UNCHANGED: pointer to merge source index vector a
, IndexT *bi    // UNCHANGED: pointer to merge source index vector b
, IndexT na     // number of elements in merge source vector a
, IndexT nb     // number of elements in merge source vector b
);


// merge sorts b ascending and leaves result in a (following Sedgewick 8.4 Mergesort with no copying)
void ram_integer64_mergesort_asc_rec(
  ValueT *a   // RETURNED: pointer to target data vector 
, ValueT *b   // MODIFIED: pointer to source data vector 
, IndexT l    // leftmost position to be sorted
, IndexT r    // rightmost position to be sorted
);

// merge sorting b ascending leaving result in a (following Sedgewick 8.4 Mergesort with no copying)
void ram_integer64_mergeorder_asc_rec(
ValueT *data  // UNCHANGED: pointer to data vector
, IndexT *a   // RETURNED: pointer to target index vector 
, IndexT *b   // MODIFIED: pointer to source index vector 
, IndexT l    // leftmost position to be sorted
, IndexT r    // rightmost position to be sorted
);

// merge sortordering b ascending leaving result in a (following Sedgewick 8.4 Mergesort with no copying)
void ram_integer64_mergesortorder_asc_rec(
  ValueT *a   // RETURNED: pointer to target data vector
, ValueT *b   // MODIFIED: pointer to source data vector
, IndexT *ai  // RETURNED: pointer to target index vector 
, IndexT *bi  // MODIFIED: pointer to source index vector 
, IndexT l    // leftmost position to be sorted
, IndexT r    // rightmost position to be sorted
);


// merge sorts b descending and leaves result in a (following Sedgewick 8.4 Mergesort with no copying)
void ram_integer64_mergesort_desc_rec(
  ValueT *a   // RETURNED: pointer to target data vector 
, ValueT *b   // MODIFIED: pointer to source data vector 
, IndexT l    // leftmost position to be sorted
, IndexT r    // rightmost position to be sorted
);

// merge sorting b descending leaving result in a (following Sedgewick 8.4 Mergesort with no copying)
void ram_integer64_mergeorder_desc_rec(
  ValueT *data  // UNCHANGED: pointer to data vector
, IndexT *a   	// RETURNED: pointer to target index vector 
, IndexT *b   	// MODIFIED: pointer to source index vector 
, IndexT l    	// leftmost position to be sorted
, IndexT r    	// rightmost position to be sorted
);

// merge sortordering b descending leaving result in a (following Sedgewick 8.4 Mergesort with no copying)
void ram_integer64_mergesortorder_desc_rec(
  ValueT *a   // RETURNED: pointer to target data vector
, ValueT *b   // MODIFIED: pointer to source data vector
, IndexT *ai  // RETURNED: pointer to target index vector 
, IndexT *bi  // MODIFIED: pointer to source index vector 
, IndexT l    // leftmost position to be sorted
, IndexT r    // rightmost position to be sorted
);


// ascending partitioning of data between l and r around pivot in r for quick sorting
IndexT ram_integer64_quicksortpart_asc_no_sentinels(
  ValueT *data  // RETURNED: pointer to data
, IndexT l      // leftmost position to be sorted
, IndexT r      // rightmost position to be sorted
);

// ascending partitioning of data between l and r around pivot in r for quick ordering
IndexT ram_integer64_quickorderpart_asc_no_sentinels(
  ValueT *data  // UNCHANGED: pointer to data
, IndexT *index	// RETURNED: pointer to index
, IndexT l      // leftmost position to be sorted
, IndexT r      // rightmost position to be sorted
);

// ascending partitioning of data between l and r around pivot in r for quick sortordering
IndexT ram_integer64_quicksortorderpart_asc_no_sentinels(
  ValueT *data  // RETURNED: pointer to data
, IndexT *index	// RETURNED: pointer to index
, IndexT l      // leftmost position to be sorted
, IndexT r      // rightmost position to be sorted
);

// descending partitioning of data between l and r around pivot in r for quick sorting
IndexT ram_integer64_quicksortpart_desc_no_sentinels(
  ValueT *data  // RETURNED: pointer to data
, IndexT l      // leftmost position to be sorted
, IndexT r      // rightmost position to be sorted
);

// descending partitioning of data between l and r around pivot in r for quick ordering
IndexT ram_integer64_quickorderpart_desc_no_sentinels(
  ValueT *data  // UNCHANGED: pointer to data
, IndexT *index	// RETURNED: pointer to index
, IndexT l      // leftmost position to be sorted
, IndexT r      // rightmost position to be sorted
);

// descending partitioning of data between l and r around pivot in r for quick sortordering
IndexT ram_integer64_quicksortorderpart_desc_no_sentinels(
  ValueT *data  // RETURNED: pointer to data
, IndexT *index	// RETURNED: pointer to index
, IndexT l      // leftmost position to be sorted
, IndexT r      // rightmost position to be sorted
);


// ascending quick sorting
void ram_integer64_quicksort_asc_mdr3_no_sentinels(
  ValueT *data    // RETURNED: pointer to data vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
);

// ascending quick sortordering
void ram_integer64_quicksortorder_asc_mdr3_no_sentinels(
  ValueT *data    // RETURNED: pointer to data vector
, IndexT *index   // RETURNED: pointer to index vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
);

// ascending quick sortordering
void ram_integer64_quickorder_asc_mdr3_no_sentinels(
  ValueT *data    // UNCHANGED: pointer to data vector
, IndexT *index   // RETURNED: pointer to index vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
);

// descending quick sorting
void ram_integer64_quicksort_desc_mdr3_no_sentinels(
  ValueT *data    // RETURNED: pointer to data vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
);

// descending quick sortordering
void ram_integer64_quicksortorder_desc_mdr3_no_sentinels(
  ValueT *data    // RETURNED: pointer to data vector
, IndexT *index   // RETURNED: pointer to index vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
);

// descending quick sortordering
void ram_integer64_quickorder_desc_mdr3_no_sentinels(
  ValueT *data    // UNCHANGED: pointer to data vector
, IndexT *index   // RETURNED: pointer to index vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
);


// ascending intro sorting (switches to shellsort when no restlevels left)
void ram_integer64_quicksort_asc_intro(
  ValueT *data    // RETURNED: pointer to data vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
, int restlevel	  // number of remaining levels for quicksort recursion before switching to shellsort
);

// ascending intro sortordering (switches to shellsort when no restlevels left)
void ram_integer64_quicksortorder_asc_intro(
  ValueT *data    // RETURNED: pointer to data vector
, IndexT *index   // RETURNED: pointer to index vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
, int restlevel	  // number of remaining levels for quicksort recursion before switching to shellsort
);

// ascending intro sortordering (switches to shellsort when no restlevels left)
void ram_integer64_quickorder_asc_intro(
  ValueT *data    // UNCHANGED: pointer to data vector
, IndexT *index   // RETURNED: pointer to index vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
, int restlevel	  // number of remaining levels for quicksort recursion before switching to shellsort
);

// descending intro sorting (switches to shellsort when no restlevels left)
void ram_integer64_quicksort_desc_intro(
  ValueT *data    // RETURNED: pointer to data vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
, int restlevel	  // number of remaining levels for quicksort recursion before switching to shellsort
);

// descending intro sortordering (switches to shellsort when no restlevels left)
void ram_integer64_quicksortorder_desc_intro(
  ValueT *data    // RETURNED: pointer to data vector
, IndexT *index   // RETURNED: pointer to index vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
, int restlevel	  // number of remaining levels for quicksort recursion before switching to shellsort
);

// descending intro sortordering (switches to shellsort when no restlevels left)
void ram_integer64_quickorder_desc_intro(
  ValueT *data    // UNCHANGED: pointer to data vector
, IndexT *index   // RETURNED: pointer to index vector
, IndexT l        // leftmost position to be sorted
, IndexT r        // rightmost position to be sorted
, int restlevel	  // number of remaining levels for quicksort recursion before switching to shellsort
);


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
);

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
);

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
);

#endif

/*****************************************************************************/
/**                                                                         **/
/**                                EOF                                      **/
/**                                                                         **/
/*****************************************************************************/
