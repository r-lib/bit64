/*
# C-Code for binary search
# (c) 2011 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2011-12-11
# Last changed:  2011-12-11
*/

#include "bsearch.h"

IndexT integer64_bsearch_asc_EQ(ValueT *data, IndexT l, IndexT r, ValueT value){
  INTEGER64_BSEARCH_ASC_EQ(data, l, r, value, return )
}

IndexT integer64_lsearch_asc_GE(ValueT *data, IndexT l, IndexT r, ValueT value){
  INTEGER64_LSEARCH_ASC_GE(data, l, r, value, return )
}

IndexT integer64_bosearch_asc_EQ(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value){
  INTEGER64_BOSEARCH_ASC_EQ(data, index, l, r, value, return)
}

IndexT integer64_losearch_asc_GE(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value){
  INTEGER64_LOSEARCH_ASC_GE(data, index, l, r, value, return)
}
