/*
# C-Header for binary search
# (c) 2011-2024 Jens Oehlschägel
# (c) 2025-2026 Michael Chirico
# Licence: GPL2
# Provided 'as is', use at your own risk
*/

#ifndef BIT64_SRC_BSEARCH_H_
#define BIT64_SRC_BSEARCH_H_

#include "sort64.h"

#define INTEGER64_BSEARCH_ASC_DOWN(data, l, r, value) \
{ \
IndexT m; \
  while (l < r) { \
    m = l + ((r - l) / 2); \
    if (LESS(data[m], value)) \
      l = m + 1; \
    else \
      r = m; \
  } \
  }

#define INTEGER64_BSEARCH_ASC_EQ(data, l, r, value, ret) \
  INTEGER64_BSEARCH_ASC_DOWN(data, l, r, value) \
  if (LESS(value, data[l])) \
    ret -1; \
  else if (LESS(data[l], value)) \
    ret -1; \
  else \
    ret l;  \

#define INTEGER64_LSEARCH_ASC_DOWN(data, l, r, value) \
{ \
  IndexT m,g,d=1; \
  while (l < r) { \
     g = l - 1 + d; \
     m = l + ((r - l) / 2); \
     if (g<m) { \
       if (LESS(data[g], value)) { \
         l = g + 1; \
         d *= 2; \
       } else{ \
         r = g; \
         break; \
       } \
     } else{ \
       if (LESS(data[m], value)) \
         l = m + 1; \
       else \
         r = m; \
       break; \
     } \
  } \
  while (l < r) { \
    m = l + ((r - l) / 2); \
    if (LESS(data[m], value)) \
      l = m + 1; \
    else \
      r = m; \
  } \
      }

#define INTEGER64_LSEARCH_ASC_GE(data, l, r, value, ret) \
  INTEGER64_LSEARCH_ASC_DOWN(data, l, r, value) \
  if (LESS(data[l], value)) \
    ret r+1; \
  else \
    ret l;   \

#define INTEGER64_BOSEARCH_ASC_DOWN(data, index, l, r, value) \
{ \
IndexT m; \
  while (l < r) { \
    m = l + ((r - l) / 2); \
    if (LESS(data[index[m]], value)) \
      l = m + 1; \
    else \
      r = m; \
  } \
}

#define INTEGER64_BOSEARCH_ASC_EQ(data, index, l, r, value, ret) \
  INTEGER64_BOSEARCH_ASC_DOWN(data, index, l, r, value) \
  if (LESS(value, data[index[l]])) \
    ret -1; \
  else if (LESS(data[index[l]], value)) \
    ret -1; \
  else \
    ret l;  \

#define INTEGER64_LOSEARCH_ASC_DOWN(data, index, l, r, value) \
{ \
  IndexT m,g,d=1; \
  while (l < r) { \
     g = l - 1 + d; \
     m = l + ((r - l) / 2); \
     if (g<m) { \
       if (LESS(data[index[g]], value)) { \
         l = g + 1; \
         d *= 2; \
       } else{ \
         r = g; \
         break; \
       } \
     } else{ \
       if (LESS(data[index[m]], value)) \
         l = m + 1; \
       else \
         r = m; \
       break; \
     } \
  } \
  while (l < r) { \
    m = l + ((r - l) / 2); \
    if (LESS(data[index[m]], value)) \
      l = m + 1; \
    else \
      r = m; \
  } \
}

#define INTEGER64_LOSEARCH_ASC_GE(data, index, l, r, value, ret) \
  INTEGER64_LOSEARCH_ASC_DOWN(data, index, l, r, value) \
  if (LESS(data[index[l]], value)) \
    ret r+1; \
  else \
    ret l;   \

IndexT integer64_bsearch_asc_EQ(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT integer64_lsearch_asc_GE(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT integer64_bosearch_asc_EQ(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);
IndexT integer64_losearch_asc_GE(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

#endif  // BIT64_SRC_BSEARCH_H_
