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

static inline IndexT integer64_bsearch_asc_EQ(const ValueT *data, IndexT l, IndexT r, ValueT value) {
  while (l < r) {
    IndexT m = l + ((r - l) / 2);
    if (data[m] < value)
      l = m + 1;
    else
      r = m;
  }
  if (value < data[l] || data[l] < value)
    return -1;
  return l;
}

static inline IndexT integer64_lsearch_asc_GE(const ValueT *data, IndexT l, IndexT r, ValueT value) {
  IndexT m, g, d = 1;
  while (l < r) {
    g = l - 1 + d;
    m = l + ((r - l) / 2);
    if (g < m) {
      if (data[g] < value) {
        l = g + 1;
        d *= 2;
      } else {
        r = g;
        break;
      }
    } else {
      if (data[m] < value)
        l = m + 1;
      else
        r = m;
      break;
    }
  }
  while (l < r) {
    m = l + ((r - l) / 2);
    if (data[m] < value)
      l = m + 1;
    else
      r = m;
  }
  if (data[l] < value)
    return r + 1;
  return l;
}

static inline IndexT integer64_bosearch_asc_EQ(const ValueT *data, const IndexT *index, IndexT l, IndexT r, ValueT value) {
  while (l < r) {
    IndexT m = l + ((r - l) / 2);
    if (data[index[m]] < value)
      l = m + 1;
    else
      r = m;
  }
  if (value < data[index[l]] || data[index[l]] < value)
    return -1;
  return l;
}

static inline IndexT integer64_losearch_asc_GE(const ValueT *data, const IndexT *index, IndexT l, IndexT r, ValueT value) {
  IndexT m, g, d = 1;
  while (l < r) {
    g = l - 1 + d;
    m = l + ((r - l) / 2);
    if (g < m) {
      if (data[index[g]] < value) {
        l = g + 1;
        d *= 2;
      } else {
        r = g;
        break;
      }
    } else {
      if (data[index[m]] < value)
        l = m + 1;
      else
        r = m;
      break;
    }
  }
  while (l < r) {
    m = l + ((r - l) / 2);
    if (data[index[m]] < value)
      l = m + 1;
    else
      r = m;
  }
  if (data[index[l]] < value)
    return r + 1;
  return l;
}

#endif  // BIT64_SRC_BSEARCH_H_
