/*
# C-Header for binary search
# (c) 2011 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2011-12-11
# Last changed:  2011-12-11
*/

#include "sort64.h"

#define INTEGER64_BSEARCH_ASC_DOWN(data, l, r, value) \
{ \
IndexT m; \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (LESS(data[m], value)) \
      l = m + 1; \
    else \
      r = m; \
  } \
}
#define INTEGER64_BSEARCH_ASC_UP(data, l, r, value) \
{ \
IndexT m; \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (LESS(value, data[m])) \
      r = m; \
    else \
      l = m + 1; \
  } \
}

#define INTEGER64_LSEARCH_ASC_DOWN(data, l, r, value) \
{ \
  IndexT m,g,d=1; \
  while (l<r){ \
     g = l - 1 + d; \
     m = l + ((r - l) / 2); \
     if (g<m){ \
       if (LESS(data[g], value)){ \
         l = g + 1; \
         d *= 2; \
       }else{ \
         r = g; \
         break; \
       } \
     }else{ \
       if (LESS(data[m], value)) \
         l = m + 1; \
       else \
         r = m; \
       break; \
     } \
  } \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (LESS(data[m], value)) \
      l = m + 1; \
    else \
      r = m; \
  } \
}
#define INTEGER64_LSEARCH_ASC_UP(data, l, r, value) \
{ \
  IndexT m,g,d=1; \
  while (l<r){ \
     g = l - 1 + d; \
     m = l + ((r - l) / 2); \
     if (g<m){ \
       if (LESS(value, data[g])){ \
         r = g; \
         break; \
       }else{ \
         l = g + 1; \
         d *= 2; \
       } \
     }else{ \
       if (LESS(value, data[m])) \
         r = m; \
       else \
         l = m + 1; \
       break; \
     } \
  } \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (LESS(value, data[m])) \
      r = m; \
    else \
      l = m + 1; \
  } \
}


#define INTEGER64_RSEARCH_ASC_DOWN(data, l, r, value) \
{ \
  IndexT m,g,d=1; \
  while (l<r){ \
     g = r - d; \
     m = l + ((r - l) / 2); \
     if (g>m){ \
       if (LESS(data[g], value)){ \
         l = g + 1; \
         break; \
       }else{ \
         r = g; \
         d *= 2; \
       } \
     }else{ \
       if (LESS(data[m], value)) \
         l = m + 1; \
       else \
         r = m; \
       break; \
     } \
  } \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (LESS(data[m], value)) \
      l = m + 1; \
    else \
      r = m; \
  } \
}
#define INTEGER64_RSEARCH_ASC_UP(data, l, r, value) \
{ \
  IndexT m,g,d=1; \
  while (l<r){ \
     g = r - d; \
     m = l + ((r - l) / 2); \
     if (g>m){ \
       if (LESS(value, data[g])){ \
         r = g; \
         d *= 2; \
       }else{ \
         l = g + 1; \
         break; \
       } \
     }else{ \
       if (LESS(value, data[m])) \
         r = m; \
       else \
         l = m + 1; \
       break; \
     } \
  } \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (LESS(value, data[m])) \
      r = m; \
    else \
      l = m + 1; \
  } \
}

// desc is a clone of asc with LESS replaced by GREATER

#define INTEGER64_BSEARCH_DESC_DOWN(data, l, r, value) \
{ \
IndexT m; \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (GREATER(data[m], value)) \
      l = m + 1; \
    else \
      r = m; \
  } \
}
#define INTEGER64_BSEARCH_DESC_UP(data, l, r, value) \
{ \
IndexT m; \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (GREATER(value, data[m])) \
      r = m; \
    else \
      l = m + 1; \
  } \
}

#define INTEGER64_LSEARCH_DESC_DOWN(data, l, r, value) \
{ \
  IndexT m,g,d=1; \
  while (l<r){ \
     g = l - 1 + d; \
     m = l + ((r - l) / 2); \
     if (g<m){ \
       if (GREATER(data[g], value)){ \
         l = g + 1; \
         d *= 2; \
       }else{ \
         r = g; \
         break; \
       } \
     }else{ \
       if (GREATER(data[m], value)) \
         l = m + 1; \
       else \
         r = m; \
       break; \
     } \
  } \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (GREATER(data[m], value)) \
      l = m + 1; \
    else \
      r = m; \
  } \
}
#define INTEGER64_LSEARCH_DESC_UP(data, l, r, value) \
{ \
  IndexT m,g,d=1; \
  while (l<r){ \
     g = l - 1 + d; \
     m = l + ((r - l) / 2); \
     if (g<m){ \
       if (GREATER(value, data[g])){ \
         r = g; \
         break; \
       }else{ \
         l = g + 1; \
         d *= 2; \
       } \
     }else{ \
       if (GREATER(value, data[m])) \
         r = m; \
       else \
         l = m + 1; \
       break; \
     } \
  } \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (GREATER(value, data[m])) \
      r = m; \
    else \
      l = m + 1; \
  } \
}


#define INTEGER64_RSEARCH_DESC_DOWN(data, l, r, value) \
{ \
  IndexT m,g,d=1; \
  while (l<r){ \
     g = r - d; \
     m = l + ((r - l) / 2); \
     if (g>m){ \
       if (GREATER(data[g], value)){ \
         l = g + 1; \
         break; \
       }else{ \
         r = g; \
         d *= 2; \
       } \
     }else{ \
       if (GREATER(data[m], value)) \
         l = m + 1; \
       else \
         r = m; \
       break; \
     } \
  } \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (GREATER(data[m], value)) \
      l = m + 1; \
    else \
      r = m; \
  } \
}
#define INTEGER64_RSEARCH_DESC_UP(data, l, r, value) \
{ \
  IndexT m,g,d=1; \
  while (l<r){ \
     g = r - d; \
     m = l + ((r - l) / 2); \
     if (g>m){ \
       if (GREATER(value, data[g])){ \
         r = g; \
         d *= 2; \
       }else{ \
         l = g + 1; \
         break; \
       } \
     }else{ \
       if (GREATER(value, data[m])) \
         r = m; \
       else \
         l = m + 1; \
       break; \
     } \
  } \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (GREATER(value, data[m])) \
      r = m; \
    else \
      l = m + 1; \
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

#define INTEGER64_BSEARCH_ASC_GE(data, l, r, value, ret) \
  INTEGER64_BSEARCH_ASC_DOWN(data, l, r, value) \
  if (LESS(data[l], value)) \
    ret r+1; \
  else \
    ret l;   \

#define INTEGER64_BSEARCH_ASC_GT(data, l, r, value, ret) \
  INTEGER64_BSEARCH_ASC_UP(data, l, r, value) \
  if (LESS(value, data[l])) \
    ret l;   \
  else \
    ret r+1; \


#define INTEGER64_BSEARCH_ASC_LE(data, l, r, value, ret) \
  INTEGER64_BSEARCH_ASC_UP(data, l, r, value) \
  if (LESS(value, data[l])) \
    ret l-1; \
  else \
    ret r;   \


#define INTEGER64_BSEARCH_ASC_LT(data, l, r, value, ret) \
  INTEGER64_BSEARCH_ASC_DOWN(data, l, r, value) \
  if (LESS(data[l], value)) \
    ret r;   \
  else \
    ret l-1; \



#define INTEGER64_BSEARCH_DESC_EQ(data, l, r, value, ret) \
  INTEGER64_BSEARCH_DESC_DOWN(data, l, r, value) \
  if (LESS(value, data[l])) \
    ret -1; \
  else if (LESS(data[l], value)) \
    ret -1; \
  else \
    ret l;  \

#define INTEGER64_BSEARCH_DESC_GE(data, l, r, value, ret) \
  INTEGER64_BSEARCH_DESC_UP(data, l, r, value) \
 if (LESS(data[l], value)) \
    ret l-1; \
  else \
    ret l;   \

#define INTEGER64_BSEARCH_DESC_GT(data, l, r, value, ret) \
  INTEGER64_BSEARCH_DESC_DOWN(data, l, r, value) \
  if (LESS(value, data[l])) \
    ret l;   \
  else \
    ret l-1; \


#define INTEGER64_BSEARCH_DESC_LE(data, l, r, value, ret) \
  INTEGER64_BSEARCH_DESC_DOWN(data, l, r, value) \
 if (LESS(value, data[l])) \
    ret r+1; \
  else \
    ret l;   \


#define INTEGER64_BSEARCH_DESC_LT(data, l, r, value, ret) \
  INTEGER64_BSEARCH_DESC_UP(data, l, r, value) \
  if (LESS(data[l], value)) \
    ret l;   \
  else \
    ret r+1; \




#define INTEGER64_LSEARCH_ASC_EQ(data, l, r, value, ret) \
  INTEGER64_LSEARCH_ASC_DOWN(data, l, r, value) \
  if (LESS(value, data[l])) \
    ret -1; \
  else if (LESS(data[l], value)) \
    ret -1; \
  else \
    ret l;  \

#define INTEGER64_LSEARCH_ASC_GE(data, l, r, value, ret) \
  INTEGER64_LSEARCH_ASC_DOWN(data, l, r, value) \
  if (LESS(data[l], value)) \
    ret r+1; \
  else \
    ret l;   \

#define INTEGER64_LSEARCH_ASC_GT(data, l, r, value, ret) \
  INTEGER64_LSEARCH_ASC_UP(data, l, r, value) \
  if (LESS(value, data[l])) \
    ret l;   \
  else \
    ret r+1; \


#define INTEGER64_LSEARCH_ASC_LE(data, l, r, value, ret) \
  INTEGER64_LSEARCH_ASC_UP(data, l, r, value) \
  if (LESS(value, data[l])) \
    ret l-1; \
  else \
    ret r;   \


#define INTEGER64_LSEARCH_ASC_LT(data, l, r, value, ret) \
  INTEGER64_LSEARCH_ASC_DOWN(data, l, r, value) \
  if (LESS(data[l], value)) \
    ret r;   \
  else \
    ret l-1; \



#define INTEGER64_LSEARCH_DESC_EQ(data, l, r, value, ret) \
  INTEGER64_LSEARCH_DESC_DOWN(data, l, r, value) \
  if (LESS(value, data[l])) \
    ret -1; \
  else if (LESS(data[l], value)) \
    ret -1; \
  else \
    ret l;  \

#define INTEGER64_LSEARCH_DESC_GE(data, l, r, value, ret) \
  INTEGER64_LSEARCH_DESC_UP(data, l, r, value) \
 if (LESS(data[l], value)) \
    ret l-1; \
  else \
    ret l;   \

#define INTEGER64_LSEARCH_DESC_GT(data, l, r, value, ret) \
  INTEGER64_LSEARCH_DESC_DOWN(data, l, r, value) \
  if (LESS(value, data[l])) \
    ret l;   \
  else \
    ret l-1; \


#define INTEGER64_LSEARCH_DESC_LE(data, l, r, value, ret) \
  INTEGER64_LSEARCH_DESC_DOWN(data, l, r, value) \
 if (LESS(value, data[l])) \
    ret r+1; \
  else \
    ret l;   \


#define INTEGER64_LSEARCH_DESC_LT(data, l, r, value, ret) \
  INTEGER64_LSEARCH_DESC_UP(data, l, r, value) \
  if (LESS(data[l], value)) \
    ret l;   \
  else \
    ret r+1; \






#define INTEGER64_RSEARCH_ASC_EQ(data, l, r, value, ret) \
  INTEGER64_RSEARCH_ASC_DOWN(data, l, r, value) \
  if (LESS(value, data[l])) \
    ret -1; \
  else if (LESS(data[l], value)) \
    ret -1; \
  else \
    ret l;  \

#define INTEGER64_RSEARCH_ASC_GE(data, l, r, value, ret) \
  INTEGER64_RSEARCH_ASC_DOWN(data, l, r, value) \
  if (LESS(data[l], value)) \
    ret r+1; \
  else \
    ret l;   \

#define INTEGER64_RSEARCH_ASC_GT(data, l, r, value, ret) \
  INTEGER64_RSEARCH_ASC_UP(data, l, r, value) \
  if (LESS(value, data[l])) \
    ret l;   \
  else \
    ret r+1; \


#define INTEGER64_RSEARCH_ASC_LE(data, l, r, value, ret) \
  INTEGER64_RSEARCH_ASC_UP(data, l, r, value) \
  if (LESS(value, data[l])) \
    ret l-1; \
  else \
    ret r;   \


#define INTEGER64_RSEARCH_ASC_LT(data, l, r, value, ret) \
  INTEGER64_RSEARCH_ASC_DOWN(data, l, r, value) \
  if (LESS(data[l], value)) \
    ret r;   \
  else \
    ret l-1; \



#define INTEGER64_RSEARCH_DESC_EQ(data, l, r, value, ret) \
  INTEGER64_RSEARCH_DESC_DOWN(data, l, r, value) \
  if (LESS(value, data[l])) \
    ret -1; \
  else if (LESS(data[l], value)) \
    ret -1; \
  else \
    ret l;  \

#define INTEGER64_RSEARCH_DESC_GE(data, l, r, value, ret) \
  INTEGER64_RSEARCH_DESC_UP(data, l, r, value) \
 if (LESS(data[l], value)) \
    ret l-1; \
  else \
    ret l;   \

#define INTEGER64_RSEARCH_DESC_GT(data, l, r, value, ret) \
  INTEGER64_RSEARCH_DESC_DOWN(data, l, r, value) \
  if (LESS(value, data[l])) \
    ret l;   \
  else \
    ret l-1; \


#define INTEGER64_RSEARCH_DESC_LE(data, l, r, value, ret) \
  INTEGER64_RSEARCH_DESC_DOWN(data, l, r, value) \
 if (LESS(value, data[l])) \
    ret r+1; \
  else \
    ret l;   \


#define INTEGER64_RSEARCH_DESC_LT(data, l, r, value, ret) \
  INTEGER64_RSEARCH_DESC_UP(data, l, r, value) \
  if (LESS(data[l], value)) \
    ret l;   \
  else \
    ret r+1; \



IndexT integer64_bsearch_asc_EQ(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT integer64_bsearch_asc_GE(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT integer64_bsearch_asc_GT(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT integer64_bsearch_asc_LE(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT integer64_bsearch_asc_LT(ValueT *data, IndexT l, IndexT r, ValueT value);


IndexT integer64_bsearch_desc_EQ(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT integer64_bsearch_desc_GE(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT integer64_bsearch_desc_GT(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT integer64_bsearch_desc_LE(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT integer64_bsearch_desc_LT(ValueT *data, IndexT l, IndexT r, ValueT value);



IndexT integer64_lsearch_asc_EQ(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT integer64_lsearch_asc_GE(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT integer64_lsearch_asc_GT(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT integer64_lsearch_asc_LE(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT integer64_lsearch_asc_LT(ValueT *data, IndexT l, IndexT r, ValueT value);


IndexT integer64_lsearch_desc_EQ(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT integer64_lsearch_desc_GE(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT integer64_lsearch_desc_GT(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT integer64_lsearch_desc_LE(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT integer64_lsearch_desc_LT(ValueT *data, IndexT l, IndexT r, ValueT value);



IndexT integer64_rsearch_asc_EQ(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT integer64_rsearch_asc_GE(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT integer64_rsearch_asc_GT(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT integer64_rsearch_asc_LE(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT integer64_rsearch_asc_LT(ValueT *data, IndexT l, IndexT r, ValueT value);


IndexT integer64_rsearch_desc_EQ(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT integer64_rsearch_desc_GE(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT integer64_rsearch_desc_GT(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT integer64_rsearch_desc_LE(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT integer64_rsearch_desc_LT(ValueT *data, IndexT l, IndexT r, ValueT value);




#define INTEGER64_BOSEARCH_ASC_DOWN(data, index, l, r, value) \
{ \
IndexT m; \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (LESS(data[index[m]], value)) \
      l = m + 1; \
    else \
      r = m; \
  } \
}
#define INTEGER64_BOSEARCH_ASC_UP(data, index, l, r, value) \
{ \
IndexT m; \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (LESS(value, data[index[m]])) \
      r = m; \
    else \
      l = m + 1; \
  } \
}

#define INTEGER64_LOSEARCH_ASC_DOWN(data, index, l, r, value) \
{ \
  IndexT m,g,d=1; \
  while (l<r){ \
     g = l - 1 + d; \
     m = l + ((r - l) / 2); \
     if (g<m){ \
       if (LESS(data[index[g]], value)){ \
         l = g + 1; \
         d *= 2; \
       }else{ \
         r = g; \
         break; \
       } \
     }else{ \
       if (LESS(data[index[m]], value)) \
         l = m + 1; \
       else \
         r = m; \
       break; \
     } \
  } \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (LESS(data[index[m]], value)) \
      l = m + 1; \
    else \
      r = m; \
  } \
}
#define INTEGER64_LOSEARCH_ASC_UP(data, index, l, r, value) \
{ \
  IndexT m,g,d=1; \
  while (l<r){ \
     g = l - 1 + d; \
     m = l + ((r - l) / 2); \
     if (g<m){ \
       if (LESS(value, data[index[g]])){ \
         r = g; \
         break; \
       }else{ \
         l = g + 1; \
         d *= 2; \
       } \
     }else{ \
       if (LESS(value, data[index[m]])) \
         r = m; \
       else \
         l = m + 1; \
       break; \
     } \
  } \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (LESS(value, data[index[m]])) \
      r = m; \
    else \
      l = m + 1; \
  } \
}


#define INTEGER64_ROSEARCH_ASC_DOWN(data, index, l, r, value) \
{ \
  IndexT m,g,d=1; \
  while (l<r){ \
     g = r - d; \
     m = l + ((r - l) / 2); \
     if (g>m){ \
       if (LESS(data[index[g]], value)){ \
         l = g + 1; \
         break; \
       }else{ \
         r = g; \
         d *= 2; \
       } \
     }else{ \
       if (LESS(data[index[m]], value)) \
         l = m + 1; \
       else \
         r = m; \
       break; \
     } \
  } \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (LESS(data[index[m]], value)) \
      l = m + 1; \
    else \
      r = m; \
  } \
}
#define INTEGER64_ROSEARCH_ASC_UP(data, index, l, r, value) \
{ \
  IndexT m,g,d=1; \
  while (l<r){ \
     g = r - d; \
     m = l + ((r - l) / 2); \
     if (g>m){ \
       if (LESS(value, data[index[g]])){ \
         r = g; \
         d *= 2; \
       }else{ \
         l = g + 1; \
         break; \
       } \
     }else{ \
       if (LESS(value, data[index[m]])) \
         r = m; \
       else \
         l = m + 1; \
       break; \
     } \
  } \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (LESS(value, data[index[m]])) \
      r = m; \
    else \
      l = m + 1; \
  } \
}

// desc is a clone of asc with LESS replaced by GREATER

#define INTEGER64_BOSEARCH_DESC_DOWN(data, index, l, r, value) \
{ \
IndexT m; \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (GREATER(data[index[m]], value)) \
      l = m + 1; \
    else \
      r = m; \
  } \
}
#define INTEGER64_BOSEARCH_DESC_UP(data, index, l, r, value) \
{ \
IndexT m; \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (GREATER(value, data[index[m]])) \
      r = m; \
    else \
      l = m + 1; \
  } \
}

#define INTEGER64_LOSEARCH_DESC_DOWN(data, index, l, r, value) \
{ \
  IndexT m,g,d=1; \
  while (l<r){ \
     g = l - 1 + d; \
     m = l + ((r - l) / 2); \
     if (g<m){ \
       if (GREATER(data[index[g]], value)){ \
         l = g + 1; \
         d *= 2; \
       }else{ \
         r = g; \
         break; \
       } \
     }else{ \
       if (GREATER(data[index[m]], value)) \
         l = m + 1; \
       else \
         r = m; \
       break; \
     } \
  } \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (GREATER(data[index[m]], value)) \
      l = m + 1; \
    else \
      r = m; \
  } \
}
#define INTEGER64_LOSEARCH_DESC_UP(data, index, l, r, value) \
{ \
  IndexT m,g,d=1; \
  while (l<r){ \
     g = l - 1 + d; \
     m = l + ((r - l) / 2); \
     if (g<m){ \
       if (GREATER(value, data[index[g]])){ \
         r = g; \
         break; \
       }else{ \
         l = g + 1; \
         d *= 2; \
       } \
     }else{ \
       if (GREATER(value, data[index[m]])) \
         r = m; \
       else \
         l = m + 1; \
       break; \
     } \
  } \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (GREATER(value, data[index[m]])) \
      r = m; \
    else \
      l = m + 1; \
  } \
}


#define INTEGER64_ROSEARCH_DESC_DOWN(data, index, l, r, value) \
{ \
  IndexT m,g,d=1; \
  while (l<r){ \
     g = r - d; \
     m = l + ((r - l) / 2); \
     if (g>m){ \
       if (GREATER(data[index[g]], value)){ \
         l = g + 1; \
         break; \
       }else{ \
         r = g; \
         d *= 2; \
       } \
     }else{ \
       if (GREATER(data[index[m]], value)) \
         l = m + 1; \
       else \
         r = m; \
       break; \
     } \
  } \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (GREATER(data[index[m]], value)) \
      l = m + 1; \
    else \
      r = m; \
  } \
}
#define INTEGER64_ROSEARCH_DESC_UP(data, index, l, r, value) \
{ \
  IndexT m,g,d=1; \
  while (l<r){ \
     g = r - d; \
     m = l + ((r - l) / 2); \
     if (g>m){ \
       if (GREATER(value, data[index[g]])){ \
         r = g; \
         d *= 2; \
       }else{ \
         l = g + 1; \
         break; \
       } \
     }else{ \
       if (GREATER(value, data[index[m]])) \
         r = m; \
       else \
         l = m + 1; \
       break; \
     } \
  } \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (GREATER(value, data[index[m]])) \
      r = m; \
    else \
      l = m + 1; \
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

#define INTEGER64_BOSEARCH_ASC_GE(data, index, l, r, value, ret) \
  INTEGER64_BOSEARCH_ASC_DOWN(data, index, l, r, value) \
  if (LESS(data[index[l]], value)) \
    ret r+1; \
  else \
    ret l;   \

#define INTEGER64_BOSEARCH_ASC_GT(data, index, l, r, value, ret) \
  INTEGER64_BOSEARCH_ASC_UP(data, index, l, r, value) \
  if (LESS(value, data[index[l]])) \
    ret l;   \
  else \
    ret r+1; \


#define INTEGER64_BOSEARCH_ASC_LE(data, index, l, r, value, ret) \
  INTEGER64_BOSEARCH_ASC_UP(data, index, l, r, value) \
  if (LESS(value, data[index[l]])) \
    ret l-1; \
  else \
    ret r;   \


#define INTEGER64_BOSEARCH_ASC_LT(data, index, l, r, value, ret) \
  INTEGER64_BOSEARCH_ASC_DOWN(data, index, l, r, value) \
  if (LESS(data[index[l]], value)) \
    ret r;   \
  else \
    ret l-1; \



#define INTEGER64_BOSEARCH_DESC_EQ(data, index, l, r, value, ret) \
  INTEGER64_BOSEARCH_DESC_DOWN(data, index, l, r, value) \
  if (LESS(value, data[index[l]])) \
    ret -1; \
  else if (LESS(data[index[l]], value)) \
    ret -1; \
  else \
    ret l;  \

#define INTEGER64_BOSEARCH_DESC_GE(data, index, l, r, value, ret) \
  INTEGER64_BOSEARCH_DESC_UP(data, index, l, r, value) \
 if (LESS(data[index[l]], value)) \
    ret l-1; \
  else \
    ret l;   \

#define INTEGER64_BOSEARCH_DESC_GT(data, index, l, r, value, ret) \
  INTEGER64_BOSEARCH_DESC_DOWN(data, index, l, r, value) \
  if (LESS(value, data[index[l]])) \
    ret l;   \
  else \
    ret l-1; \


#define INTEGER64_BOSEARCH_DESC_LE(data, index, l, r, value, ret) \
  INTEGER64_BOSEARCH_DESC_DOWN(data, index, l, r, value) \
 if (LESS(value, data[index[l]])) \
    ret r+1; \
  else \
    ret l;   \


#define INTEGER64_BOSEARCH_DESC_LT(data, index, l, r, value, ret) \
  INTEGER64_BOSEARCH_DESC_UP(data, index, l, r, value) \
  if (LESS(data[index[l]], value)) \
    ret l;   \
  else \
    ret r+1; \




#define INTEGER64_LOSEARCH_ASC_EQ(data, index, l, r, value, ret) \
  INTEGER64_LOSEARCH_ASC_DOWN(data, index, l, r, value) \
  if (LESS(value, data[index[l]])) \
    ret -1; \
  else if (LESS(data[index[l]], value)) \
    ret -1; \
  else \
    ret l;  \

#define INTEGER64_LOSEARCH_ASC_GE(data, index, l, r, value, ret) \
  INTEGER64_LOSEARCH_ASC_DOWN(data, index, l, r, value) \
  if (LESS(data[index[l]], value)) \
    ret r+1; \
  else \
    ret l;   \

#define INTEGER64_LOSEARCH_ASC_GT(data, index, l, r, value, ret) \
  INTEGER64_LOSEARCH_ASC_UP(data, index, l, r, value) \
  if (LESS(value, data[index[l]])) \
    ret l;   \
  else \
    ret r+1; \


#define INTEGER64_LOSEARCH_ASC_LE(data, index, l, r, value, ret) \
  INTEGER64_LOSEARCH_ASC_UP(data, index, l, r, value) \
  if (LESS(value, data[index[l]])) \
    ret l-1; \
  else \
    ret r;   \


#define INTEGER64_LOSEARCH_ASC_LT(data, index, l, r, value, ret) \
  INTEGER64_LOSEARCH_ASC_DOWN(data, index, l, r, value) \
  if (LESS(data[index[l]], value)) \
    ret r;   \
  else \
    ret l-1; \



#define INTEGER64_LOSEARCH_DESC_EQ(data, index, l, r, value, ret) \
  INTEGER64_LOSEARCH_DESC_DOWN(data, index, l, r, value) \
  if (LESS(value, data[index[l]])) \
    ret -1; \
  else if (LESS(data[index[l]], value)) \
    ret -1; \
  else \
    ret l;  \

#define INTEGER64_LOSEARCH_DESC_GE(data, index, l, r, value, ret) \
  INTEGER64_LOSEARCH_DESC_UP(data, index, l, r, value) \
 if (LESS(data[index[l]], value)) \
    ret l-1; \
  else \
    ret l;   \

#define INTEGER64_LOSEARCH_DESC_GT(data, index, l, r, value, ret) \
  INTEGER64_LOSEARCH_DESC_DOWN(data, index, l, r, value) \
  if (LESS(value, data[index[l]])) \
    ret l;   \
  else \
    ret l-1; \


#define INTEGER64_LOSEARCH_DESC_LE(data, index, l, r, value, ret) \
  INTEGER64_LOSEARCH_DESC_DOWN(data, index, l, r, value) \
 if (LESS(value, data[index[l]])) \
    ret r+1; \
  else \
    ret l;   \


#define INTEGER64_LOSEARCH_DESC_LT(data, index, l, r, value, ret) \
  INTEGER64_LOSEARCH_DESC_UP(data, index, l, r, value) \
  if (LESS(data[index[l]], value)) \
    ret l;   \
  else \
    ret r+1; \






#define INTEGER64_ROSEARCH_ASC_EQ(data, index, l, r, value, ret) \
  INTEGER64_ROSEARCH_ASC_DOWN(data, index, l, r, value) \
  if (LESS(value, data[index[l]])) \
    ret -1; \
  else if (LESS(data[index[l]], value)) \
    ret -1; \
  else \
    ret l;  \

#define INTEGER64_ROSEARCH_ASC_GE(data, index, l, r, value, ret) \
  INTEGER64_ROSEARCH_ASC_DOWN(data, index, l, r, value) \
  if (LESS(data[index[l]], value)) \
    ret r+1; \
  else \
    ret l;   \

#define INTEGER64_ROSEARCH_ASC_GT(data, index, l, r, value, ret) \
  INTEGER64_ROSEARCH_ASC_UP(data, index, l, r, value) \
  if (LESS(value, data[index[l]])) \
    ret l;   \
  else \
    ret r+1; \


#define INTEGER64_ROSEARCH_ASC_LE(data, index, l, r, value, ret) \
  INTEGER64_ROSEARCH_ASC_UP(data, index, l, r, value) \
  if (LESS(value, data[index[l]])) \
    ret l-1; \
  else \
    ret r;   \


#define INTEGER64_ROSEARCH_ASC_LT(data, index, l, r, value, ret) \
  INTEGER64_ROSEARCH_ASC_DOWN(data, index, l, r, value) \
  if (LESS(data[index[l]], value)) \
    ret r;   \
  else \
    ret l-1; \



#define INTEGER64_ROSEARCH_DESC_EQ(data, index, l, r, value, ret) \
  INTEGER64_ROSEARCH_DESC_DOWN(data, index, l, r, value) \
  if (LESS(value, data[index[l]])) \
    ret -1; \
  else if (LESS(data[index[l]], value)) \
    ret -1; \
  else \
    ret l;  \

#define INTEGER64_ROSEARCH_DESC_GE(data, index, l, r, value, ret) \
  INTEGER64_ROSEARCH_DESC_UP(data, index, l, r, value) \
 if (LESS(data[index[l]], value)) \
    ret l-1; \
  else \
    ret l;   \

#define INTEGER64_ROSEARCH_DESC_GT(data, index, l, r, value, ret) \
  INTEGER64_ROSEARCH_DESC_DOWN(data, index, l, r, value) \
  if (LESS(value, data[index[l]])) \
    ret l;   \
  else \
    ret l-1; \


#define INTEGER64_ROSEARCH_DESC_LE(data, index, l, r, value, ret) \
  INTEGER64_ROSEARCH_DESC_DOWN(data, index, l, r, value) \
 if (LESS(value, data[index[l]])) \
    ret r+1; \
  else \
    ret l;   \


#define INTEGER64_ROSEARCH_DESC_LT(data, index, l, r, value, ret) \
  INTEGER64_ROSEARCH_DESC_UP(data, index, l, r, value) \
  if (LESS(data[index[l]], value)) \
    ret l;   \
  else \
    ret r+1; \



IndexT integer64_bosearch_asc_EQ(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

IndexT integer64_bosearch_asc_GE(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

IndexT integer64_bosearch_asc_GT(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

IndexT integer64_bosearch_asc_LE(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

IndexT integer64_bosearch_asc_LT(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);


IndexT integer64_bosearch_desc_EQ(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

IndexT integer64_bosearch_desc_GE(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

IndexT integer64_bosearch_desc_GT(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

IndexT integer64_bosearch_desc_LE(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

IndexT integer64_bosearch_desc_LT(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);



IndexT integer64_losearch_asc_EQ(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

IndexT integer64_losearch_asc_GE(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

IndexT integer64_losearch_asc_GT(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

IndexT integer64_losearch_asc_LE(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

IndexT integer64_losearch_asc_LT(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);


IndexT integer64_losearch_desc_EQ(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

IndexT integer64_losearch_desc_GE(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

IndexT integer64_losearch_desc_GT(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

IndexT integer64_losearch_desc_LE(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

IndexT integer64_losearch_desc_LT(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);



IndexT integer64_rosearch_asc_EQ(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

IndexT integer64_rosearch_asc_GE(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

IndexT integer64_rosearch_asc_GT(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

IndexT integer64_rosearch_asc_LE(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

IndexT integer64_rosearch_asc_LT(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);


IndexT integer64_rosearch_desc_EQ(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

IndexT integer64_rosearch_desc_GE(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

IndexT integer64_rosearch_desc_GT(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

IndexT integer64_rosearch_desc_LE(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);

IndexT integer64_rosearch_desc_LT(ValueT *data, IndexT *index, IndexT l, IndexT r, ValueT value);
