/*
# Header-Code
# S3 atomic 64bit integers for R
# (c) 2026 Michael Chirico
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2026-03-24
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

#define GOODISHIFTL64(x, y, z) ((y == 0 || x == 0) ? (z == x) : ((x > 0) ? (z > x) : (z < x)))

#define BITWAND64(x,y,ret) \
    if (x == NA_INTEGER64 || y == NA_INTEGER64) \
        ret = NA_INTEGER64; \
    else { \
        ret = x & y; \
    }

#define BITWAND(x,y,ret) \
    if (x == NA_INTEGER64 || y == NA_INTEGER) \
        ret = NA_INTEGER64; \
    else { \
        ret = x & (long long) y; \
    }

#define BITWOR64(x,y,ret) \
    if (x == NA_INTEGER64 || y == NA_INTEGER64) \
        ret = NA_INTEGER64; \
    else { \
        ret = x | y; \
    }

#define BITWOR(x,y,ret) \
    if (x == NA_INTEGER64 || y == NA_INTEGER) \
        ret = NA_INTEGER64; \
    else { \
        ret = x | (long long) y; \
    }

#define BITWXOR64(x,y,ret) \
    if (x == NA_INTEGER64 || y == NA_INTEGER64) \
        ret = NA_INTEGER64; \
    else { \
        ret = x ^ y; \
    }

#define BITWXOR(x,y,ret) \
    if (x == NA_INTEGER64 || y == NA_INTEGER) \
        ret = NA_INTEGER64; \
    else { \
        ret = x ^ (long long) y; \
    }

#define BITWSHIFTL(x,y,ret) \
    if (x == NA_INTEGER64 || y == NA_INTEGER64 || y < 0 || y > 63) \
        ret = NA_INTEGER64; \
    else { \
        ret = x << y; \
        if (!GOODISHIFTL64(x, y, ret)) \
          ret = NA_INTEGER64; \
    }

#define BITWSHIFTR(x,y,ret) \
    if (x == NA_INTEGER64 || y == NA_INTEGER || y < 0 || y > 63) \
        ret = NA_INTEGER64; \
    else { \
        ret = (unsigned long long) x >> y; \
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

