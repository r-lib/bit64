#ifndef BIT64_SRC_HASH64_H_
#define BIT64_SRC_HASH64_H_

#include <Rinternals.h>

SEXP hashdup_integer64(SEXP hashdat_, SEXP bits_, SEXP hashpos_, SEXP nunique_, SEXP ret_);
SEXP hashfin_integer64(SEXP x_, SEXP hashdat_, SEXP bits_, SEXP hashpos_, SEXP ret_);
SEXP hashfun_integer64(SEXP x_, SEXP bits_, SEXP ret_);
SEXP hashmap_integer64(SEXP x_, SEXP bits_, SEXP hashpos_, SEXP nunique_);
SEXP hashmaptab_integer64(SEXP x_, SEXP bits_, SEXP hashpos_, SEXP nunique_);
SEXP hashmapuni_integer64(SEXP x_, SEXP bits_, SEXP hashpos_, SEXP nunique_);
SEXP hashmapupo_integer64(SEXP x_, SEXP bits_, SEXP hashpos_, SEXP nunique_);
SEXP hashpos_integer64(SEXP x_, SEXP hashdat_, SEXP bits_, SEXP hashpos_, SEXP nomatch_, SEXP ret_);
SEXP hashrev_integer64(SEXP x_, SEXP hashdat_, SEXP bits_, SEXP hashpos_, SEXP nunique_, SEXP nomatch_, SEXP ret_);
SEXP hashrin_integer64(SEXP x_, SEXP hashdat_, SEXP bits_, SEXP hashpos_, SEXP nunique_, SEXP ret_);
SEXP hashtab_integer64(SEXP x_, SEXP bits_, SEXP hashpos_, SEXP nunique_);
SEXP hashuni_integer64(SEXP hashdat_, SEXP bits_, SEXP hashpos_, SEXP keep_order_, SEXP ret_);
SEXP hashupo_integer64(SEXP hashdat_, SEXP bits_, SEXP hashpos_, SEXP keep_order_, SEXP ret_);

#endif  // BIT64_SRC_HASH64_H_
