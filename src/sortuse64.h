#ifndef BIT64_SRC_SORTUSE64_H_
#define BIT64_SRC_SORTUSE64_H_

#include <Rinternals.h>

SEXP r_ram_integer64_all_na(SEXP x_);
SEXP r_ram_integer64_any_na(SEXP x_);
SEXP r_ram_integer64_issorted_asc(SEXP x_);
SEXP r_ram_integer64_mergeorder(SEXP x_, SEXP index_, SEXP has_na_, SEXP na_last_, SEXP decreasing_);
SEXP r_ram_integer64_mergesort(SEXP x_, SEXP has_na_, SEXP na_last_, SEXP decreasing_);
SEXP r_ram_integer64_mergesortorder(SEXP x_, SEXP index_, SEXP has_na_, SEXP na_last_, SEXP decreasing_);
SEXP r_ram_integer64_nacount(SEXP x_);
SEXP r_ram_integer64_orderdup_asc(SEXP table_, SEXP order_, SEXP method_, SEXP ret_);
SEXP r_ram_integer64_orderfin_asc(SEXP x_, SEXP table_, SEXP order_, SEXP method_, SEXP ret_);
SEXP r_ram_integer64_orderkey_asc(SEXP table_, SEXP order_, SEXP na_skip_num_, SEXP ret_);
SEXP r_ram_integer64_ordernut(SEXP table_, SEXP order_);
SEXP r_ram_integer64_orderord(SEXP x_, SEXP index_, SEXP na_count_, SEXP na_last_, SEXP decreasing_, SEXP ret_);
SEXP r_ram_integer64_orderpos_asc(SEXP x_, SEXP table_, SEXP order_, SEXP nomatch_, SEXP method_, SEXP ret_);
SEXP r_ram_integer64_orderrnk_asc(SEXP table_, SEXP order_, SEXP nacount_, SEXP ret_);
SEXP r_ram_integer64_ordertab_asc(SEXP table_, SEXP order_, SEXP denormalize_, SEXP keep_order_, SEXP ret_);
SEXP r_ram_integer64_ordertie_asc(SEXP table_, SEXP order_, SEXP ret_);
SEXP r_ram_integer64_orderuni_asc(SEXP table_, SEXP order_, SEXP keep_order_, SEXP ret_);
SEXP r_ram_integer64_orderupo_asc(SEXP table_, SEXP order_, SEXP keep_order_, SEXP ret_);
SEXP r_ram_integer64_quickorder(SEXP x_, SEXP index_, SEXP has_na_, SEXP na_last_, SEXP decreasing_, SEXP restlevel_);
SEXP r_ram_integer64_quicksort(SEXP x_, SEXP has_na_, SEXP na_last_, SEXP decreasing_, SEXP restlevel_);
SEXP r_ram_integer64_quicksortorder(SEXP x_, SEXP index_, SEXP has_na_, SEXP na_last_, SEXP decreasing_, SEXP restlevel_);
SEXP r_ram_integer64_radixorder(SEXP x_, SEXP index_, SEXP has_na_, SEXP na_last_, SEXP decreasing_, SEXP radixbits_);
SEXP r_ram_integer64_radixsort(SEXP x_, SEXP has_na_, SEXP na_last_, SEXP decreasing_, SEXP radixbits_);
SEXP r_ram_integer64_radixsortorder(SEXP x_, SEXP index_, SEXP has_na_, SEXP na_last_, SEXP decreasing_, SEXP radixbits_);
SEXP r_ram_integer64_shellorder(SEXP x_, SEXP index_, SEXP has_na_, SEXP na_last_, SEXP decreasing_);
SEXP r_ram_integer64_shellsort(SEXP x_, SEXP has_na_, SEXP na_last_, SEXP decreasing_);
SEXP r_ram_integer64_shellsortorder(SEXP x_, SEXP index_, SEXP has_na_, SEXP na_last_, SEXP decreasing_);
SEXP r_ram_integer64_sortfin_asc(SEXP x_, SEXP sorted_, SEXP method_, SEXP ret_);
SEXP r_ram_integer64_sortnut(SEXP sorted_);
SEXP r_ram_integer64_sortorderdup_asc(SEXP sorted_, SEXP order_, SEXP method_, SEXP ret_);
SEXP r_ram_integer64_sortorderkey_asc(SEXP sorted_, SEXP order_, SEXP na_skip_num_, SEXP ret_);
SEXP r_ram_integer64_sortorderord(SEXP x_, SEXP index_, SEXP na_count_, SEXP na_last_, SEXP decreasing_, SEXP ret_);
SEXP r_ram_integer64_sortorderpos_asc(SEXP x_, SEXP sorted_, SEXP order_, SEXP nomatch_, SEXP method_, SEXP ret_);
SEXP r_ram_integer64_sortorderrnk_asc(SEXP sorted_, SEXP order_, SEXP nacount_, SEXP ret_);
SEXP r_ram_integer64_sortordertab_asc(SEXP sorted_, SEXP order_, SEXP denormalize_, SEXP ret_);
SEXP r_ram_integer64_sortordertie_asc(SEXP sorted_, SEXP order_, SEXP ret_);
SEXP r_ram_integer64_sortorderuni_asc(SEXP table_, SEXP sorted_, SEXP order_, SEXP ret_);
SEXP r_ram_integer64_sortorderupo_asc(SEXP sorted_, SEXP order_, SEXP keep_order_, SEXP ret_);
SEXP r_ram_integer64_sortsrt(SEXP x_, SEXP na_count_, SEXP na_last_, SEXP decreasing_, SEXP ret_);
SEXP r_ram_integer64_sorttab_asc(SEXP sorted_, SEXP ret_);
SEXP r_ram_integer64_sortuni_asc(SEXP sorted_, SEXP ret_);

#endif  // BIT64_SRC_SORTUSE64_H_
