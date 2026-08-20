#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
SEXP as_list_integer64(SEXP);
SEXP abs_integer64(SEXP, SEXP);
SEXP all_integer64(SEXP, SEXP, SEXP);
SEXP any_integer64(SEXP, SEXP, SEXP);
SEXP as_bitstring_integer64(SEXP, SEXP);
SEXP as_character_integer64(SEXP, SEXP);
SEXP as_double_integer64(SEXP, SEXP);
SEXP as_integer64_bitstring(SEXP, SEXP);
SEXP as_integer64_character(SEXP, SEXP);
SEXP as_integer64_double(SEXP, SEXP);
SEXP as_integer64_integer(SEXP, SEXP);
SEXP as_integer_integer64(SEXP, SEXP);
SEXP as_logical_integer64(SEXP, SEXP);
SEXP cummax_integer64(SEXP, SEXP);
SEXP cummin_integer64(SEXP, SEXP);
SEXP cumprod_integer64(SEXP, SEXP);
SEXP cumsum_integer64(SEXP, SEXP);
SEXP diff_integer64(SEXP, SEXP, SEXP, SEXP);
SEXP divide_integer64_double(SEXP, SEXP, SEXP);
SEXP divide_double_integer64(SEXP, SEXP, SEXP); /* Ofek Shilon */
SEXP divide_integer64_integer64(SEXP, SEXP, SEXP);
SEXP EQ_integer64(SEXP, SEXP, SEXP);
SEXP GE_integer64(SEXP, SEXP, SEXP);
SEXP GT_integer64(SEXP, SEXP, SEXP);
SEXP hashdup_integer64(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP hashfin_integer64(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP hashfun_integer64(SEXP, SEXP, SEXP);
SEXP hashmap_integer64(SEXP, SEXP, SEXP, SEXP);
SEXP hashmaptab_integer64(SEXP, SEXP, SEXP, SEXP);
SEXP hashmapuni_integer64(SEXP, SEXP, SEXP, SEXP);
SEXP hashmapupo_integer64(SEXP, SEXP, SEXP, SEXP);
SEXP hashpos_integer64(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP hashrev_integer64(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP hashrin_integer64(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP hashtab_integer64(SEXP, SEXP, SEXP, SEXP);
SEXP hashuni_integer64(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP hashupo_integer64(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP intdiv_integer64(SEXP, SEXP, SEXP);
SEXP isna_integer64(SEXP, SEXP);
SEXP LE_integer64(SEXP, SEXP, SEXP);
SEXP lim_integer64(SEXP);
SEXP log10_integer64(SEXP, SEXP);
SEXP log2_integer64(SEXP, SEXP);
SEXP logbase_integer64(SEXP, SEXP, SEXP);
SEXP log_integer64(SEXP, SEXP);
SEXP logvect_integer64(SEXP, SEXP, SEXP);
SEXP LT_integer64(SEXP, SEXP, SEXP);
SEXP matmult_double_integer64(SEXP, SEXP, SEXP);
SEXP matmult_integer64_double(SEXP, SEXP, SEXP);
SEXP matmult_integer64_integer64(SEXP, SEXP, SEXP);
SEXP max_integer64(SEXP, SEXP, SEXP);
SEXP mean_integer64(SEXP, SEXP, SEXP);
SEXP min_integer64(SEXP, SEXP, SEXP);
SEXP minus_integer64(SEXP, SEXP, SEXP);
SEXP mod_integer64(SEXP, SEXP, SEXP);
SEXP NE_integer64(SEXP, SEXP, SEXP);
SEXP plus_integer64(SEXP, SEXP, SEXP);
SEXP power_integer64_double(SEXP, SEXP, SEXP);
SEXP power_integer64_integer64(SEXP, SEXP, SEXP);
SEXP prod_integer64(SEXP, SEXP, SEXP);
SEXP range_integer64(SEXP, SEXP, SEXP);
SEXP runif_integer64(SEXP, SEXP, SEXP);
SEXP r_ram_integer64_issorted_asc(SEXP);
SEXP r_ram_integer64_mergeorder(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_mergesort(SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_mergesortorder(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_all_na(SEXP);
SEXP r_ram_integer64_any_na(SEXP);
SEXP r_ram_integer64_nacount(SEXP);
SEXP r_ram_integer64_orderdup_asc(SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_orderfin_asc(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_orderkey_asc(SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_ordernut(SEXP, SEXP);
SEXP r_ram_integer64_orderord(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_orderpos_asc(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_orderrnk_asc(SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_ordertab_asc(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_ordertie_asc(SEXP, SEXP, SEXP);
SEXP r_ram_integer64_orderuni_asc(SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_orderupo_asc(SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_quickorder(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_quicksort(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_quicksortorder(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_radixorder(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_radixsort(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_radixsortorder(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_shellorder(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_shellsort(SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_shellsortorder(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_sortfin_asc(SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_sortnut(SEXP);
SEXP r_ram_integer64_sortorderdup_asc(SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_sortorderkey_asc(SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_sortorderord(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_sortorderpos_asc(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_sortorderrnk_asc(SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_sortordertab_asc(SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_sortordertie_asc(SEXP, SEXP, SEXP);
SEXP r_ram_integer64_sortorderuni_asc(SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_sortorderupo_asc(SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_sortsrt(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP r_ram_integer64_sorttab_asc(SEXP, SEXP);
SEXP r_ram_integer64_sortuni_asc(SEXP, SEXP);
SEXP seq_integer64(SEXP, SEXP, SEXP);
SEXP sign_integer64(SEXP, SEXP);
SEXP sqrt_integer64(SEXP, SEXP);
SEXP sum_integer64(SEXP, SEXP, SEXP);
SEXP times_integer64_double(SEXP, SEXP, SEXP);
SEXP times_integer64_integer64(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"as_list_integer64",                (DL_FUNC) &as_list_integer64,                1},
    {"abs_integer64",                    (DL_FUNC) &abs_integer64,                    2},
    {"all_integer64",                    (DL_FUNC) &all_integer64,                    3},
    {"any_integer64",                    (DL_FUNC) &any_integer64,                    3},
    {"as_bitstring_integer64",           (DL_FUNC) &as_bitstring_integer64,           2},
    {"as_character_integer64",           (DL_FUNC) &as_character_integer64,           2},
    {"as_double_integer64",              (DL_FUNC) &as_double_integer64,              2},
    {"as_integer64_bitstring",           (DL_FUNC) &as_integer64_bitstring,           2},
    {"as_integer64_character",           (DL_FUNC) &as_integer64_character,           2},
    {"as_integer64_double",              (DL_FUNC) &as_integer64_double,              2},
    {"as_integer64_integer",             (DL_FUNC) &as_integer64_integer,             2},
    {"as_integer_integer64",             (DL_FUNC) &as_integer_integer64,             2},
    {"as_logical_integer64",             (DL_FUNC) &as_logical_integer64,             2},
    {"cummax_integer64",                 (DL_FUNC) &cummax_integer64,                 2},
    {"cummin_integer64",                 (DL_FUNC) &cummin_integer64,                 2},
    {"cumprod_integer64",                (DL_FUNC) &cumprod_integer64,                2},
    {"cumsum_integer64",                 (DL_FUNC) &cumsum_integer64,                 2},
    {"diff_integer64",                   (DL_FUNC) &diff_integer64,                   4},
    {"divide_integer64_double",          (DL_FUNC) &divide_integer64_double,          3},
    {"divide_double_integer64",          (DL_FUNC) &divide_double_integer64,          3}, /* Ofek Shilon */
    {"divide_integer64_integer64",       (DL_FUNC) &divide_integer64_integer64,       3},
    {"EQ_integer64",                     (DL_FUNC) &EQ_integer64,                     3},
    {"GE_integer64",                     (DL_FUNC) &GE_integer64,                     3},
    {"GT_integer64",                     (DL_FUNC) &GT_integer64,                     3},
    {"hashdup_integer64",                (DL_FUNC) &hashdup_integer64,                5},
    {"hashfin_integer64",                (DL_FUNC) &hashfin_integer64,                5},
    {"hashfun_integer64",                (DL_FUNC) &hashfun_integer64,                3},
    {"hashmap_integer64",                (DL_FUNC) &hashmap_integer64,                4},
    {"hashmaptab_integer64",             (DL_FUNC) &hashmaptab_integer64,             4},
    {"hashmapuni_integer64",             (DL_FUNC) &hashmapuni_integer64,             4},
    {"hashmapupo_integer64",             (DL_FUNC) &hashmapupo_integer64,             4},
    {"hashpos_integer64",                (DL_FUNC) &hashpos_integer64,                6},
    {"hashrev_integer64",                (DL_FUNC) &hashrev_integer64,                7},
    {"hashrin_integer64",                (DL_FUNC) &hashrin_integer64,                6},
    {"hashtab_integer64",                (DL_FUNC) &hashtab_integer64,                4},
    {"hashuni_integer64",                (DL_FUNC) &hashuni_integer64,                5},
    {"hashupo_integer64",                (DL_FUNC) &hashupo_integer64,                5},
    {"intdiv_integer64",                 (DL_FUNC) &intdiv_integer64,                 3},
    {"isna_integer64",                   (DL_FUNC) &isna_integer64,                   2},
    {"LE_integer64",                     (DL_FUNC) &LE_integer64,                     3},
    {"lim_integer64",                    (DL_FUNC) &lim_integer64,                    1},
    {"log10_integer64",                  (DL_FUNC) &log10_integer64,                  2},
    {"log2_integer64",                   (DL_FUNC) &log2_integer64,                   2},
    {"logbase_integer64",                (DL_FUNC) &logbase_integer64,                3},
    {"log_integer64",                    (DL_FUNC) &log_integer64,                    2},
    {"logvect_integer64",                (DL_FUNC) &logvect_integer64,                3},
    {"LT_integer64",                     (DL_FUNC) &LT_integer64,                     3},
    {"matmult_double_integer64",         (DL_FUNC) &matmult_double_integer64,         3},
    {"matmult_integer64_double",         (DL_FUNC) &matmult_integer64_double,         3},
    {"matmult_integer64_integer64",      (DL_FUNC) &matmult_integer64_integer64,      3},
    {"max_integer64",                    (DL_FUNC) &max_integer64,                    3},
    {"mean_integer64",                   (DL_FUNC) &mean_integer64,                   3},
    {"min_integer64",                    (DL_FUNC) &min_integer64,                    3},
    {"minus_integer64",                  (DL_FUNC) &minus_integer64,                  3},
    {"mod_integer64",                    (DL_FUNC) &mod_integer64,                    3},
    {"NE_integer64",                     (DL_FUNC) &NE_integer64,                     3},
    {"plus_integer64",                   (DL_FUNC) &plus_integer64,                   3},
    {"power_integer64_double",           (DL_FUNC) &power_integer64_double,           3},
    {"power_integer64_integer64",        (DL_FUNC) &power_integer64_integer64,        3},
    {"prod_integer64",                   (DL_FUNC) &prod_integer64,                   3},
    {"range_integer64",                  (DL_FUNC) &range_integer64,                  3},
    {"runif_integer64",                  (DL_FUNC) &runif_integer64,                  3},
    {"r_ram_integer64_issorted_asc",     (DL_FUNC) &r_ram_integer64_issorted_asc,     1},
    {"r_ram_integer64_mergeorder",       (DL_FUNC) &r_ram_integer64_mergeorder,       5},
    {"r_ram_integer64_mergesort",        (DL_FUNC) &r_ram_integer64_mergesort,        4},
    {"r_ram_integer64_mergesortorder",   (DL_FUNC) &r_ram_integer64_mergesortorder,   5},
    {"r_ram_integer64_all_na",           (DL_FUNC) &r_ram_integer64_all_na,           1},
    {"r_ram_integer64_any_na",           (DL_FUNC) &r_ram_integer64_any_na,           1},
    {"r_ram_integer64_nacount",          (DL_FUNC) &r_ram_integer64_nacount,          1},
    {"r_ram_integer64_orderdup_asc",     (DL_FUNC) &r_ram_integer64_orderdup_asc,     4},
    {"r_ram_integer64_orderfin_asc",     (DL_FUNC) &r_ram_integer64_orderfin_asc,     5},
    {"r_ram_integer64_orderkey_asc",     (DL_FUNC) &r_ram_integer64_orderkey_asc,     4},
    {"r_ram_integer64_ordernut",         (DL_FUNC) &r_ram_integer64_ordernut,         2},
    {"r_ram_integer64_orderord",         (DL_FUNC) &r_ram_integer64_orderord,         6},
    {"r_ram_integer64_orderpos_asc",     (DL_FUNC) &r_ram_integer64_orderpos_asc,     6},
    {"r_ram_integer64_orderrnk_asc",     (DL_FUNC) &r_ram_integer64_orderrnk_asc,     4},
    {"r_ram_integer64_ordertab_asc",     (DL_FUNC) &r_ram_integer64_ordertab_asc,     5},
    {"r_ram_integer64_ordertie_asc",     (DL_FUNC) &r_ram_integer64_ordertie_asc,     3},
    {"r_ram_integer64_orderuni_asc",     (DL_FUNC) &r_ram_integer64_orderuni_asc,     4},
    {"r_ram_integer64_orderupo_asc",     (DL_FUNC) &r_ram_integer64_orderupo_asc,     4},
    {"r_ram_integer64_quickorder",       (DL_FUNC) &r_ram_integer64_quickorder,       6},
    {"r_ram_integer64_quicksort",        (DL_FUNC) &r_ram_integer64_quicksort,        5},
    {"r_ram_integer64_quicksortorder",   (DL_FUNC) &r_ram_integer64_quicksortorder,   6},
    {"r_ram_integer64_radixorder",       (DL_FUNC) &r_ram_integer64_radixorder,       6},
    {"r_ram_integer64_radixsort",        (DL_FUNC) &r_ram_integer64_radixsort,        5},
    {"r_ram_integer64_radixsortorder",   (DL_FUNC) &r_ram_integer64_radixsortorder,   6},
    {"r_ram_integer64_shellorder",       (DL_FUNC) &r_ram_integer64_shellorder,       5},
    {"r_ram_integer64_shellsort",        (DL_FUNC) &r_ram_integer64_shellsort,        4},
    {"r_ram_integer64_shellsortorder",   (DL_FUNC) &r_ram_integer64_shellsortorder,   5},
    {"r_ram_integer64_sortfin_asc",      (DL_FUNC) &r_ram_integer64_sortfin_asc,      4},
    {"r_ram_integer64_sortnut",          (DL_FUNC) &r_ram_integer64_sortnut,          1},
    {"r_ram_integer64_sortorderdup_asc", (DL_FUNC) &r_ram_integer64_sortorderdup_asc, 4},
    {"r_ram_integer64_sortorderkey_asc", (DL_FUNC) &r_ram_integer64_sortorderkey_asc, 4},
    {"r_ram_integer64_sortorderord",     (DL_FUNC) &r_ram_integer64_sortorderord,     6},
    {"r_ram_integer64_sortorderpos_asc", (DL_FUNC) &r_ram_integer64_sortorderpos_asc, 6},
    {"r_ram_integer64_sortorderrnk_asc", (DL_FUNC) &r_ram_integer64_sortorderrnk_asc, 4},
    {"r_ram_integer64_sortordertab_asc", (DL_FUNC) &r_ram_integer64_sortordertab_asc, 4},
    {"r_ram_integer64_sortordertie_asc", (DL_FUNC) &r_ram_integer64_sortordertie_asc, 3},
    {"r_ram_integer64_sortorderuni_asc", (DL_FUNC) &r_ram_integer64_sortorderuni_asc, 4},
    {"r_ram_integer64_sortorderupo_asc", (DL_FUNC) &r_ram_integer64_sortorderupo_asc, 4},
    {"r_ram_integer64_sortsrt",          (DL_FUNC) &r_ram_integer64_sortsrt,          5},
    {"r_ram_integer64_sorttab_asc",      (DL_FUNC) &r_ram_integer64_sorttab_asc,      2},
    {"r_ram_integer64_sortuni_asc",      (DL_FUNC) &r_ram_integer64_sortuni_asc,      2},
    {"seq_integer64",                    (DL_FUNC) &seq_integer64,                    3},
    {"sign_integer64",                   (DL_FUNC) &sign_integer64,                   2},
    {"sqrt_integer64",                   (DL_FUNC) &sqrt_integer64,                   2},
    {"sum_integer64",                    (DL_FUNC) &sum_integer64,                    3},
    {"times_integer64_double",           (DL_FUNC) &times_integer64_double,           3},
    {"times_integer64_integer64",        (DL_FUNC) &times_integer64_integer64,        3},
    {NULL, NULL, 0}
};


void R_init_bit64(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
