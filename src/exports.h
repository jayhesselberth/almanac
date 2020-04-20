#ifndef ALMANAC_EXPORTS_H
#define ALMANAC_EXPORTS_H

#include "r.h"

extern "C" {

  sexp export_alma_seq_impl(sexp events,
                            sexp from,
                            sexp to,
                            sexp inclusive);

  sexp export_alma_next_impl(sexp x, sexp events, sexp inclusive);
  sexp export_alma_previous_impl(sexp x, sexp events, sexp inclusive);

  sexp export_alma_step_impl(sexp x, sexp n, sexp events, sexp size);

  sexp export_adj_following_impl(sexp x, sexp events);
  sexp export_adj_preceding_impl(sexp x, sexp events);
  sexp export_adj_modified_following_impl(sexp x, sexp events);
  sexp export_adj_modified_preceding_impl(sexp x, sexp events);
  sexp export_adj_nearest_impl(sexp x, sexp events);

  sexp export_test_month_from_days(sexp x);

  sexp export_almanac_init();

}

#endif