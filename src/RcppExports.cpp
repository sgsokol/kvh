// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/kvh.h"
#include <Rcpp.h>

using namespace Rcpp;

// kvh_read
RObject kvh_read(std::string fn, const std::string& comment_str, const bool strip_white, const bool skip_blank, const std::string& split_str, const bool follow_url);
RcppExport SEXP _kvh_kvh_read(SEXP fnSEXP, SEXP comment_strSEXP, SEXP strip_whiteSEXP, SEXP skip_blankSEXP, SEXP split_strSEXP, SEXP follow_urlSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type fn(fnSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type comment_str(comment_strSEXP);
    Rcpp::traits::input_parameter< const bool >::type strip_white(strip_whiteSEXP);
    Rcpp::traits::input_parameter< const bool >::type skip_blank(skip_blankSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type split_str(split_strSEXP);
    Rcpp::traits::input_parameter< const bool >::type follow_url(follow_urlSEXP);
    rcpp_result_gen = Rcpp::wrap(kvh_read(fn, comment_str, strip_white, skip_blank, split_str, follow_url));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_kvh_kvh_read", (DL_FUNC) &_kvh_kvh_read, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_kvh(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
