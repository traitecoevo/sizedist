// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif


RcppExport SEXP _rcpp_module_boot_stan_fit4mortality_age_mod();
RcppExport SEXP _rcpp_module_boot_stan_fit4mortality_age_wprior_mod();
RcppExport SEXP _rcpp_module_boot_stan_fit4mortality_size_growth_mod();
RcppExport SEXP _rcpp_module_boot_stan_fit4mortality_size_known_g_mod();

static const R_CallMethodDef CallEntries[] = {
    {"_rcpp_module_boot_stan_fit4mortality_age_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4mortality_age_mod, 0},
    {"_rcpp_module_boot_stan_fit4mortality_age_wprior_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4mortality_age_wprior_mod, 0},
    {"_rcpp_module_boot_stan_fit4mortality_size_growth_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4mortality_size_growth_mod, 0},
    {"_rcpp_module_boot_stan_fit4mortality_size_known_g_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4mortality_size_known_g_mod, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_sizedist(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
