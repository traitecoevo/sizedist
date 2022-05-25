#' The 'sizedist' package.
#'
#' @description Fitting size distributions to estimate growth and mortality
#'
#' @docType package
#' @name sizedist-package
#' @aliases sizedist
#' @useDynLib sizedist, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstan sampling
#'
#' @references
#' Stan Development Team (2020). RStan: the R interface to Stan. R package version 2.21.2. https://mc-stan.org
#'
NULL

# Recommendation by jennybc https://github.com/STAT545-UBC/Discussion/issues/451
## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
