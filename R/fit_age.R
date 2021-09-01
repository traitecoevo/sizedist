#' Simple Mortality Age Model with Stan
#'
#' @export
#' @param data Stan-friendly data list created by XX function
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#' @rdname fit_
#'
fit_age <- function(data, ...) {
  out <- rstan::sampling(stanmodels$mortality_age_n, data = data, ...)
  return(out)
}

#' @rdname fit_
fit_known_g <- function(data, ...){
  out <- rstan::sampling(stanmodels$mortality_size_known_g, data = data, ...)
  return(out)
}
