#' Simple Mortality Age Model with Stan
#'
#' @export
#' @param data Stan-friendly data list created by XX function
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
fit_age <- function(data, ...) {
  standata <- data
  out <- rstan::sampling(stanmodels$mortality_age, data = standata, ...)
  return(out)
}
