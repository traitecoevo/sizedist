#' Fit size distribution model with Stan
#'
#' @export
#' @param stan_data Stan-friendly data list created by XX function
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
fit_dist <- function(stan_data) {
  out <-
    switch(stan_data$model,
           model1 = rstan::sampling(stanmodels$mortality_age, data = stan_data, ...),
           model2 = rstan::sampling(stanmodels$mortality_size_known_g, data = stan_data, ...)
  )

  return(out)
}


