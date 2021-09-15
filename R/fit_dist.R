#' Fit size distribution model with Stan
#'
#' @export
#' @param stan_data Stan-friendly data list created by XX function
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
fit_dist <- function(data, ...) {
  out <-
    switch(data$model,
           model1 = rstan::sampling(stanmodels$mortality_age, data = data, ...),
           model2 = rstan::sampling(stanmodels$mortality_size_known_g, data = data, ...)
  )

  return(out)
}


