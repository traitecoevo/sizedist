#' Fit size distribution model with Stan
#'
#' @export
#' @param data Stan-friendly data list created by `compose_data` function
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
fit_sizedist <- function(data, ...) {
  out <-
    switch(data$model,
           model1 = rstan::sampling(stanmodels$mortality_age, data = data, ...),
           model2 = rstan::sampling(stanmodels$mortality_size_known_g, data = data, ...),
           model3 = rstan::sampling(stanmodels$mortality_size_growth, data = data, ...)
  )

  return(out)
}

