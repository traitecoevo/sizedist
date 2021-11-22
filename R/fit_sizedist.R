#' Fit size distribution model with Stan
#'
#' @export
#' @param data Stan-friendly data list created by `compose_data` function
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
fit_sizedist <- function(data, ...) {

  #Flattening out the pars list
  data <- c(purrr::discard(data, names(data) %in% "par"),
            purrr::discard(data, names(data) %in% "priors"),
            data$par,
            data$priors)

  out <-
    switch(data$model,
           model1a = rstan::sampling(stanmodels$mortality_age_wprior, data = data, ...),
           model1b = rstan::sampling(stanmodels$mortality_size_known_g_wprior, data = data, ...),
           model1c = rstan::sampling(stanmodels$mortality_size_growth_wprior, data = data, ...)
  )

  return(out)
}



