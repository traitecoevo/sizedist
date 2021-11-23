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
           model1a = rstan::sampling(stanmodels$model1a, data = data, ...),
           model1b = rstan::sampling(stanmodels$model1b, data = data, ...),
           model1c = rstan::sampling(stanmodels$model1c, data = data, ...)
  )

  return(out)
}



