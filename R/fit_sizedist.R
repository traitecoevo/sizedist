#' Fit size distribution model with Stan
#'
#' @export
#' @param data Stan-friendly data list created by `compose_data` function
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
fit_sizedist <- function(data, ...){

  suppressWarnings(if(! any(names(data) %in% "pars") & data$model == "model1b" |
                      ! any(names(data) %in% "pars") & data$model == "model1c"
                      ){
    abort("`pars` have not been added to data! - try add_pars()")


  })

  #Flattening out the pars list
  data <- c(purrr::discard(data, names(data) %in% "pars"),
            purrr::discard(data, names(data) %in% "priors"),
            data$par,
            data$priors)

  out <-
    switch(data$model,
           model1a = rstan::sampling(stanmodels$model1a, data = data, ...),
           model1b = rstan::sampling(stanmodels$model1b, data = data, ...),
           model1c = rstan::sampling(stanmodels$model1c, data = data, ...),
           model1d = rstan::sampling(stanmodels$model1d, data = data, ...),
           model1e = rstan::sampling(stanmodels$model1e, data = data, ...),
           model2a = rstan::sampling(stanmodels$model2a, data = data, ...),
           model2b = rstan::sampling(stanmodels$model2b, data = data, ...)
  )

  return(out)
}



