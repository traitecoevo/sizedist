#' Join parameters object to Stan-composed data
#'
#' @param data Binned data created by compose_count_data()
#' @param pars pars object
#' @param type Character string of model e.g. "model1"
#' @param prune If TRUE, parameters used for simulate_population() will be dropped
#' @export

add_pars <- function(data, pars = NULL, type = NULL, prune = TRUE){
  # What if pars have already been added, don't want to do it again
  suppressWarnings(
    if( any(names(data) %in% "pars") ){
      rlang::abort("`pars` have already been added")
    }
  )
  suppressWarnings(
  if(! any(names(pars) %in% "priors")){
    abort("Priors must be added before using add_pars! - Try default_priors()")
  }
  )

  if(is.null(pars) & is.null(type)){
    abort("Hyperparameters or type of model must be supplied")
  }

   if(is.null(pars) & ! is.null(type) & is.character(type)){
    pars <- default_pars(type)
   }

  if(prune){
    pars <- keep_fit_pars_model(pars)
  }

  out  <- c(data,
            pars)

  #Order the list with model at the end I don't know if this is important but it looks nice
  out

}

#' Function to prune down hyper-pars
#'
#' @param pars list object containing model$

keep_fit_pars_model <- function(pars){
  pars <- switch(pars$model,
                 model1a = purrr::keep(pars, names(pars) %in% c("model", "priors")),
                 model1b = c(purrr::keep(pars, names(pars) %in% c("model", "priors")),
                             list(pars = purrr::keep(pars$pars, names(pars$pars) %in% c("g_av", "s0_av")))),
                 model1c = c(purrr::keep(pars, names(pars) %in% c("model", "priors")),
                             list(pars = purrr::keep(pars$pars, names(pars$pars) %in% "s0_av"))),
                 model1d = c(purrr::keep(pars, names(pars) %in% c("model", "priors"))),
                 model1e = c(purrr::keep(pars, names(pars) %in% c("model", "priors"))),
                 model2a = c(purrr::keep(pars, names(pars) %in% c("model", "priors"))),
                 model2b = c(purrr::keep(pars, names(pars) %in% c("model", "priors"))),
                 model2a_h1 = c(purrr::keep(pars, names(pars) %in% c("model", "priors"))),
                 model2a_h2 = c(purrr::keep(pars, names(pars) %in% c("model", "priors")))



  )
  pars

}
