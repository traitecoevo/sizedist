#' Join parameters object to Stan-composed data
#'
#' @param data Binned data created by summarise_bin_counts
#' @param pars pars object
#' @param type Character string of model
#' @param prune If TRUE, parameters used for simulate_population() will be dropped
#' @export

add_pars <- function(data, pars = NULL, type = NULL, prune = TRUE){
  if(is.null(pars) & is.null(type)){
    rlang::abort("Hyperparameters or type of model must be supplied")
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
                             list(par = purrr::keep(pars$pars, names(pars$pars) %in% c("g_av", "s0_av")))),
                 model1c = c(purrr::keep(pars, names(pars) %in% c("model", "priors")),
                             list(par = purrr::keep(pars$pars, names(pars$pars) %in% "s0_av")))
  )
  pars

}
