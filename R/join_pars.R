#' Join parameters to Stan-composed data
#'
#' @param data Data in Stan friendly format
#' @param pars model pars e.g. default_model_pars("model1")
#' @param var Name strings
#'
#' @return List with variables from pars appended
#' @export

join_pars <- function(data,
                      pars,
                      var){

  out  <- c(data,
            pars[var])

  #Order the list with model at the end I don't know if this is important but it looks nice
  out <- c(purrr::discard(out, is.character),
           purrr::keep(out, is.character))

  out

}


#' Add_pars Join_pars superseder
#'
#' @param data Data created by summarise_bin_counts
#' @param pars pars object
#' @param type Character string of model
#' @param prune If TRUE, hyperparams used for simulate_population() will be dropped
#' @export
#'
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
  out <- c(purrr::discard(out, is.character),
           purrr::keep(out, is.character))

  out

}

#' Function to prune down hyper-pars
#'
#' @param pars list object containing model$
#'
keep_fit_pars_model <- function(pars){
  pars <- switch(pars$model,
                 model1_a = purrr::keep(pars, names(pars) %in% c("model", "priors")),
                 model1_b = purrr::keep(pars, names(pars) %in% c("model", "g_av", "s0_av")),
                 model1_c = purrr::keep(pars, names(pars) %in% c("model", "s0_av"))
  )
  pars

  c(pars[1], pars[[2]]) #squashing priors down
}


#Function to return pars that are used to fit the model, switch based on pars$model (model prefix e.g model1)

#' Merge bin counts and growth data for model3
#'
#' @param counts_list Stan-friendly count data created using `summarise_by_counts`
#' @param growth_list Stan-friendly growth data created using `compose_growth_data`
#' @export

join_stan_data <- function(counts_list,
                           growth_list){
  c(counts_list,
    growth_list)
}

