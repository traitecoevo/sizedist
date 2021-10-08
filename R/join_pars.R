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
#'
#' @return
#' @export
add_pars <- function(data, pars = NULL, type = NULL, prune = TRUE){
  if(is.null(pars) & is.null(type)){
    rlang::abort("Hyperparameters or type of model must be supplied")
  }

   if(is.null(pars) & ! is.null(type) & is.character(type)){
    pars <- default_pars(type)
   }

  if(prune){
  prune_pars <- function(pars, vars){
    sim_vars <- c("R", "log10s0_sd", "g_av", "log10g_sd", "z_av", "log10z_sd")
    purrr::discard(pars, names(pars) %in% sim_vars)
  }

  pars <- prune_pars(pars)
  }

  out  <- c(data,
            pars)

  #Order the list with model at the end I don't know if this is important but it looks nice
  out <- c(purrr::discard(out, is.character),
           purrr::keep(out, is.character))

  out

}


#' Merge bin counts and growth data for model3
#'
#' @param counts_list Stan-friendly count data created using `summarise_by_counts`
#' @param growth_list Stan-friendly growth data created using `compose_growth_data`
#'
#' @return
#' @export
join_stan_data <- function(counts_list,
                           growth_list){
  c(counts_list,
    growth_list)
}

