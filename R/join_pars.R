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

