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

