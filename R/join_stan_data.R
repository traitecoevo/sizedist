#' Merge bin counts and growth data for model1c
#'
#' @param counts_list Stan-friendly count data created using `summarise_by_counts`
#' @param growth_list Stan-friendly growth data created using `compose_growth_data`
#' @export

join_stan_data <- function(counts_list,
                           growth_list){

  if(missing(counts_list) | missing(growth_list)){
    abort("Count/size-at-age list data must be supplied!")
  }

  c(counts_list,
    growth_list)
}

