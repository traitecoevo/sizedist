#' literature_aged_data
#'
#' This function estimates the ages of fish by applying a growth rate from the literture
#' to the size of all individuals, for use in catch-curve models. This function should
#' be used to when making direct comparisons between the performance of our size distribution
#' model when size is known and fixed i.e. 'mortality_size_known_g.stan'
#'
#' @param data a dataframe containing a simulated larval fish population using simulate_catch_data() 
#' @param s0_av the average size of individuals at birth. This would be taken from the 
#'              intercept of published growth rate regression, i.e. linear model of size~age.
#' @param g_av  the average growth rate of a population. This would be taken from the 
#'              slope of published growth rate regression, i.e. linear model of size~age.
#'
#' @return a dataframe that now contains estimated ages based on linear growth rate from the literature.

age_data_with_literature_growth_rate <- function(data, s0_av = 3, g_av = 0.21) {
  
  data$estimated_age <- (data$size_sampled - s0_av)/g_av
  
  data
}