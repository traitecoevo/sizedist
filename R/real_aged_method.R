#' Returns the 'real' ages of simulated fish data for all fish instead of using a linear model of age-length-key
#'
#' This requires a simulated_sample dataframe from the simulate_catch_data function; source("R/generate_sample.R")
#'
#' @param data a dataframe with a simulated sample of larval fish, created using simulate_catch_data.
#' 
#'
#' @return a datraframe of 'real' aged simulated data for use in catch-curve and chapman-robson analyses.

real_aged_data <- function(data) {

          data$estimated_age <- data$age 
          
          data

}