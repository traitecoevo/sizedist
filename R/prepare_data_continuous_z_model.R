
#' The function takes data frame used in catch-curve or Chapman-robson analysis     
#' and makes it useable in the continuous stan model.
#' This requires a simulated_sample2 dataframe created using lm_aged_data or alk_aged_data functions.
#'
#' @param data a datraframe of re-aged simulated data, created using lm_aged_data or alk_aged_data on simulate_catch_data.
#'
#' @return a list of data containing Y, individual fish ages - min fish age, and N, the length of the data.

prepare_data_continuous_z_model <- function(data, recruit_age = 0){

   data <- subset(data, data$estimated_age > recruit_age)
                          
                          
   cont_data <- list(
                     y = data$estimated_age - min(data$estimated_age),#Important to center around 0
                     N = length(data$estimated_age)
                          )
                          
   cont_data
}

