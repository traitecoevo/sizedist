#' Re-ageing a simulated individual larval fish using linear model method   
#'
#' This requires a simulated_sample dataframe from the simulate_catch_data function; source("R/generate_sample.R")
#'
#' @param data a dataframe with a simulated sample of larval fish, created using simulate_catch_data.
#' @param N The number of individuals selected from each day 
#'
#' @return a datraframe of re-aged simulated data

lm_aged_data <- function(data,
                         N = 5){
  ageing_data <- data %>% group_by(day_born) %>% 
  top_n(N) #here we are selecting first 5 individuals from each day_born (note oldest ages may have less than 5 individuals left)

  ageing_data %>% 
  ggplot(aes(size_sampled)) + geom_histogram() + 
  geom_histogram(binwidth=1) + facet_grid(~age)


###estimate G_av from subset

  g_mod <- lm(size_sampled ~ age, data = ageing_data)

  plot(size_sampled ~ age, data = ageing_data)


###apply new estimated G to sizes of all fish sampled


  simulated_sample2 <- data

  simulated_sample2$estimated_ages <- (simulated_sample2$size_sampled - g_mod$coefficients[1])/g_mod$coefficients[2]

  simulated_sample2 %>% ggplot(aes(estimated_ages)) + geom_histogram() # + xlim(0, 12)


#Remove negetive ages

  simulated_sample2 <- simulated_sample2 %>% filter(estimated_ages >= 0)
  
  simulated_sample2
}


