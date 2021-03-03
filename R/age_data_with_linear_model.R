#' Re-ageing a simulated individual larval fish using linear model method   
#'
#' This requires a simulated_sample dataframe from the simulate_catch_data function; source("R/generate_sample.R")
#'
#' @param data a dataframe with a simulated sample of larval fish, created using simulate_catch_data.
#' @param N The number of individuals selected from each day 
#'
#' @return a datraframe of re-aged simulated data

age_data_with_linear_model <- function(data,
                                       bin_width_size = 0.1,
                                       rough_bin_width = 1.5,
                                       n_samples_each = 3,
                                       add_error = FALSE,
                                       age_var = 1,
                                       size_var = 0.1){
  
  #rounding function
  round_by_bin <- function(x, bin_width) {  
    round(x/bin_width, 0)*bin_width
  }
  
  ageing_data <- data %>%  
    mutate(
      rough_size = round_by_bin(size_sampled, rough_bin_width),
      random = runif(length(size_sampled))
    ) %>% 
    group_by(rough_size) %>% 
    top_n(n_samples_each, wt= random) %>% 
    select(-random) #here we are randomly selecting 5 individuals from each rough bin width. (note largest bins may have less than 5 individuals left)

  
  #add error to age-length samples
  
  if (add_error){
  ageing_data <- 
    ageing_data %>%
    mutate(
      age_bin = age_bin + rpois(length(age_bin), age_var),
      size_bin = size_bin + rnorm(length(size_bin), size_var)
    )
  }
  
  
  ageing_data %>% 
  ggplot(aes(size_bin)) + geom_histogram() + 
  geom_histogram(binwidth=1) + facet_grid(~age_bin)


###estimate G_av from subset

  g_mod <- lm(size_bin ~ age_bin, data = ageing_data)

  plot(size_bin ~ age_bin, data = ageing_data)


###apply new estimated G to sizes of all fish sampled


  simulated_sample2 <- data

  simulated_sample2$estimated_age <- (simulated_sample2$size_sampled - g_mod$coefficients[1])/g_mod$coefficients[2]
  
  simulated_sample2$g_rate <- g_mod$coefficients[2]

  simulated_sample2 %>% ggplot(aes(estimated_age)) + geom_histogram() # + xlim(0, 12)


#Remove negetive ages

  simulated_sample2 <- simulated_sample2 %>% filter(estimated_age >= 0)
  
  simulated_sample2
}


