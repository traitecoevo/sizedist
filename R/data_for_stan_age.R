#' Prepare data from a generated catch sample for use in 'mortality_age.stan' model  
#'
#' This requires a dataframe from the simulate_catch_data function; source("R/generate_sample.R")
#' 
#' @param data a dataframe with a simulated sample of larval fish, created using simulate_catch_data function
#'             and binned to smallest resolution using round_by_bin function.
#' @param bin_width_age The bin size used for age, eg 1 day.
#'
#' @return a list of data for use in a mortality-from-age model 'mortality_age.stan'


data_for_stan_age <- function(data,
                              bin_width_age = 1){

#prepare data by grouping age bins and making counts
data_prep_age <- 
  tibble(
    age_bin = seq(min(data$age_bin), max(data$age_bin), by = bin_width_age),
    age_lower = age_bin - 0.5*bin_width_age, 
    age_upper = age_bin + 0.5*bin_width_age
  ) %>% 
  left_join(by="age_bin",
            data %>% group_by(age_bin) %>% summarise(counts= n())
  ) %>% 
  replace_na(list(counts=0)) 

data_prep_age$age_lower[1] <- data_prep_age$age_bin[1]
data_prep_age$age_lower[nrow(data_prep_age)] <- data_prep_age$age_bin[nrow(data_prep_age)]

#make data into list format for use in stan
data_for_stan_age <- 
  list(
    age_lower = data_prep_age$age_lower,
    age_upper = data_prep_age$age_upper,
    counts = data_prep_age$counts,
    N = length(data_prep_age$age_lower)
  )

data_for_stan_age

}