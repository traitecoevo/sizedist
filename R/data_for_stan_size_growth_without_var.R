#' Prepare data from a generated catch sample for use in 'mortality_size_growth.stan' model  
#'
#' This requires a simulated_sample dataframe from the simulate_catch_data function; source("R/generate_sample.R")
#'
#' @param data a dataframe with a simulated sample of larval fish, created using simulate_catch_data function
#'             and binned to smallest resolution using round_by_bin function.
#' @param bin_width_size The bin size used for size, eg 0.1 mm.
#' @param rough_bin_width rough bin with for age-length samples
#' @param n_samples_each number of indivuals to select from each rough size class
#' @return a list of data for use in a mortality-from-age model 'mortality_age.stan'




data_for_stan_size_growth_without_var <- function(data,
                                                  bin_width_size = 0.1,
                                                  rough_bin_width = 1.5,
                                                  n_samples_each = 3,
                                                  s0_av = 3,
                                                  trim_to_mode = FALSE,
                                                  add_to_cut = 2){


  #remove individuals smaller than modal size.
  if (trim_to_mode) { 
    data <- trim_data_to_mode(data, add_to_cut = add_to_cut)
    
  }
  
  
  #prepare data by grouping size bins and making counts
  data_prep_size <- 
    tibble(
      size_bin = seq(min(data$size_bin), max(data$size_bin), by = bin_width_size) %>% as.character()
    ) %>% 
    left_join(by="size_bin",
              data %>% group_by(size_bin) %>% summarise(counts = n()) %>% mutate(size_bin = size_bin  %>% as.character)
    ) %>% 
    mutate(size_bin = as.numeric(size_bin)) %>% 
    replace_na(list(counts=0)) %>%
    mutate(size_lower = size_bin - 0.5*bin_width_size, size_upper =size_bin + 0.5*bin_width_size)
  
  data_prep_size$size_lower[1] <- data_prep_size$size_bin[1]
  data_prep_size$size_lower[nrow(data_prep_size)] <- data_prep_size$size_bin[nrow(data_prep_size)]
  


  #rounding function
  round_by_bin <- function(x, bin_width) {  
  round(x/bin_width, 0)*bin_width
  }

  #simulate sampling of length-age across a few major sizes groups
  data_length_age_samples <- 
  data %>%  
  mutate(
    rough_size = round_by_bin(size_sampled, rough_bin_width),
    random = runif(length(size_sampled))
  ) %>% 
  group_by(rough_size) %>% 
  top_n(n_samples_each, wt= random) %>% 
  select(-random)

  #Prepare list for use in stan model
  data_for_stan_size_growth <- 
  list(
    # data for counts
    size_lower = data_prep_size$size_lower,
    size_upper = data_prep_size$size_upper,
    counts = data_prep_size$counts,
    N_counts = nrow(data_prep_size),
    l0 = s0_av,
    
    # data for length age estimates
    N_growth = nrow(data_length_age_samples),
    age = data_length_age_samples$age_bin,
    size_ind = data_length_age_samples$size_bin
  )

data_for_stan_size_growth
}