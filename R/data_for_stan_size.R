#' Prepare data from a generated catch sample for use in 'mortality_age.stan' model  
#'
#' This requires a simulated_sample dataframe from the simulate_catch_data function; source("R/generate_sample.R")
#'
#' @param data a dataframe with a simulated sample of larval fish, created using simulate_catch_data function
#'             and binned to smallest resolution using round_by_bin function.
#' @param lit_G is a specified known growth rate taken from literature
#' @param bin_width_size The bin size used for age, eg 1 day.
#' @param trim_to_mode removes all individuals smaller than the modal size from data
#' @return a list of data for use in a mortality-from-age model 'mortality_age.stan'


data_for_stan_size <- function(data, 
                              lit_G = 0.21,
                              bin_width_size = 0.1,
                              trim_to_mode = FALSE,
                              s0_av = 3,
                              add_to_cut = 2
                              ) {
  
  
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

#make data into list format for use in stan
  data_for_stan_size <- 
  list(
    size_lower = data_prep_size$size_lower,
    size_upper = data_prep_size$size_upper,
    counts = data_prep_size$counts,
    N = length(data_prep_size$size_lower),
    l0 = s0_av,
    lit_G = lit_G
  )

data_for_stan_size

}