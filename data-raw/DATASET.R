## Code to prepare `fishlarvae` dataset goes here

source("R/simulate_population.R")
source("R/model1.R")

#' create_bins
#'
#' This function is used to creat bins requires for the size distrbution models
#'
#' @param data a dataframe containg a fish population generated using simulate_catch_data()
#' @param bin_width_age the bin width you want to use for age, usually the smallest resolution of measurement eg 1 day
#' @param bin_width_size the bin width you want to use for size, usually the smallest resolution of measurement eg 0.1 mm
#'
#' @return a dataframe age_bin and size_bin bariables added
add_bins <- function(data,
                        bin_width_age = 1,
                        bin_width_size = 0.1) {

  round_by_bin <- function(x, bin_width) {
    round(x/bin_width, 0)*bin_width
  }

  data_binned <- data %>%
    mutate(
      age_bin = round_by_bin(age, bin_width_age),
      size_bin = round_by_bin(size_sampled, bin_width_size)
    )

  data_binned

}

#' Prepare data from a generated catch sample for use in 'mortality_age.stan' model
#'
#' This requires a dataframe from the simulate_catch_data function; source("R/generate_sample.R")
#'
#' @param data a dataframe with a simulated sample of larval fish, created using simulate_catch_data function
#'             and binned to smallest resolution using round_by_bin function.
#' @param bin_width_age The bin size used for age, eg 1 day.
#'
#' @return a list of data for use in a mortality-from-age model 'mortality_age.stan'


compose_bin_data <- function(data,
                              bin_width_age = 1){

  #prepare data by grouping age bins and making counts
  tmp <-
    tibble(
      age_bin = seq(min(data$age_bin), max(data$age_bin), by = bin_width_age),
      age_lower = age_bin - 0.5*bin_width_age,
      age_upper = age_bin + 0.5*bin_width_age
    ) %>%
    left_join(by="age_bin",
              data %>% group_by(age_bin) %>% summarise(counts= n())
    ) %>%
    tidyr::replace_na(list(counts=0))

  tmp$age_lower[1] <- tmp$age_bin[1]
  tmp$age_lower[nrow(tmp)] <- tmp$age_bin[nrow(tmp)]

  #make data into list format for use in Stan
  ret <-
    list(
      age_lower = tmp$age_lower,
      age_upper = tmp$age_upper,
      counts = tmp$counts,
      N = length(tmp$age_lower)
    )

  ret

}

pars <- default_pars("model1")
fishlarvae <- simulate_population(pars = pars)
fishlarvae <- add_bins(fishlarvae)
stan_fishlarvae <- compose_bin_data(fishlarvae)

usethis::use_data(fishlarvae, overwrite = TRUE)
usethis::use_data(stan_fishlarvae, overwrite = TRUE)
