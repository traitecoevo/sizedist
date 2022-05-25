## Code to prepare `fishlarvae` dataset goes here

pars <- default_pars("model1")
tow_data <- simulate_population(pars = pars) %>%
  add_bins() %>%
  create_age_bin_counts() %>%
  compose_bin_data()

usethis::use_data(tow_data, overwrite = TRUE, internal = TRUE)
