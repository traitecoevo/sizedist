# Getting data together

pars  <- default_pars("model1")

data <- simulate_population(pars)

size_data <- data %>% summarise_bin_counts(bin_var = size,
                                           bin_width = 0.1)
# Compose data
standata_size <- size_data %>% compose_count_data()

pars_cm_kg <- purrr::list_modify(pars,
                                 model = "model1b",
                                 pars = list(g_av = 0.5))


pars_cm_kg <- pars_cm_kg %>% default_priors()

standata_size_mod2 <- standata_size %>% add_pars(pars_cm_kg)

str(standata_size_mod2)

test_that("Output in correct format", {
  expect_visible(standata_size %>% add_pars(pars_cm_kg))
  expect_length(standata_size_mod2, 8)
  expect_named(standata_size_mod2)
  expect_type(standata_size_mod2, "list")
})


test_that("The right errors are tripped", {
  expect_error(standata_size %>% add_pars())
  expect_error(standata_size_mod2 %>% add_pars(pars_cm_kg))

})


