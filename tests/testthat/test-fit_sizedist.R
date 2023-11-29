# The set up
pars <- default_pars("model1")
data <- simulate_population(pars)

# model1a
age_data <- data %>% summarise_bin_counts(bin_var = age,
                                          bin_width = 1)
standata_age <- age_data %>% compose_count_data()
age_pars <- pars %>% purrr::list_modify(model = "model1a")
age_pars <- age_pars %>% default_priors()

standata_model1a <- standata_age %>% add_pars(pars = age_pars)

# model1b
size_data <- data %>% summarise_bin_counts(bin_var = size,
                                           bin_width = 0.1)

standata_size <- size_data %>% compose_count_data()
pars_cm_kg <- purrr::list_modify(pars,
                                 model = "model1b",
                                 pars = list(g_av = 0.5))
pars_cm_kg <- pars_cm_kg %>% default_priors()
standata_model1b <- standata_size %>% add_pars(pars_cm_kg)

# model1c
growth_data <- simulate_population(pars) %>%
  add_sampling_noise(size, sd = 0.5)

standata_size <- size_data %>%
  compose_count_data()

standata_growth <-
  growth_data %>%
  compose_growth_data(age_var = age,
                      size_var = size)
standata_size_growth_mod3 <- join_stan_data(standata_size, standata_growth)

pars_m_g <- purrr::list_modify(pars,
                               model = "model1c")
pars_m_g <- pars_m_g %>% default_priors()
standata_model1c <- standata_size_growth_mod3 %>% add_pars(pars_m_g)

# model1d
growth_data <- simulate_population(pars) %>%
  add_sampling_noise(size, sd = 0.5)

standata_size <- size_data %>%
  compose_count_data()

standata_growth <-
  growth_data %>%
  compose_growth_data(age_var = age,
                      size_var = size)

standata_size_growth_mod4 <- join_stan_data(standata_size, standata_growth)

pars  <- default_pars("model1")

pars_m_g_s0 <- purrr::list_modify(pars,
                                  model = "model1d")

pars_m_g_s0 <- pars_m_g_s0 %>% default_priors()

standata_model1d <- standata_size_growth_mod4 %>% add_pars(pars_m_g_s0)

#model1e

size_data <- data %>% summarise_bin_counts(bin_var = size,
                                           bin_width = 0.1)

standata_size <- size_data %>% compose_count_data()

pars  <- default_pars("model1")

pars_abund <- purrr::list_modify(pars,
                                 model = "model1e")

pars_abund <- pars_abund %>% default_priors()


standata_model1e <- standata_size %>% add_pars(pars_abund)

#model2a

pars <- default_pars("model2")

data <- simulate_population(pars)


size_data <- data %>% summarise_bin_counts(bin_var = size,
                                           bin_width = 0.01)
standata_size <- size_data %>% compose_count_data()

pars  <- default_pars("model2")

pars_abund <- purrr::list_modify(pars,
                                 model = "model2a")


pars_abund <- pars_abund %>% default_priors()


standata_model2a <- standata_size %>% add_pars(pars_abund)

#model2b

standata_size <- size_data %>%
  compose_count_data()

growth_data <- simulate_population(default_pars("model2")) %>%
  add_sampling_noise(size, sd = 0.01) %>%
  add_sampling_noise(age, sd = 3)

standata_growth <-
  growth_data %>%
  compose_growth_data(age_var = age,
                      size_var = size)


pars  <- default_pars("model2")

pars_model2b <- purrr::list_modify(pars,
                                   model = "model2b")

pars_model2b <- pars_model2b %>% default_priors()


standata_model2b <- join_stan_data(standata_size, standata_growth)


standata_model2b <- standata_model2b %>% add_pars(pars_model2b)

#tests

test_that("Function runs", {
  # The fits
  fit1 <- suppressWarnings(fit_sizedist(standata_model1a, iter = 200))
  fit2 <- suppressWarnings(fit_sizedist(standata_model1b, iter = 200))
  fit3 <- suppressWarnings(fit_sizedist(standata_model1c, iter = 200))
  fit4 <- suppressWarnings(fit_sizedist(standata_model1d, iter = 200))
  fit5 <- suppressWarnings(fit_sizedist(standata_model1e, iter = 200))
  fit6 <- suppressWarnings(fit_sizedist(standata_model2a, iter = 200))
  fit7 <- suppressWarnings(fit_sizedist(standata_model2b, iter = 200))

  expect_visible(fit1)
  expect_named(fit1)
  expect_s4_class(fit1, "stanfit")

  expect_visible(fit2)
  expect_named(fit2)
  expect_s4_class(fit2, "stanfit")

  expect_visible(fit3)
  expect_named(fit3)
  expect_s4_class(fit3, "stanfit")

  expect_visible(fit4)
  expect_named(fit4)
  expect_s4_class(fit4, "stanfit")

  expect_visible(fit5)
  expect_named(fit5)
  expect_s4_class(fit5, "stanfit")

  expect_visible(fit6)
  expect_named(fit6)
  expect_s4_class(fit6, "stanfit")

  expect_visible(fit7)
  expect_named(fit7)
  expect_s4_class(fit7, "stanfit")
})

test_that("Throws errors when it needs to",{
  expect_error(fit_sizedist())
  expect_error(fit_sizedist(standata_size))
})


