# The set up
pars <- default_pars("model1")
data <- simulate_population(pars)

# model1a
age_data <- data %>% summarise_bin_counts(bin_var = age,
                                          bin_width = 1)
standata_age <- age_data %>% compose_count_data()
age_pars <- pars %>% purrr::list_modify(model = "model1a")
age_pars <- age_pars %>% default_priors()
standata_age_mod1 <- standata_age %>% add_pars(pars = age_pars)

# model1b
size_data <- data %>% summarise_bin_counts(bin_var = size,
                                           bin_width = 0.1)

standata_size <- size_data %>% compose_count_data()
pars_cm_kg <- purrr::list_modify(pars,
                                 model = "model1b",
                                 pars = list(g_av = 0.5))
pars_cm_kg <- pars_cm_kg %>% default_priors()
standata_size_mod2 <- standata_size %>% add_pars(pars_cm_kg)

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
standata_size_growth_mod3 <- standata_size_growth_mod3 %>% add_pars(pars_m_g)

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

standata_size_growth_mod4 <- standata_size_growth_mod4 %>% add_pars(pars_m_g_s0)


test_that("Function runs", {
  # The fits
  fit1 <- suppressWarnings(fit_sizedist(standata_age_mod1, iter = 200))
  fit2 <- suppressWarnings(fit_sizedist(standata_size_mod2, iter = 200))
  fit3 <- suppressWarnings(fit_sizedist(standata_size_growth_mod3, iter = 200))
  fit4 <- suppressWarnings(fit_sizedist(standata_size_growth_mod4, iter = 200))

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
})

test_that("Throws errors when it needs to",{
  expect_error(fit_sizedist())
  expect_error(fit_sizedist(standata_size))
})


