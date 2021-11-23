# Getting pars and age data together

pars <- default_pars("model1")

data <- simulate_population(pars)

age_data <- data %>% summarise_bin_counts(bin_var = age,
                                          bin_width = 1)

standata_age <- age_data %>% compose_count_data()

# Getting size and growth together
size_data <- data %>% summarise_bin_counts(bin_var = size,
                                           bin_width = 0.1)

growth_data <- simulate_population(default_pars("model1")) %>%
  add_sampling_noise(size, sd = 0.5)

standata_size <- size_data %>%
  compose_count_data()

# Growth data
standata_growth <-
  growth_data %>%
  compose_growth_data(age_var = age,
                      size_var = size)

###################################

test_that("Output is correct format", {
  expect_visible(age_data %>% compose_count_data())
  expect_length(standata_age, 5)
  expect_named(standata_age)
  expect_type(standata_age, "list")

  expect_visible(size_data %>% compose_count_data())
  expect_length(standata_size, 5)
  expect_named(standata_size)
  expect_type(standata_size, "list")

  expect_visible(growth_data %>%
                   compose_growth_data(age_var = age,
                                       size_var = size))
  expect_length(standata_growth, 3)
  expect_named(standata_growth)
  expect_type(standata_growth, "list")

})

test_that("The right errors are tripped", {
  expect_error(data %>% compose_count_data())

})



