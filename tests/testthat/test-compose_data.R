pars <- default_pars("model1")

data <- simulate_population(pars)

age_data <- data %>% summarise_bin_counts(bin_var = age,
                                          bin_width = 1)

standata_age <- age_data %>% compose_count_data()

test_that("Output is correct format", {
  expect_visible(age_data %>% compose_count_data())
  expect_length(standata_age, 5)
  expect_named(standata_age)
  expect_type(standata_age, "list")
})

test_that("The right errors are tripped", {
  expect_error(data %>% compose_count_data())

})


