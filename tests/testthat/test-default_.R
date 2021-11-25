# Getting pars and priors together

new_pars <-
  default_pars("model1") %>%
  purrr::list_modify(model = "model1c") %>%
  default_priors()

######################################

test_that("Output in correct format", {
  expect_visible(default_pars("model1"))
  expect_length(default_pars("model1"), 2)
  expect_named(default_pars("model1"))
  expect_type(default_pars("model1"), "list")

  expect_visible(new_pars)
  expect_length(new_pars, 3)
  expect_named(new_pars)
  expect_type(new_pars, "list")
})


test_that("The right errors are tripped", {
  expect_error(default_pars())
  expect_error(default_pars(5))
  expect_error(default_pars("apple"))
})

