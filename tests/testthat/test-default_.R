
test_that("Output in correct format", {
  expect_visible(default_pars("model1"))
  expect_length(default_pars("model1"), 2)
  expect_named(default_pars("model1"))
  expect_type(default_pars("model1"), "list")
})


test_that("The right errors are tripped", {
  expect_error(default_pars())
  expect_error(default_pars(5))
  expect_error(default_pars("apple"))
})

