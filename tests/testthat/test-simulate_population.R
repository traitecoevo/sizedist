pars  <- default_pars("model1")

pars_cm_kg <- purrr::list_modify(pars,
                                 model = "model1b",
                                 pars = list(g_av = 0.5))


test_that("Output in correct format", {
  expect_visible(simulate_population())
  expect_length(simulate_population(), 10)
  expect_named(simulate_population())
  expect_type(simulate_population(), "list")
})


test_that("The right errors are tripped", {
  expect_error(standata_size %>% add_pars())
  expect_error(standata_size_mod2 %>% add_pars(pars_cm_kg))

})
