pars <- default_pars("model1")
data <- simulate_population(pars)
p <- plot_age_dist(data, pars, 1)
p2 <- plot_size_dist(data, pars, 0.1)

test_that("Plot returns ggplot object",{
  expect_visible(plot_age_dist(data, pars, 1))
  expect_s3_class(p, "ggplot")
  expect_silent(plot_age_dist(data, pars, 1))

  expect_visible(plot_size_dist(data, pars, 0.1))
  expect_s3_class(p2, "ggplot")
  expect_silent(plot_size_dist(data, pars, 0.1))
})

test_that("Plot uses correct data", {
  expect_equal(data, p$data)

  expect_equal(data, p2$data)
})

test_that("Labels are correct", {
  expect_named(p$labels)
  expect_identical(p$labels$x, "age")
  expect_identical(p$labels$y[1], "y")
  expect_equal(p$layers[[1]]$geom$required_aes[1], "x")
  expect_equal(p$layers[[1]]$geom$required_aes[2], "y")

  expect_named(p2$labels)
  expect_identical(p2$labels$x, "size")
  expect_identical(p2$labels$y[1], "y")
  expect_equal(p2$layers[[1]]$geom$required_aes[1], "x")
  expect_equal(p2$layers[[1]]$geom$required_aes[2], "y")
})

test_that("Plot layers match",{
  expect_identical(p$layers[[1]]$geom$default_aes$fill, "grey35")
  expect_identical(p$layers[[1]]$geom$default_aes$linetype, 1)

  expect_identical(p2$layers[[1]]$geom$default_aes$fill, "grey35")
  expect_identical(p2$layers[[1]]$geom$default_aes$linetype, 1)
})

