#Example dataset
bin_width = 0.1
example_1 <- Loblolly %>%
  add_bins(var = height, bin_width = bin_width)

test_that("Returned output is correct", {
  expect_visible(example_1)
  expect_named(example_1)
  expect_true(ncol(Loblolly) < ncol(example_1))
  expect_equal(ncol(example_1), stringr::str_which(names(example_1), "_bin"))
  expect_equal(example_1[1,"height_bin"],
               (round(example_1[1,"height"] / bin_width, 0) * bin_width),
               ignore_attr = TRUE)
})

