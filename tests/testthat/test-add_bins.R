library(stringr)

#Example dataset
example_1 <- Loblolly %>% add_bins(var = height, bin_width = 0.1)

test_that("Output is correct", {
  expect_true(ncol(Loblolly) < ncol(example_1))
  expect_equal(ncol(example_1), str_which(names(example_1), "_bin"))
})
