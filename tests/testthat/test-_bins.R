test_that("Returned output is correct", {
  #Example dataset
  bin_width = 0.1
  example_1 <- Loblolly %>%
    summarise_bin_counts(height, bin_width)

  expect_visible(example_1)
  expect_named(example_1)
  expect_equal(ncol(example_1), 4)
  expect_equal(example_1[,2], example_1[,stringr::str_which(names(example_1), "_lower")])
  expect_equal(example_1[1,"height"],
               (round(example_1[1,"height"] / bin_width, 0) * bin_width),
               ignore_attr = TRUE)
})

test_that("Error handling", {
  Loblolly$char <- "a"
  bin_width = 0.1

  expect_error( Loblolly %>% summarise_bin_counts(char, bin_width) )
  expect_error( Loblolly %>% summarise_bin_counts(bin_width = bin_width) )
  expect_error( Loblolly %>% summarise_bin_counts(height) )
})
