# Functions in summarise_bin_counts
round_by_bin <- function(x, bin_width) {
  round(x/bin_width, 0)*bin_width
}

create_all_bins <- function(vec, bin_width){
  ret <- seq(min(vec), max(vec), by = bin_width)
  ret
}

### Data and bin width set up
set.seed(19)
data <- simulate_population(default_pars(model = 1))

binwidth = 1

test_data_1 <- tibble(id = 1:10,
                      var = c(1.25, 2.82, 2.19, 3.42, 4.54,
                              5, 5.90, 6.25, 6.86, 8.79),
                      binned_var = round_by_bin(var, binwidth))

td1 <- test_data_1 %>%
  select(-binned_var) %>%
  add_bins(var, binwidth)

# Using simulated data
td2 <- data %>% add_bins(age, binwidth)

td2_data <- data %>%
  select(age, size) %>%
  mutate(binned_var = round_by_bin(age, binwidth))

test_data_3 <- data %>% sample_frac(0.10)
test_data_3_wbins <- test_data_3 %>%
  mutate(binned_var = round_by_bin(age, binwidth))

# Solutions "Actual" - using summarise bin counts
td1_summarised <- test_data_1 %>% summarise_bin_counts(var, binwidth)
td2_summarised <- data %>% summarise_bin_counts(age, binwidth)
td3_summarised <- test_data_3 %>% summarise_bin_counts(age, binwidth)

# A solid test for the ones
test_that("The number of counts is what is expected",{
  expect_equal(test_data_1 %>% summarise_bin_counts(var, binwidth) %>% filter(! counts == 0) %>% pull(counts), janitor::tabyl(test_data_1, binned_var)$n)
  expect_equal(td2 %>% summarise_bin_counts(age, binwidth) %>% filter(! counts == 0) %>% pull(counts), janitor::tabyl(td2, binned_var)$n)
})

# What about the zeros?
test_that("The number of zeros is what is expected", {
  expect_equal(td1_summarised[td1_summarised$counts == 0, "binned_var"] %>% pull(binned_var), setdiff(seq(min(test_data_1$binned_var), max(test_data_1$binned_var), by = binwidth), test_data_1$binned_var))  #which bins have zero counts
  expect_equal(nrow(td1_summarised[td1_summarised$counts == 0, "binned_var"]), length(setdiff(seq(min(test_data_1$binned_var), max(test_data_1$binned_var)), test_data_1$binned_var))) #number of zeros
  expect_equal(td2_summarised[td2_summarised$counts == 0, "binned_var"] %>% pull(binned_var), setdiff(seq(min(td2$binned_var), max(td2$binned_var), by = binwidth), td2$binned_var))
  expect_true(janitor::tabyl(td2_summarised, counts)[janitor::tabyl(td2_summarised, counts)$counts == 0, "n"] == length(setdiff(seq(min(td2$binned_var), max(td2$binned_var)), td2$binned_var)))
  })

test_that("td2 and td2_data is the same", {
  expect_equal(test_data_1$binned_var, td1$binned_var)
  expect_equal(td2$binned_var, td2_data$binned_var)
})


# This is what we want to test
test_data_3 %>% summarise_bin_counts(age, age_bin_width)

#Test adding bins
test_that("Add_bins and round_by_bin is doing the same thing", {
  expect_equal(test_data_3 %>% add_bins(age, age_bin_width) %>% pull(binned_var), round_by_bin(test_data_3$age, age_bin_width))
  #(test_data_3 %>% add_bins(age, binwidth) %>% pull(binned_var)) %in% td3_summarised$binned_var
})

test_that("Counts are matching to doing by hand",{
  #Computing bin counts by hand
  td3_bin_counts <- test_data_3_wbins %>%
    dplyr::group_by(binned_var) %>%
    dplyr::summarise(counts = dplyr::n()) %>%
    dplyr::ungroup()

  expect_true(sum(td3_bin_counts$counts) == nrow(test_data_3_wbins))
  expect_true(test_data_3_wbins %>% select(age, binned_var) %>% filter(binned_var < binwidth) %>% nrow() == td3_bin_counts %>% filter(binned_var == 0) %>% pull(counts))
})

test_that("Bins are created correctly", {
  td3_allbins <- test_data_3_wbins %>% dplyr::pull(binned_var) %>% create_all_bins(age_bin_width)
  expect_true(td3_allbins[1] ==  min(test_data_3_wbins))
  expect_true(td3_allbins[length(td3_allbins)] ==  max(test_data_3_wbins))
})

td3_tmp <- dplyr::tibble(
  binned_var = test_data_3_wbins %>% dplyr::pull(binned_var) %>% create_all_bins(age_bin_width),
  bin_lower = binned_var - 0.5 * age_bin_width,
  bin_upper = binned_var + 0.5 * age_bin_width)

#Fixing half bounds for first and last bin of lower bound
td3_tmp[1,2] <- td3_tmp[1,1]
td3_tmp[nrow(td3_tmp),2] <- td3_tmp[nrow(td3_tmp), 1]

test_that("Bin bounds are correct", {
  #Creating all bins and bounds
  expect_true(td3_tmp[1,2] == td3_tmp[1,1])
  expect_true(td3_tmp[nrow(td3_tmp),2] == td3_tmp[nrow(td3_tmp), 1])
})

test_that("Left joining counts to bins are correct",{
  td3_merge_byhand <- td3_tmp %>% dplyr::left_join(by = "binned_var", td3_bin_counts) %>%
    tidyr::replace_na(list(counts = 0))

  expect_equal(td3_merge_byhand$counts, test_data_3 %>% summarise_bin_counts(age, binwidth) %>% pull(counts))
})


