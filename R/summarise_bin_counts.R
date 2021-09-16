

summarise_bin_counts <- function(data, bin_var, bin_width) {

 bin_var <- enquo(bin_var)

 data <- data %>% mutate(x4235_corn_bob = !!bin_var)


 #Get counts
 bin_counts <- data %>%
   group_by(x4235_corn_bob) %>%
   summarise(counts = n()) %>%
   ungroup()

  #Create all possible bins
  create_all_bins <- function(vec, bin_width){
    ret <- seq(min(vec), max(vec), by = bin_width)
    ret
  }

  #Create variable name
  bin_nm <- rlang::as_label(bin_var)

  #Append suffixes for lower and upper bounds
  lb_name  <- glue::glue(bin_nm, "_lower")
  ub_name  <- glue::glue(bin_nm, "_upper")

  #Creating new df with all bins
  tmp <- dplyr::tibble(
    x4235_corn_bob = data %>% pull(x4235_corn_bob) %>% create_all_bins(bin_width),
    !!bin_nm := x4235_corn_bob,
    !!lb_name := x4235_corn_bob - 0.5 * bin_width,
    !!ub_name := x4235_corn_bob + 0.5 * bin_width)

  #Get the bin counts and then joining to data
  tmp %>% left_join(by = "x4235_corn_bob", bin_counts) %>%
    tidyr::replace_na(list(counts = 0)) %>%
    dplyr::select(-x4235_corn_bob)

}

# #data <- simulate_population() %>% add_bins(size_sampled, 0.1)
data %>% add_bins(size, 0.1) %>%
  summarise_bin_counts(size_bin, 0.1) #Add_bins into summarise_by_bin(), count_by_bin()
summarise_bin_counts(Loblolly, age, 10)

data$size %>% unique()

