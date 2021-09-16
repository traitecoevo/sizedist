

summarise_bin_counts <- function(data, bin_var, bin_width) {

  #Add bin variable to data
  data <- data %>% add_bins({{bin_var}}, bin_width)

 bin_var <- enquo(bin_var)

 #Get counts
 bin_counts <- data %>%
   group_by(binned_var) %>%
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
    binned_var = data %>% pull(binned_var) %>% create_all_bins(bin_width),
    !!bin_nm := binned_var,
    !!lb_name := binned_var - 0.5 * bin_width,
    !!ub_name := binned_var + 0.5 * bin_width)

  #Get the bin counts and then joining to data
  tmp %>% left_join(by = "binned_var", bin_counts) %>%
    tidyr::replace_na(list(counts = 0)) %>%
    dplyr::select(-binned_var)

}

# #data <- simulate_population() %>% add_bins(size_sampled, 0.1)
data %>% summarise_bin_counts(size, 0.1)
summarise_bin_counts(Loblolly, age, 10)

data$size %>% unique()

