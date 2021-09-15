

summarise_bin_counts <- function(data, bin_var, bin_width) {

 bin_var <- enquo(bin_var)

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
    !!bin_var := data %>% pull(!!bin_var) %>% create_all_bins(bin_width),
    !!lb_name := !!bin_var - 0.5 * bin_width,
    !!ub_name := !!bin_var + 0.5 * bin_width)

  #Get the bin counts and then joining to data
  # tmp %>% left_join(by = setNames(!!bin_var, nm = bin_var),
  #                   data %>% group_by(!!bin_var) %>%
  #                     summarise(counts = n())) %>%
  #                     tidyr::replace_na(list(counts = 0))

  tmp
}



# #data <- simulate_population() %>% add_bins(size_sampled, 0.1)
# summarise_bin_counts(data = data, size_sampled_bin, 0.1)
# summarise_bin_counts(Loblolly, age, 0.1)

