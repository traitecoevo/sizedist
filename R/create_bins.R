#' create_bins
#'
#' This function is used to creat bins requires for the size distrbution models 
#'
#' @param data a dataframe containg a fish population generated using simulate_catch_data()
#' @param bin_width_age the bin width you want to use for age, usually the smallest resolution of measurement eg 1 day
#' @param bin_width_size the bin width you want to use for size, usually the smallest resolution of measurement eg 0.1 mm
#'
#' @return a dataframe age_bin and size_bin bariables added
create_bins <- function(data,
                        bin_width_age = 1,
                        bin_width_size = 0.1) {

round_by_bin <- function(x, bin_width) {  
  round(x/bin_width, 0)*bin_width
}

data_binned <- data %>%  
  mutate(
    age_bin = round_by_bin(age, bin_width_age),
    size_bin = round_by_bin(size_sampled, bin_width_size)
  )

data_binned

}