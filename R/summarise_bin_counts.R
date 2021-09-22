#' Summarise counts for each bin interval
#'
#' @param data A data frame or tibble
#' @param bin_var Numeric variable that bin intervals and counts are computed
#' @param bin_width The bin width for `age`bin_var`, usually the smallest resolution of re-sampling
#'
#' @return A tibble containing `bin_var`, lower and upper bounds and counts for each bin
#' @export
#'
#' @examples
#' Loblolly %>% summarise_bin_counts(height, bin_width = 0.5)
#'
summarise_bin_counts <- function(data, bin_var, bin_width) {

  #Add bin variable to data
  data <- data %>% add_bins({{bin_var}}, bin_width)

 bin_var <- rlang::enquo(bin_var)

 #Get counts
 bin_counts <- data %>%
   dplyr::group_by(binned_var) %>%
   dplyr::summarise(counts = n()) %>%
   dplyr::ungroup()

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
    binned_var = data %>% dplyr::pull(binned_var) %>% create_all_bins(bin_width),
    !!bin_nm := binned_var,
    !!lb_name := binned_var - 0.5 * bin_width,
    !!ub_name := binned_var + 0.5 * bin_width)

  #Get the bin counts and then joining to data
  out <- tmp %>% dplyr::left_join(by = "binned_var", bin_counts) %>%
    tidyr::replace_na(list(counts = 0)) %>%
    dplyr::select(-binned_var)

  #Correct the lower bounds of the first and last bins
  out[1,2] <- out[1,1]
  out[nrow(out),2] <-out[nrow(out), 1]

  out
}

#' Creates bins
#'
#' add_bins() adds size and/or age bins to the dataset.
#'
#' @param data A data frame, data frame extension (e.g. tibble) containing `age` and `size` variable as columns
#' @param var Name of variable, using tidy evaluation with data masking
#' @param bin_width The bin width for variable, usually the smallest resolution of resampling or measurement
#' @return a data frame with binned variable added with suffix `_bin`
#' @export
#' @importFrom rlang :=
#' @seealso simulate_population()
#' @examples
#' \dontrun{
#'
#' Loblolly %>% add_bins(var = height, bin_width = 0.5)
#' }
#'
add_bins <- function(data,
                     var,
                     bin_width) {

  round_by_bin <- function(x, bin_width) {
    round(x/bin_width, 0)*bin_width
  }

  if(missing(var)){
    stop("`var` is missing! Which variable do you want to add bins to?")
  }

  if(missing(bin_width)){
    stop("`bin_width` must be supplied! This is usually the smallest measurement resolution for `var`")
  }

  # Rounding data value to specific bin
  data %>%
    dplyr::tibble() %>% #Become tibble
    dplyr::mutate(
      binned_var = round_by_bin({{var}}, bin_width)
    )

}

