#' Creates bins
#'
#' add_bins() adds size and/or age bins to the dataset.
#'
#' @param data A data frame, data frame extension (e.g. tibble) containing `age` and `size` variable as columns
#' @param age_var Name of age variable, using tidy evaluation with data masking
#' @param size_var Name of size variable, using tidy evaluation with data masking
#' @param bin_width_age The bin width for age, usually the smallest resolution of resampling e.g. 1 day
#' @param bin_width_size The bin width for size, usually the smallest resolution of measurement e.g 0.1 mm
#' @return a data frame with `age_bin` and `size_bin` variables added
#' @export
#' @seealso simulate_population()
#' @examples
#' \dontrun{
#' fishlarvae %>% add_bins()
#'
#' data %>% add_bins(age_var = days_since_birth, size_var = length)
#' }
#'
add_bins <- function(data,
                     age_var = age,
                     size_var = size_sampled,
                     bin_width_age = 1,
                     bin_width_size = 0.1) {

  round_by_bin <- function(x, bin_width) {
    round(x/bin_width, 0)*bin_width
  }

  out <- data %>%
    dplyr::mutate(
      age_bin = round_by_bin({{age_var}}, bin_width_age),
      size_bin = round_by_bin({{size_var}}, bin_width_size)
    )

  out

}
