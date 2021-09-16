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
