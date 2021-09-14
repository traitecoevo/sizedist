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

  # #Abort if not numeric
  # if(! is.numeric({{var}})){
  #   stop("Variable must be numeric")
  # }

  # Rounding data value to specific bin
  tmp <- data %>%
    dplyr::mutate(
     binned_var = round_by_bin({{var}}, bin_width)
    )

  # Suffix to append to end of variable name
  suffix <-"_bin"

  # Renaming the variable using supplied variable name and suffix
  out <- tmp %>%
    dplyr::rename("{{var}}{suffix}" := binned_var)

  #Return as tibble
  dplyr::tibble(out)

}
