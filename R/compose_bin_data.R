#' Compile data frame into Stan friendly format for size distribution models
#'
#' @param data A data frame containing age and/or size bins counts created using `add_bins()` and `create_age_bin_counts()`
#' @param ... Arguments passed to tidybayes::compose_data
#' @export
#' @seealso `fit_age()` and `create_age_bin_counts()`
#' @return A data list where each list element is a column from the original data frame

compose_bin_data <- function(data, ...){
  tidybayes::compose_data(data, ...)
}
