#' Compile age-size data into Stan friendly format for growth models
#'
#' @param data A data frame containing age and/or size bins counts created using `summarise_bin_counts`
#' @param age_var Name of age variable
#' @param size_var Name of size variable
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' data <- simulate_population(pars = pars)
#' compose_growth_data(data, age, size)
#' }

compose_growth_data <- function(data,
                                age_var,
                                size_var){
  #Filter out the relevant cols
  tmp <- data %>% dplyr::select({{age_var}}, {{size_var}})

  #Convert df to list
  out <- tidybayes::compose_data(tmp)

  #rename list name for $n
  #function from here: https://github.com/tidyverse/purrr/issues/804
  ls_rename <- function(.x, ..., .strict = TRUE) {
    pos <- tidyselect::eval_rename(quote(c(...)), .x, strict = .strict)
    names(.x)[pos] <- names(pos)
    .x
  }

  ls_rename(out, N_growth = n)

}



