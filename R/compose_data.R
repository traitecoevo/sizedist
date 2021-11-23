#' Compile data frame into Stan friendly format for size distribution models
#'
#' @param data A data frame containing age and/or size bins counts created using `add_bins()` and `create_age_bin_counts()`
#' @param ... Arguments passed to tidybayes::compose_data
#' @export
#' @importFrom rlang .data
#' @seealso `fit_sizedist()` and `create_age_bin_counts()`
#' @return A data list where each list element is a column from the original data frame

compose_count_data <- function(data,...){

  suppressWarnings(
    if(! names(data) %in% c("binned_var", "counts")){
      rlang::abort("Data must be binned first! - see ?summarise_bin_counts()")
    }
  )

  ret <- tidybayes::compose_data(data, ...)

  #rename list name for $n
  #function from here: https://github.com/tidyverse/purrr/issues/804
  ls_rename <- function(.x, ..., .strict = TRUE) {
    pos <- tidyselect::eval_rename(quote(c(...)), .x, strict = .strict)
    names(.x)[pos] <- names(pos)
    .x
  }

  out <- ret %>% ls_rename(N_counts = .data$n)

  if("size" %in% names(ret)){
    out %>% ls_rename(size_sampled = .data$size)
  }

  out
}


#' Compile age-size data into Stan friendly format for growth models
#'
#' @param data A data frame containing age and/or size bins counts created using `summarise_bin_counts`
#' @param age_var Name of age variable
#' @param size_var Name of size variable
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

  out %>% ls_rename(N_growth = .data$n,
                    size_ind = {{size_var}})
}



