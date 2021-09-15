#' Counts for age bin data
#'
#' @param data A data frame, data frame extension (e.g. tibble) containing `age` as a column
#' @param bin_width_age The bin width for age, usually the smallest resolution of re-ssampling e.g. 1 day
#' @return a data frame containing lower and upper bounds as well as counts for each `age_bin`
#' @export
#' @seealso `simulate_population()` `add_bins()`
#' @importFrom dplyr tibble left_join group_by summarise n
#' @examples
#' \dontrun{
#' fishlarvae %>% add_bins() %>% create_age_bin_counts()
#' }
#'
create_age_bin_counts <- function(data, bin_width_age = 1){

  tmp <-
    tibble(
      age_bin = seq(min(data$age_bin), max(data$age_bin), by = bin_width_age),
      age_lower = age_bin - 0.5*bin_width_age,
      age_upper = age_bin + 0.5*bin_width_age
    ) %>%
    left_join(by="age_bin",
              data %>% group_by(age_bin) %>% summarise(counts= n())
    ) %>%
    tidyr::replace_na(list(counts=0))

  tmp$age_lower[1] <- tmp$age_bin[1]
  tmp$age_lower[nrow(tmp)] <- tmp$age_bin[nrow(tmp)]

  #My version
  # ret <- tmp %>% mutate(
  #   age_lower = replace(age_lower, age_lower < 0, 0),
  #   age_lower = replace(age_lower, row_number() == n(), last(age_bin)))

  tmp
}

#' Counts for size bin data
#'
#' @param data A data frame, data frame extension (e.g. tibble) containing `age` as a column
#' @param bin_width_age The bin width for age, usually the smallest resolution of re-sampling e.g. 1 day
#' @return a data frame containing lower and upper bounds as well as counts for each `size_bin`
#' @export
#' @seealso `simulate_population()` `add_bins()`

create_size_bin_counts <- function(data,
                                   bin_width_size = 0.1){
  tmp <-
    tibble(
      size_bin = seq(min(data$size_bin), max(data$size_bin), by = bin_width_size),
      size_lower = size_bin - 0.5*bin_width_size,
      size_upper = size_bin + 0.5*bin_width_size
    ) %>%
    left_join(by="size_bin",
              data %>% group_by(size_bin) %>% summarise(counts= n())
    ) %>%
    tidyr::replace_na(list(counts=0))

  tmp$size_lower[1] <- tmp$size_bin[1]
  tmp$size_lower[nrow(tmp)] <- tmp$size_bin[nrow(tmp)]

  #My version
  # ret <- tmp %>% mutate(
  #   size_lower = replace(size_lower, size_lower < 0, 0), #when size_lower is < min(size_bin), first(size_bin)
  #   size_lower = replace(size_lower, row_number() == n(), last(size_bin)),
  #   known_g = known_g)

  tmp
}
