#' Creates bins
#'
#' @param data A data frame, data frame extension (e.g. tibble) containing `age` as a column
#' @param bin_width_age The bin width for age, usually the smallest resolution of re-ssampling e.g. 1 day
#' @return a data frame containing lower and upper bounds as well as counts for each `age_bin`
#' @export
#' @seealso `simulate_population()` `add_bins()`
#' @example
#' \dontrun{
#' fishlarvae %>% add_bins() %>% create_age_bin_counts()
#' }
#'
create_age_bin_counts <- function(data, bin_width_age = 1){
  #Is there a way to determine bin_width_age from data?
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
  tmp$age_lower[nrow(tmp)] <- tmp$age_bin[nrow(tmp)] #This I don't understand why...

  #My version
  # ret <- tmp %>% mutate(
  #   age_lower = replace(age_lower, age_lower < 0, 0),
  #   age_lower = replace(age_lower, row_number() == n(), last(age_bin)))

  tmp
}

