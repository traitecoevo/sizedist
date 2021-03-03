
#' trim_data_to_mode
#' 
#' This function is used to remove all fish smaller than the modal size class,
#' which may be required to estimate mortality for simulated population that have variable size at
#' birth, or simulated catch that has net extrusion of small individuals 
#'
#' @param data a dataframe with a simulated sample of larval fish, created using simulate_catch_data function
#'             and binned to smallest resolution using round_by_bin function.
#'
#' @return a dataframe that has been subset to remove all fish smaller than the model size
#' @export
#'
#' @examples
trim_data_to_mode <- function(data, 
                              add_to_cut = 0){ 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Create the vector with numbers.
v <- c(data$size_bin)
# Calculate the mode using the user function.
modal_size <- getmode(v)

data <- subset(data, data$size_bin >= (modal_size + add_to_cut))

data

}