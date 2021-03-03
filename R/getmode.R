#' getmode
#' 
#' This function is for calculating the mode of a vector
#'
#' @param v is a vector
#'
#' @return the mode of a vector

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
