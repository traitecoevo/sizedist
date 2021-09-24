#' Plot the age distribution
#'
#' @description This function is for plotting the age distribution of simulated populations.
#' @param data a data frame with a simulated sample of larval fish, created using simulate_catch_data function
#' and binned to smallest resolution using round_by_bin function.
#' @param pars list of parameter values for the size-distribution model, including model name
#' @param binwidth The bin width for age, usually the smallest resolution of resampling e.g. 1 day
#' @param fitted is a list containing estimated parameters from mortality_age.stan model
#' @importFrom ggplot2 ggplot aes geom_histogram stat_function
#' @return a ggplot of age-frequency histogram with fitted function line
#' @export

plot_age_dist <- function(data, pars, binwidth, fitted = NULL) {
  p1 <- data %>%
    ggplot(aes(age)) +
    geom_histogram(binwidth = binwidth) +
    stat_function(fun = function(x) age_dist_model(x, pars)*binwidth)

  if (!is.null(fitted))
    p1 <- p1 + stat_function(fun = function(x) age_dist_model(x, fitted)* fitted$binwidth, col = "red")

  p1
}
