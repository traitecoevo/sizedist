#' Plot the size distribution
#'
#' @description This function is for plotting the size distribution of simulated populations.
#' @param data a dataframe with a simulated size, size_sampled
#' @param pars list of parameter values for the size-distribution model, including model name
#' @param binwidth The bin width for size, usually the smallest resolution of measurement e.g 0.1 mm
#' @param fitted is a list containing estimated parameters from a stan mortality_size model.
#' @importFrom ggplot2 ggplot aes geom_histogram stat_function
#' @return a ggplot of size-frequency histogram with fitted function line

plot_size_dist <- function(data, pars, binwidth, fitted = NULL) {
  p1 <- data %>%
    ggplot(aes(size)) +
    geom_histogram(binwidth = binwidth) +
    stat_function(fun = function(x) size_dist_model(x, pars) * binwidth)

  if (!is.null(fitted))
    p1 <- p1 + stat_function(fun = size_dist_model(x, fitted)*fitted$binwidth, col = "red")

  p1
}
