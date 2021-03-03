#' plot_age_dist
#'
#'This function is for plotting the age distribution of siminulated fish populations. 
#' @param data a dataframe with a simulated sample of larval fish, created using simulate_catch_data function
#'             and binned to smallest resolution using round_by_bin function.
#' @param pars is a list containing the parameters used to generate simulated population eg; R, s0_av , g_av, z_av = 0.25, 
#'             bin_width_age, bin_width_size.
#' @param fitted is a list containg estimated paramets from mortality_age.stan model
#'
#' @return a ggplot of age-frequency histogram with fitted function line

plot_age_dist <- function(data, pars, fitted = NULL) {
  p1 <- data %>% 
    ggplot(aes(age)) + 
    geom_histogram(binwidth = pars$bin_width_age) +
    stat_function(fun = function(x) pars$R * exp(-pars$z_av*x)*pars$bin_width_age)
  
  if (!is.null(fitted))
    
    p1 <- p1 + stat_function(fun = function(x) fitted$R * exp(-fitted$z_av*x)*fitted$bin_width_age, col = "red")
  
  p1
}