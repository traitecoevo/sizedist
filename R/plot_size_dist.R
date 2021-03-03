#' plot_size_dist
#'
#'This function is for plotting the age distribution of siminulated fish populations.
#' @param data a dataframe with a simulated sample of larval fish, created using simulate_catch_data function
#'             and binned to smallest resolution using round_by_bin function.
#' @param pars is a list containing the parameters used to generate simulated population eg; R, s0_av , g_av, z_av = 0.25, 
#'             bin_width_age, bin_width_size.
#' @param fitted is a list containg estimated parameters from a stan mortality_size model.
#'
#' @return a ggplot of size-frequency histogram with fitted function line

plot_size_dist <- function(data, pars, fitted = NULL) {
  p1 <- data %>% 
    ggplot(aes(size_sampled)) + 
    geom_histogram(binwidth = pars$bin_width_size) +
    stat_function(fun = function(x) pars$R / pars$g_av * exp(-pars$z_av/pars$g_av*(x-pars$s0_av))*pars$bin_width_size)  
  
  if (!is.null(fitted))
    
    p1 <- p1 + stat_function(fun = function(x) fitted$R / fitted$g_av * exp(-fitted$z_av/fitted$g_av*(x-fitted$s0_av))*fitted$bin_width_size, col = "red")
  
  
  p1
  
  
}