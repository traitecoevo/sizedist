#' Plot the size distribution
#'
#' @description This function is for plotting the size distribution of simulated populations.
#' @param data a dataframe with a simulated size, size_sampled
#' @param pars list of parameter values for the size-distribution model, including model name
#' @param binwidth The bin width for size, usually the smallest resolution of measurement e.g 0.1 mm
#' @param fitted is a list containing estimated parameters from a stan mortality_size model.
#' @importFrom ggplot2 ggplot aes geom_histogram stat_function
#' @return a ggplot of size-frequency histogram with fitted function line
#' @export

plot_size_dist <- function(data, pars, binwidth, fitted = NULL) {
  p1 <- data %>%
    ggplot(aes(size)) +
    geom_histogram(binwidth = binwidth) +
    stat_function(fun = function(x) size_dist_model(x, pars) * binwidth)

  if (!is.null(fitted))
    p1 <- p1 + stat_function(fun = function(x) size_dist_model(x, fitted)*binwidth, col = "red")

  p1

}

#' Extract fitted values for Z, R, g
#'
#' @param fit model fit for growth model
#' @param pars pars list used for model fitting
#' @export
#' @return

extract_fitted <- function(fit, pars){
  # extract posterior mean estimates
  fit_Z_size <- mean(rstan::extract(fit, pars="Z")[["Z"]])
  fit_R_size <- mean(rstan::extract(fit, pars="R")[["R"]])
  fit_g_size <- mean(rstan::extract(fit, pars="g")[["g"]])

  # fitted values for plotting
  fitted <- list(model = pars$model,
                 pars = list(R = fit_R_size,
                            s0_av = pars$pars$s0_av,
                            z_av = fit_Z_size,
                            g_av = fit_g_size))

  fitted
}

