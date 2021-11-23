#' Plot the size distribution
#'
#' @description This function is for plotting the size distribution of simulated populations.
#' @param data a dataframe with a simulated size, size_sampled
#' @param pars list of parameter values for the size-distribution model, including model name
#' @param binwidth The bin width for size, usually the smallest resolution of measurement e.g 0.1 mm
#' @param fitted is a list containing estimated parameters from a stan mortality_size model.
#' @importFrom ggplot2 ggplot aes geom_histogram stat_function
#' @importFrom rlang .data
#' @return a ggplot of size-frequency histogram with fitted function line
#' @export

plot_size_dist <- function(data, pars, binwidth, fitted = NULL) {
  p1 <- data %>%
    ggplot(aes(.data$size)) +
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
#' @return list containing fitted values predicted by the model

extract_fitted <- function(fit, pars){
  switch(pars$model,
         model1a = extract_fitted_model1a(fit, pars),
         model1b = extract_fitted_model1b(fit, pars),
         model1c = extract_fitted_model1c(fit, pars)
  )
}

#' @rdname extract_fitted

extract_fitted_model1a <- function(fit, pars){
  fit_Z <- mean(rstan::extract(fit, pars="Z")[["Z"]])
  fit_R <- mean(rstan::extract(fit, pars="R")[["R"]])

  # fitted values for plotting
  fitted <- list(model = pars$model,
                 pars = list(R = fit_R,
                             z_av = fit_Z)
  )

  fitted
}

#' @rdname extract_fitted

extract_fitted_model1b <- function(fit, pars){
  fit_Z <- mean(rstan::extract(fit, pars="Z")[["Z"]])
  fit_R <- mean(rstan::extract(fit, pars="R")[["R"]])
  fit_g <- mean(rstan::extract(fit, pars="g")[["g"]])

  # fitted values for plotting
  fitted <- list(model = pars$model,
                 pars = list(R = fit_R,
                             s0_av = pars$pars$s0_av,
                             z_av = fit_Z,
                             g_av = fit_g))


  fitted
}

#' @rdname extract_fitted

extract_fitted_model1c <- function(fit, pars){
  fit_Z <- mean(rstan::extract(fit, pars="Z")[["Z"]])
  fit_R <- mean(rstan::extract(fit, pars="R")[["R"]])
  fit_g <- mean(rstan::extract(fit, pars="g")[["g"]])

  # fitted values for plotting
  fitted <- list(model = pars$model,
                 pars = list(R = fit_R,
                             s0_av = pars$pars$s0_av,
                             z_av = fit_Z,
                             g_av = fit_g))


  fitted
}


