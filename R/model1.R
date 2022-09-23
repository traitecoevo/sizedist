
#' @rdname default_pars
default_pars_model1 <- function() {
  pars <- list(
    R= 500,
    s0_av = 3,
    log10s0_sd = 0,
    g_av = 0.21,
    log10g_sd = 0.0,
    z_av = 0.25,
    log10z_sd = 0.0
  )

  list(model = "model1",
       pars = pars)
}


sample_individual_variation_model1 <- function(n, pars) {
  with(
    pars$pars,
    dplyr::tibble(
      size_birth = 10 ^ rnorm(n, log10(s0_av), log10s0_sd),
      growth_rate = 10 ^ rnorm(n, log10(g_av), log10g_sd),
      mortality_rate = 10 ^ rnorm(n, log10(z_av), log10z_sd)
    )
  )
}

#' @rdname simulate_growth
simulate_growth_model1 <- function(individual_data) {
  with(individual_data,
       size_birth + growth_rate * age
       )
}

#' @rdname simulate_cumulative_mortality
simulate_cumulative_mortality_model1 <- function(individual_data) {
  with(individual_data,
       mortality_rate * age
       )
}

#' @rdname age_dist_model
age_dist_model1 <- function(x, pars) {
  with(pars$pars,
       R * exp(-z_av*x)
       )
}

#' @rdname size_dist_model
size_dist_model1 <- function(x, pars) {
  with(pars$pars,
       R / g_av * exp(-z_av/g_av*(x-s0_av))
       )
}

