#' Default parameters for size-distribution models
#'
#' @param model name of size-distribution model being modeled
#' @rdname default_pars
#' @return list of parameter values for the size-distribution model, including model name
#' @export

default_pars <- function(model) {

  if(missing(model)){
    abort("Model name must be supplied! Try `model1`")
  }

  if(! is.character(model)){
    abort("Character string of model name must be supplied! Try `model1`")
  }

  if( is.character(model) & ! stringr::str_sub(model, 1, 5) == "model" ){
    abort("This model is not supported! Try `model1`")
  }

  # Switch for different models
  switch (model,
          model1 = default_pars_model1(),
          model2 = default_pars_model2()
          )
}

#' Samples time of birth from a homogeneous Poisson process.
#'
#' Samples are drawn assuming a homogeneous Poisson process. Under this distribution, the inter-arrival times follow a log distribution. https://en.wikipedia.org/wiki/Poisson_distribution
#' @param R average birth rate per unit time
#' @param time_end Duration of simulation
#' @export

sample_birth_times <- function(R, time_end) {

  # simulate birth times assuming homogeneous Possion process
  # https://transp-or.epfl.ch/courses/OptSim2012/slides/05b-poisson.pdf
  # Could extend to time varying rate via non-homogeneous process
  # 1. t = 0, k = 0.
  # 2. Draw r ∼ U (0, 1).
  # 3. t = t − ln(r)/λ.
  # 4.If t>T,STOP.
  # 5. k = k + 1, Sk = t.
  # 6. Go to step 2.

  # to make sure we get enough individuals in the sample, simulate 2 x the expected number
  r <- stats::runif(2 * R * time_end)
  inter_event_times <- -log(r) / R
  birth_times <- cumsum(inter_event_times)
  # cull to only those born within allowable time interval
  birth_times[birth_times <= time_end]
}

#' Sample variation in parameters among individuals across the population under a particular size-distribution model
#'
#' @param n  number of individuals to sample
#' @return list of parameter values for the size-distribution model, including model name
#' @inheritParams simulate_population
#' @rdname sample_individual_variation
#' @export

sample_individual_variation <- function(n, pars) {
  # Switch for different models

  # Model prefix
  prefix <- stringr::str_sub(pars$model, 1,6)

  switch (prefix,
          model1 = sample_individual_variation_model1(n, pars),
          model2 = sample_individual_variation_model2(n, pars)
          )
}


#' Simulated integrated growth from time_birth to time_end for each individual under a particular size-distribution model
#'
#' @param individual_data A dataframe of individual data. Must include columns age and parameters of the size-distribution model.
#' @param pars pars list from which $model can be taken from
#' @return A vector of sizes for each individual. Length is the same as number of rows in the input data
#' @export
#' @rdname simulate_growth
simulate_growth <- function(individual_data, pars) {
  # Switch for different models
  prefix <- stringr::str_sub(pars$model, 1,6)

  switch (prefix,
          model1 = simulate_growth_model1(individual_data),
          model2 = simulate_growth_model2(individual_data)
          )
}

#' Simulated integrated mortality rate (= cumulative hazard) from age birth to end for each individual under a particular size-distribution model
#'
#' @param individual_data A dataframe of individual data. Must include columns size_birth, size, age and paramaters of the size-distribution model.
#' @param pars pars list from which $model can be taken from
#' @return A vector of integrated mortality rates for each individual. Lenght is the same as number of rows in the input data
#' @export
#' @rdname simulate_cumulative_mortality

simulate_cumulative_mortality <- function(individual_data, pars) {
  # Switch for different models
  prefix <- stringr::str_sub(pars$model, 1,6)

  switch (prefix,
          model1 = simulate_cumulative_mortality_model1(individual_data),
          model2 = simulate_cumulative_mortality_model2(individual_data)
          )
}


#' Simulates individuals sampled from population process with particular size-distribution model
#'
#' @param time_end Time to end simulation
#' @param pars list of parameter values for the size-distribution model, including model name
#' @param keep_dead logical indicating whether dead individuals should be kept
#' @return a data frame (tibble) of simulated data
#' @export
simulate_population <-  function(pars = default_pars("model1"),
                                 time_end = 25,
                                 keep_dead = FALSE) {

  # R CMD check by-pass
  time_birth = NULL
  is_dead = NULL

  df <-
    dplyr::tibble(
      # total number of individuals sampled from birth process and time of birth
      time_birth = sample_birth_times(pars$pars$R, time_end),
      time_end = time_end,
      age = time_end - time_birth
    ) %>%
    # sample individual variation in rate
    dplyr::bind_cols(.,
              sample_individual_variation(nrow(.), pars))
  # simulate growth and survival
  # could be converted to mutate with pipe
  # need to pass all columns into functions, but without knowing names

  df$size<- simulate_growth(df, pars)
  df$cumulative_hazard <- simulate_cumulative_mortality(df, pars)

  # common across all models
  df$survival <- exp(-df$cumulative_hazard)
  df$is_dead <- df$survival < stats::runif(length(df$survival))

  # remove dead individuals, unless otherwise specified
  if (!keep_dead)
    df <- df %>% dplyr::filter(!is_dead)

  df
}


#' Predicted  distribution of individual ages under a particular size-distribution model, for a given binwidth
#'
#' @param x Age of individuals
#' @param pars list of parameter values for the size-distribution model, including model name
#' @return Predicted age distribution for each x
#' @export
#' @rdname age_dist_model
age_dist_model <- function(x, pars) {
  prefix <- stringr::str_sub(pars$model, 1, 6)

  switch (prefix,
          model1 = age_dist_model1(x, pars),
          model2 = age_dist_model2(x, pars)
          )
}


#' Predicted density distribution of individual sizes under a particular size-distribution model
#'
#' @param x Size of individuals
#' @param pars list of parameter values for the size-distribution model, including model name
#' @return Predicted size distribution for each x
#' @export
#' @rdname size_dist_model
size_dist_model <- function(x, pars) {
  # Switch for different models
  prefix <- stringr::str_sub(pars$model, 1, 6)

  switch (prefix,
          model1 = size_dist_model1(x, pars),
          model2 = size_dist_model2(x, pars)
          )
}

#' Simulate noise for any variable from a distribution with mean = 0
#' @param data simulated dataframe or own data
#' @param var variable you wish to add noise to
#' @param sd standard deviation
#' @param overwrite logical, if TRUE the original variable will be overwritten
#' @return dataframe where variable w
#' @importFrom rlang :=
#' @export

add_sampling_noise <- function(data, var, sd = 0.1, overwrite = TRUE){

  ret <- data %>%
    dplyr::mutate({{var}} := {{var}} + stats::rnorm(length({{var}}), 0, 0.1))

  if(!overwrite){
    suffix <- "_noise"

    ret <- data %>%
      dplyr::mutate("{{var}}{suffix}" := {{var}} + stats::rnorm(length({{var}}), 0, 0.1))
  }

  ret
}

