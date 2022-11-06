#' Load default priors for a specific model
#'
#' @param pars pars list object you want to join priors to
#' @param model name of the model, takes value in pars$model
#'
#' @return list object with original pars with priors joined at the end
#' @export
#'
#' @examples
#' pars <- default_pars("model1")
#' pars <- pars %>% purrr::list_modify(model = "model1a")
#' pars_new <- default_priors(pars, "model1a")

default_priors <- function(pars, model = pars$model){
  #Switch for different models
  priors <- switch(model,
                   model1a = default_priors_model1a(),
                   model1b = default_priors_model1b(),
                   model1c = default_priors_model1c(),
                   model1d = default_priors_model1d(),
                   model1e = default_priors_model1e(),
                   model2a = default_priors_model2a()
  )

  c(pars,
    priors)
}

#'@rdname default_priors

default_priors_model1a <- function(){
  list(priors = list(Z_mu = 0.1,
                     Z_sd = 1,
                     R_mu = 100,
                     R_sd = 10)
  )
  }

#'@rdname default_priors
default_priors_model1b <- function(){
  list(priors = list(
    Z_mu = 0.1,
    Z_sd = 1,
    R_mu = 100,
    R_sd = 10)
  )
}

#'@rdname default_priors
default_priors_model1c <- function(){
  list(priors = list(
    Z_mu = 0.1,
    Z_sd = 1,
    g_mu = 0.1,
    g_sd = 1,
    R_mu = 100,
    R_sd = 10,
    sigma_size_sd = 2.5)
    )
}

#'@rdname default_priors
default_priors_model1d <- function(){
  list(priors = list(
    Z_mu = 0.1,
    Z_sd = 1,
    g_mu = 0.1,
    g_sd = 1,
    R_mu = 100,
    R_sd = 10,
    s0_mu = 3,
    s0_sd = 0.1,
    sigma_size_sd = 2.5)
  )
}

#'@rdname default_priors
default_priors_model1e <- function(){
    list(priors = list(
      c_mu = 5,
      c_sd = 1,
      b_mu = 0.5,
      b_sd = 1)
    )
}

default_priors_model2a <- function(){
  list(priors = list(
    c_mu = 5,
    c_sd = 1,
    b_mu = 0.5,
    b_sd = 1)
  )
}

