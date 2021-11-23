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
                   model1c = default_priors_model1c()
  )

  c(pars,
    priors)
}

#'@rdname default_priors

default_priors_model1a <- function(){
  list(priors = list(Z_sd = 10)
  )
  }

#'@rdname default_priors
default_priors_model1b <- function(){
  list(priors = list(
    Z_sd = 10,
    g_sd = 0.0001,
    R_sd = 100)
  )
}

#'@rdname default_priors
default_priors_model1c <- function(){
  list(priors = list(
    Z_sd = 10,
    g_sd = 0.0001,
    R_sd = 100,
    sigma_size_sd = 2.5)
    )
}



