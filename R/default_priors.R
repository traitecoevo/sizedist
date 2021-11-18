default_priors <- function(model = "model1a", pars){
  #Switch for different models
  switch(model,
        model1a = default_priors_model1a(),
        model1b = default_priors_model1b(),
        model1c = default_priors_model1c()
        )
}

default_priors_model1a <- function(){
 priors <- list(
    Z_sd = 10
  )

  list(priors = priors)
  }

default_priors_model1b <- function(){
  priors <- list(
    Z_sd = 10,
    g_sd = 0.0001,
    R_sd = 100)

  list(priors = priors)
}

default_priors_model1c <- function(){
  priors <- list(
    Z_sd = 10,
    g_sd = 0.0001,
    R_sd = 100,
    sigma_size_sd = 2.5)

  list(priors = priors)
}



