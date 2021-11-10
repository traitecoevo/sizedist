#' Add priors for sizedist models
#'
#' @param pars pars object
#' @param Z_sd standard deviation for Z
#'
#' @return list of priors appended to pars list
#' @export
#'
#' @examples
add_priors <- function(pars,
                       Z_sd = 10){
  pars$priors$Z_sd <- Z_sd

  pars
}


