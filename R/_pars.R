#' Create, modify and remove model parameters
#'
#' @param pars The name of the model in the format default_pars("modelname")
#' @param ... Name and value pair:
#' * Set value to NULL to delete parameter
#' * Values that are character strings need to be in quotes e.g. name of the model
#' @return
#' @export
#'
mutate_pars <- function(pars = default_pars("model1"), ...){
  ls <- purrr::list_modify(.x = pars,...)

  #Set the order of the list
  #Which one is character filter out, sort it alphabetically and then add it back on?
  ls <- c(purrr::discard(ls, is.character),
          purrr::keep(ls, is.character))

  ls
}

join_pars <- function(stan_data,
                      pars,
                      var){

  out  <- c(stan_data,
            pars[var])

  #Order the list with model at the end I don't know if this is important but it looks nice
  out <- c(purrr::discard(out, is.character),
          purrr::keep(out, is.character))

  out

}



