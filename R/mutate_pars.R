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



