#' fishlarvae dataset
#'
#' A simulated dataset containing size and age data of individual fish.
#' Each row is an individual.
#' Generated from data-raw/DATASET.R
#'
#' @format A tibble with 4009 rows and 8 columns:
#' \describe{
#'  \item{day_born}{day when larvae was born, day starts at 0}
#'  \item{age}{age when individual was sampled}
#'  \item{individual}{An individual animal's name}
#'  \item{size_birth}{size at birth assigned randomly}
#'  \item{growth_rate}{rate of growth, assigned randomly}
#'  \item{size_sampled}{size when individual was resampled again}
#'  \item{mortality_rate}{rate of mortality}
#'  \item{pr_survival}{probability of survival, exp(-mortality_rate*age)}
#'   }
#'
#' "fishlarvae"
