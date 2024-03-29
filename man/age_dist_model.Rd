% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model1.R, R/model2.R, R/simulate_population.R
\name{age_dist_model1}
\alias{age_dist_model1}
\alias{age_dist_model2}
\alias{age_dist_model}
\title{Predicted  distribution of individual ages under a particular size-distribution model, for a given binwidth}
\usage{
age_dist_model1(x, pars)

age_dist_model2(x, pars)

age_dist_model(x, pars)
}
\arguments{
\item{x}{Age of individuals}

\item{pars}{list of parameter values for the size-distribution model, including model name}
}
\value{
Predicted age distribution for each x
}
\description{
Predicted  distribution of individual ages under a particular size-distribution model, for a given binwidth
}
