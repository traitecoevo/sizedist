
#' Simulating a dataset of individual larval fish which are caught in a towed net.   
#'
#' @param R Numebr of fish born per day
#' @param s0_av The mean size of individuals at hatch
#' @param log10s0_sd Variance of individuals around the mean size at hatch
#' @param g_av The mean growth rate of individuals in the cohort
#' @param log10g_sd Variance of individual growth rates around the mean growth rate of the cohort 
#' @param z_av The mean mortality rate of individuals in the cohort
#' @param log10z_sd Variance of individual growth rates around the mean mortality rate of the cohort
#' @param growth_in_weight if TRUE growth rate is considered as the instanteous growth coefficient in weight (i.e  "size_sampled = size_birth * exp(growth_rate*age)"), If FALSE growth is modelled as change in length using a linear model.
#' 
#' @return a datraframe of simulated data

simulate_catch_data <-  function(R, 
                                 s0_av = 3,    
                                 log10s0_sd = 0, 
                                 g_av = 0.21,
                                 log10g_sd = 0.0,
                                 z_av = 0.25,
                                 log10z_sd = 0.0,
                                 max_age = 25,
                                 growth_in_weight = FALSE
                                 ) { 
  
  n_days <- 25
  gen_data <- tibble(
    day_born = seq(0, max_age-1, by = 1), 
    R = R,
    s0_av = s0_av,
    log10s0_sd = log10s0_sd,
    g_av = g_av,
    log10g_sd = log10g_sd,
    z_av = z_av,
    log10z_sd = log10z_sd
  )
  
  tmp <- gen_data %>% 
    rowwise() %>%
    mutate(
      n_born = rpois(1, R),
      individuals = list(
        tibble(
          day_born = day_born,
          age = max_age - (day_born + runif(n_born)),
          individual = seq_len(n_born),
          size_birth = 10^rnorm(n_born, log10(s0_av), log10s0_sd),
          growth_rate = 10^rnorm(n_born, log10(g_av), log10g_sd),
          size_sampled = size_birth + growth_rate*age,
          mortality_rate = 10^rnorm(n_born, log10(z_av), log10z_sd),
          pr_survival = exp(-mortality_rate*age) 
        ) %>%
          filter(pr_survival > runif(n()))
      ),
      n_sampled = nrow(individuals),
      size_average = mean(individuals$size_sampled)
    )
  
  
  #remove individuals smaller than modal size.
  if (growth_in_weight) { 
    
    tmp <- gen_data %>% 
      rowwise() %>%
      mutate(
        n_born = rpois(1, R),
        individuals = list(
          tibble(
            day_born = day_born,
            age = max_age - (day_born + runif(n_born)),
            individual = seq_len(n_born),
            size_birth = 10^rnorm(n_born, log10(s0_av), log10s0_sd),
            growth_rate = 10^rnorm(n_born, log10(g_av), log10g_sd),
            size_sampled = size_birth * exp(growth_rate*age),
            mortality_rate = 10^rnorm(n_born, log10(z_av), log10z_sd),
            pr_survival = exp(-mortality_rate*age) 
          ) %>%
            filter(pr_survival > runif(n()))
        ),
        n_sampled = nrow(individuals),
        size_average = mean(individuals$size_sampled)
      )
    
  }
  
  
  tmp$individuals %>% bind_rows()
}
