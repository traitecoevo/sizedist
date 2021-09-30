//simple mortality function

data {
  int N_counts;
  real age_lower[N_counts];
  real age_upper[N_counts];
  int counts[N_counts];
}

parameters {
  real<lower = 0> Z;
  real<lower = 0> R;
}

model {

  real counts_est[N_counts];

  // Priors
  Z ~ cauchy(0, 10);

  // Model estimated counts
  for(i in 1:N_counts){
      counts_est[i] =  -R/Z * ( exp(-Z*age_upper[i]) - exp(-Z*age_lower[i]));
  }

  // Likelihood
  counts ~ poisson(counts_est);
}
