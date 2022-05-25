//simple mortality function

data {
  int N_counts;
  real bin_lower[N_counts];
  real bin_upper[N_counts];
  int counts[N_counts];
  real Z_sd;
}

parameters {
  real<lower = 0> Z;
  real<lower = 0> R;
}

model {

  real counts_est[N_counts];

  // Priors
  Z ~ cauchy(0, Z_sd);

  // Model estimated counts
  for(i in 1:N_counts){
      counts_est[i] =  -R/Z * ( exp(-Z*bin_upper[i]) - exp(-Z*bin_lower[i]));
  }

  // Likelihood
  counts ~ poisson(counts_est);
}
