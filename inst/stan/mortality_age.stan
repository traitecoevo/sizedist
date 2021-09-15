//simple mortality function

data {
  int n;
  real age_lower[n];
  real age_upper[n];
  int counts[n];
}

parameters {
  real<lower = 0> Z;
  real<lower = 0> R;
}

model {

  real counts_est[n];

  // Priors
  Z ~ cauchy(0, 10);

  // Model estimated counts
  for(i in 1:n){
      counts_est[i] =  -R/Z * ( exp(-Z*age_upper[i]) - exp(-Z*age_lower[i]));
  }

  // Likelihood
  counts ~ poisson(counts_est);
}
