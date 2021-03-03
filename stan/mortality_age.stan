//simple mortality function

data {
  int N;
  real age_lower[N];
  real age_upper[N];
  int counts[N];
}

parameters {
  real<lower = 0> Z;
  real<lower = 0> R;
}

model {
  
  real counts_est[N];

  // Priors
  Z ~ cauchy(0, 10);
  
  // Model estimated counts
  for(i in 1:N){
      counts_est[i] =  -R/Z * ( exp(-Z*age_upper[i]) - exp(-Z*age_lower[i]));
  }

  // Likelihood
  counts ~ poisson(counts_est);
}
