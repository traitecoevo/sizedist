//simple mortality function

data {
  
  // Data on counts
  int N_counts;
  real l0;
  real size_lower[N_counts];
  real size_upper[N_counts];
  int counts[N_counts];
  
  // Data on growth
  
  int<lower=0> N_growth;
  vector[N_growth] age;
  vector[N_growth] size_ind;
}

parameters {
  real<lower = 0> Z;
  real<lower = 0> R;
  real<lower = 0> g;
  real<lower=0> sigma_size;
}

model {
  
  real counts_est[N_counts];
  real size_est[N_growth];

  // Priors
  Z ~ cauchy(0, 10);
  g ~ cauchy(0, 10); 
  R ~ cauchy(0, 100);
  sigma_size ~ cauchy(0, 2.5);
  
  // Model for counts
  for(i in 1:N_counts) {
    counts_est[i] = 
        - R/Z * (exp(-Z/g*(size_upper[i] - l0)) - exp(-Z/g*(size_lower[i] - l0)));
  }
  counts ~ poisson(counts_est);

  // Model for growth rate
  for(i in 1:N_growth) {
    size_est[i] = l0 + age[i]*g;
  }
  
  // model for how y varies
  size_ind ~ normal(size_est, sigma_size);
}
