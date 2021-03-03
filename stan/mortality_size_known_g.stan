//simple mortality function

data {
  int N;
  real l0;
  real lit_G;
  real size_lower[N];
  real size_upper[N];
  int counts[N];
}

parameters {
  real<lower = 0> Z;
  real<lower = 0> R;
  real<lower = 0> g;
}

model {
  
  real counts_est[N];

  // Priors
  Z ~ cauchy(0, 10);
  g ~  normal(lit_G, 0.0001); //cauchy(0, 10); 
  R ~ cauchy(0, 100);
  
  // Model estimated countss
  for(i in 1:N) {
    //counts_est[i] = 0.5*(R/g * (exp(-Z/g*(sizes[i+1] - l0)) + exp(-Z/g*(sizes[i] - l0))));
    counts_est[i] = 
        - R/Z * (exp(-Z/g*(size_upper[i] - l0)) - exp(-Z/g*(size_lower[i] - l0)));
  }

  // Likelihood
  counts ~ poisson(counts_est);
}
