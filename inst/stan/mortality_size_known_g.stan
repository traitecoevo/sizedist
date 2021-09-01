//simple mortality function

data {
  int n;
  real l0;
  real known_g;
  real size_lower[n];
  real size_upper[n];
  int counts[n];
}

parameters {
  real<lower = 0> Z;
  real<lower = 0> R;
  real<lower = 0> g;
}

model {

  real counts_est[n];

  // Priors
  Z ~ cauchy(0, 10);
  g ~  normal(known_g, 0.0001); //cauchy(0, 10);
  R ~ cauchy(0, 100);

  // Model estimated countss
  for(i in 1:n) {
    //counts_est[i] = 0.5*(R/g * (exp(-Z/g*(sizes[i+1] - l0)) + exp(-Z/g*(sizes[i] - l0))));
    counts_est[i] =
        - R/Z * (exp(-Z/g*(size_upper[i] - l0)) - exp(-Z/g*(size_lower[i] - l0)));
  }

  // Likelihood
  counts ~ poisson(counts_est);
}
