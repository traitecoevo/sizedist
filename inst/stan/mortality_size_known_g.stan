//simple mortality function

data {
  int N_counts;
  real s0_av;
  real known_g;
  real size_lower[N_counts];
  real size_upper[N_counts];
  int counts[N_counts];
}

parameters {
  real<lower = 0> Z;
  real<lower = 0> R;
  real<lower = 0> g;
}

model {

  real counts_est[N_counts];

  // Priors
  Z ~ cauchy(0, 10);
  g ~  normal(known_g, 0.0001); //cauchy(0, 10);
  R ~ cauchy(0, 100);

  // Model estimated countss
  for(i in 1:N_counts) {
    //counts_est[i] = 0.5*(R/g * (exp(-Z/g*(sizes[i+1] - s0_av)) + exp(-Z/g*(sizes[i] - s0_av))));
    counts_est[i] =
        - R/Z * (exp(-Z/g*(size_upper[i] - s0_av)) - exp(-Z/g*(size_lower[i] - s0_av)));
  }

  // Likelihood
  counts ~ poisson(counts_est);
}
