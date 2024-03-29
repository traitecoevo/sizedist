//simple mortality function

data {
  int N_counts;
  real s0_av;
  real g_av;
  real bin_lower[N_counts];
  real bin_upper[N_counts];
  int counts[N_counts];

  //Priors
  real Z_mu;
  real Z_sd;
  real R_mu;
  real R_sd;
}

parameters {
  real<lower = 0> Z;
  real<lower = 0> R;
  real<lower = 0> g;
}

model {

  real counts_est[N_counts];

  // Priors
  Z ~ cauchy(Z_mu, Z_sd);
  g ~  normal(g_av, 0.0001); //cauchy(0, 10);
  R ~ cauchy(R_mu, R_sd);

  // Model estimated countss
  for(i in 1:N_counts) {
    counts_est[i] =
        - R/Z * (exp(-Z/g*(bin_upper[i] - s0_av)) - exp(-Z/g*(bin_lower[i] - s0_av)));
  }

  // Likelihood
  counts ~ poisson(counts_est);
}
