//simple mortality function

data {

  // Data on counts
  int N_counts;
  real s0_av;
  real bin_lower[N_counts];
  real bin_upper[N_counts];
  int counts[N_counts];

  // Data on growth

  int<lower=0> N_growth;
  vector[N_growth] age;
  vector[N_growth] size_ind;

  //Priors
  real Z_mu;
  real Z_sd;
  real g_mu;
  real g_sd;
  real R_mu;
  real R_sd;
  real sigma_size_sd;
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
  Z ~ cauchy(Z_mu, Z_sd);
  g ~ cauchy(g_mu, g_sd);
  R ~ cauchy(R_mu, R_sd);
  sigma_size ~ cauchy(0, sigma_size_sd);

  // Model for counts
  for(i in 1:N_counts) {
    counts_est[i] =
        - R/Z * (exp(-Z/g*(bin_upper[i] - s0_av)) - exp(-Z/g*(bin_lower[i] - s0_av)));
  }
  counts ~ poisson(counts_est);

  // Model for growth rate
  for(i in 1:N_growth) {
    size_est[i] = s0_av + age[i]*g;
  }

  // model for how y varies
  size_ind ~ normal(size_est, sigma_size);
}
