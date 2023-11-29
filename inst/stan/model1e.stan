// Abundance spectra model assuming constant g

data {

  // Data on counts
  int N_counts;
  real bin_lower[N_counts];
  real bin_upper[N_counts];
  int counts[N_counts];

  //
  real c_mu;
  real c_sd;
  real b_mu;
  real b_sd;

}

parameters {
  real<lower = 0> c; // Constant
  real<lower = 0> b; // Slope

}

model {

  real counts_est[N_counts];

  // Priors
  c ~ lognormal(c_mu, c_sd);
  b ~ lognormal(b_mu, b_sd);

  // Model for counts
  for(i in 1:N_counts) {
    counts_est[i] =
      // c estimation is too high. b seems to be correct
      //(c*(exp(-b * bin_lower[i]))/b) - (c*(exp(-b * bin_upper[i]))/b);
      c * ((exp(-b * bin_lower[i]))/b) - ((exp(-b * bin_upper[i]))/b);

  }
  counts ~ poisson(counts_est);

}

