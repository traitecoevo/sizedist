// Abundance slope model

data {

  // Data on counts
  int N_counts; //number of bins/counts
  real bin_lower[N_counts]; //upper bin limit
  real bin_upper[N_counts]; //lower bin limit
  int counts[N_counts]; // count in each bin

  //
  real c_mu;
  real c_sd;
  real b_mu;
  real b_sd;

}

parameters {
  real<lower = 0> b; //b is the slope parameter (ie., log(N)/log(s))
  real<lower = 0> c; //c is a constant

}

model {

  real counts_est[N_counts];

  // Priors
  c ~ lognormal(c_mu, c_sd);
  b ~ lognormal(b_mu, b_sd);

  // Model for counts
  for(i in 1:N_counts) {
    counts_est[i] =
        c/(1-b) * ((bin_upper[i]^(1-b)) - (bin_lower[i]^(1-b)));
  }
  counts ~ poisson(counts_est);

}
