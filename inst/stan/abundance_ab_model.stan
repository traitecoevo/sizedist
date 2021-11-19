// This model estimates the flux (a) and slope (b) of the abundance size spectrum.
// The slope of the biomass spectrum is = b-1, which is ≈ Z/g.

data {

  // Data on counts
  int N_counts;
  real bin_lower[N_counts];
  real bin_upper[N_counts];
  int counts[N_counts];

}

parameters {
  real<lower = 0> b; // Slope
  real<lower = 0> a; // Flux

}

model {

  real counts_est[N_counts];

  // Priors
  a ~ lognormal(2, 1);
  b ~ lognormal(0, 1);

  // Model for counts
  for(i in 1:N_counts) {
    counts_est[i] =
        a/(1-b) * ((bin_upper[i]^(1-b)) - (bin_lower[i]^(1-b)));
  }
  counts ~ poisson(counts_est);

}
