// Abundance slope model

data {

  // Data on counts
  int N_counts; //number of bins/counts
  int N_samples; //number of samples

  real bin_lower[N_counts]; //upper bin limit
  real bin_upper[N_counts]; //lower bin limit
  int counts[N_counts]; // count in each bin

  int sample_index[N_counts];

  //
  real c_mu;
  real c_sd;
  real b_mu;
  real b_sd;

}

parameters {
  real<lower = 0> b; //b is the slope parameter (ie., log(N)/log(s))
  real<lower = 0> c; //c is a constant

  real<lower = 0> b_sample[N_samples]; //b is the slope parameter (ie., log(N)/log(s))
  real<lower = 0> c_sample[N_samples]; //c is a constant

  real <lower= 0> sigma_c_sample;
  real <lower= 0> sigma_b_sample;


}

model {

  real counts_est[N_counts];

  int ID;

  // Priors
    // Global
  c ~ lognormal(c_mu, c_sd);
  b ~ lognormal(b_mu, b_sd);
    // sample level
  c_sample ~ lognormal(log(c), sigma_c_sample);
  b_sample ~ lognormal(log(b), sigma_b_sample);

  // Model for counts
  for(i in 1:N_counts) {
    ID = sample_index[i];

    counts_est[i] =
        c_sample[ID]/(1-b_sample[ID]) * ((bin_upper[i]^(1-b_sample[ID])) - (bin_lower[i]^(1-b_sample[ID])));
  }
  counts ~ poisson(counts_est);

}

