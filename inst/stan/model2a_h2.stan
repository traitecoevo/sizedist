// Abundance slope model

data {

  // Data on counts
  int N_counts; //number of bins/counts
  int N_samples; //number of samples (i.e., lvl 1)
  int N_groups; //number of groups (i.e., lvl 2)

  real bin_lower[N_counts]; //upper bin limit
  real bin_upper[N_counts]; //lower bin limit
  int counts[N_counts]; // count in each bin

  int sample_index[N_counts];
  int group_index[N_samples];

  //
  real c_mu;
  real c_sd;
  real b_mu;
  real b_sd;

}

parameters {
  real<lower = 0> b; //b is the slope parameter (ie., log(N)/log(s))
  real<lower = 0> c; //c is a constant

  real<lower = 0> b_group[N_groups]; //b is the slope parameter (ie., log(N)/log(s))
  real<lower = 0> c_group[N_groups]; //c is a constant

  real<lower = 0> b_sample[N_samples]; //b is the slope parameter (ie., log(N)/log(s))
  real<lower = 0> c_sample[N_samples]; //c is a constant

  real<lower = 0> sigma_c_group;
  real<lower = 0> sigma_b_group;

  real<lower = 0> sigma_c_sample[N_groups];
  real<lower = 0> sigma_b_sample[N_groups];

}

model {

  real counts_est[N_counts];
  int ID;

  // Priors
    // Global means
  c ~ lognormal(c_mu, c_sd);
  b ~ lognormal(b_mu, b_sd);

    // level 2 means
  c_group ~ lognormal(log(c), sigma_c_group);
  b_group ~ lognormal(log(b), sigma_b_group);

    // sample level means
  c_sample ~ lognormal(log(c_group[group_index]), sigma_c_sample[group_index]);
  b_sample ~ lognormal(log(b_group[group_index]), sigma_b_sample[group_index]);

      // sample variance
  sigma_c_sample ~ cauchy(0, 1);
  sigma_b_sample ~ cauchy(0, 1);

  // Model for counts
  for(i in 1:N_counts) {
    ID = sample_index[i];

    counts_est[i] =
        c_sample[ID]/(1-b_sample[ID]) * ((bin_upper[i]^(1-b_sample[ID])) - (bin_lower[i]^(1-b_sample[ID])));
  }
  counts ~ poisson(counts_est);

}
