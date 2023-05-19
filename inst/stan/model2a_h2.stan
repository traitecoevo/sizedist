// Abundance slope model

data {

  // Data on counts
  int N_counts; //number of bins/counts
  int N_level1; //number of level1 (i.e., lvl 1)
  int N_level2; //number of groups (i.e., lvl 2)

  real bin_lower[N_counts]; //upper bin limit
  real bin_upper[N_counts]; //lower bin limit
  int counts[N_counts]; // count in each bin

  int level1_index[N_counts];
  int level2_index[N_level1];

  //
  real c_mu;
  real c_sd;
  real b_mu;
  real b_sd;

}

parameters {
  real<lower = 0> b_global; //b is the slope parameter (ie., log(N)/log(s))
  real<lower = 0> c_global; //c is a constant

  real<lower = 0> b_level2[N_level2]; //b is the slope parameter (ie., log(N)/log(s))
  real<lower = 0> c_level2[N_level2]; //c is a constant

  real<lower = 0> b_level1[N_level1]; //b is the slope parameter (ie., log(N)/log(s))
  real<lower = 0> c_level1[N_level1]; //c is a constant

  real<lower = 0> sigma_c_level2;
  real<lower = 0> sigma_b_level2;

  real<lower = 0> sigma_c_level1[N_level2];
  real<lower = 0> sigma_b_level1[N_level2];

}

model {

  real counts_est[N_counts];
  int ID;

  // Priors
    // Global means
  c_global ~ lognormal(c_mu, c_sd);
  b_global ~ lognormal(b_mu, b_sd);

    // level 2 means
  c_level2 ~ lognormal(log(c_global), sigma_c_level2);
  b_level2 ~ lognormal(log(b_global), sigma_b_level2);

    // level1 level means
  c_level1 ~ lognormal(log(c_level2[level2_index]), sigma_c_level1[level2_index]);
  b_level1 ~ lognormal(log(b_level2[level2_index]), sigma_b_level1[level2_index]);

      // level1 variance
  sigma_c_level1 ~ cauchy(0, 1);
  sigma_b_level1 ~ cauchy(0, 1);

  // Model for counts
  for(i in 1:N_counts) {
    ID = level1_index[i];

    counts_est[i] =
        c_level1[ID]/(1-b_level1[ID]) * ((bin_upper[i]^(1-b_level1[ID])) - (bin_lower[i]^(1-b_level1[ID])));
  }
  counts ~ poisson(counts_est);

}
