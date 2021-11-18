//model2_h? -Hierachical model for growth and mortality as power functions
//This stan model estimates R, z, g and s0, at the sample and global level.
//
data {

  // Data on counts
  int N_counts;
  int N_samples;

  real bin_lower[N_counts];
  real bin_upper[N_counts];
  int counts[N_counts];

  int sample_index[N_counts];


  // Data on growth

  int<lower=0> N_growth;
  vector[N_growth] age;
  vector[N_growth] size_ind;
  int ind_sample_index[N_growth];
}

parameters {
  //Z
  real <lower = 0, upper = 2> Z; // <lower = 0>

  real<lower = 0, upper = 2> Z_t[N_samples];

  real<lower= 0> sigma_Z_t;

  //R
  real<lower = 0, upper = 200> R; //Note upper limit

  real<lower = 0, upper = 200> R_t[N_samples];

  real<lower= 0> sigma_R_t;

  //g
  real<lower = 0, upper = 2> g;

  real<lower = 0, upper = 2> g_t[N_samples];

  real<lower = 0> sigma_g_t;

  //s0
  real<lower = 0> s0;

  //size
  real<lower=0> sigma_size;

}

model {

  real counts_est[N_counts];
  real size_est[N_growth];

  int ID;

  // Priors
      //Z
  Z ~ lognormal(-1, 1);

  sigma_Z_t ~ cauchy(0, 2.5);

      //g
  g ~ lognormal(-1, 1);

  sigma_g_t ~ cauchy(0, 2.5);

     //R
  R ~ lognormal(2, 1);

  sigma_R_t ~ cauchy(0, 2.5);

     //lo
  s0 ~ cauchy(1, 2);

     //size
  sigma_size ~ cauchy(0, 2.5);

  // sample means

  Z_t ~ lognormal(log(Z), sigma_Z_t);
  g_t ~ lognormal(log(g), sigma_g_t);
  R_t ~ lognormal(log(R), sigma_R_t);



  // Model for counts
  for(i in 1:N_counts) {
    ID = sample_index[i];

        counts_est[i] =

         - R_t[ID]/Z_t[ID] * s0^(Z_t[ID]/g_t[ID]) * ((bin_upper[i]^(- (Z_t[ID]/g_t[ID]))) - (bin_lower[i]^((- Z_t[ID]/g_t[ID]))));
  }
  counts ~ poisson(counts_est);

  // Model for growth rate
  for(i in 1:N_growth) {

    size_est[i] = s0 * exp(age[i]*g_t[ind_sample_index[i]]);
  }
  // model for how y varies
  size_ind ~ normal(size_est, sigma_size);
}
