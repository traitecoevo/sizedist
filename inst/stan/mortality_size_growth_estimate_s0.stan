//This stan model estimated R, z, g and s0, and is equivilent to model1_c
//with the addition of estimating s0

data {

  // Data on counts
  int N_counts;
  real bin_lower[N_counts];
  real bin_upper[N_counts];
  int counts[N_counts];

  // Data on growth

  int<lower=0> N_growth;
  vector[N_growth] age;
  vector[N_growth] size_ind;
}

parameters {
  real<lower = 0> Z;
  real<lower = 0> R;
  real<lower = 0> g;
  real<lower=0> sigma_size;

  real<lower = 0> s0;

}

model {

  real counts_est[N_counts];
  real size_est[N_growth];

  // Priors
  Z ~ cauchy(0, 10);
  g ~ cauchy(0, 10);
  R ~ cauchy(0, 100);
  sigma_size ~ cauchy(0, 2.5);

  s0 ~ cauchy(0, 10);

  // Model for counts
  for(i in 1:N_counts) {
    counts_est[i] =
        - R/Z * (exp(-Z/g*(bin_upper[i] - s0)) - exp(-Z/g*(bin_lower[i] - s0)));
  }
  counts ~ poisson(counts_est);

  // Model for growth rate
  for(i in 1:N_growth) {
    size_est[i] = s0 + age[i]*g;
  }

  // model for how y varies
  size_ind ~ normal(size_est, sigma_size);
}
