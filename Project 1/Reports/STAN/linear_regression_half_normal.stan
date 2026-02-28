
data {
  int<lower=0> N;
  int<lower=0> P;
  matrix[N, P] X;
  vector[N] y;
  vector[P] prior_mean;
  vector<lower=0>[P] prior_sd;
  real<lower=0> sigma_prior_sd;
}
parameters {
  vector[P] beta;
  real<lower=0> sigma;
}
model {
  beta ~ normal(prior_mean, prior_sd);
  sigma ~ normal(0, sigma_prior_sd);
  y ~ normal(X * beta, sigma);
}
generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(y[n] | X[n] * beta, sigma);
  }
}
