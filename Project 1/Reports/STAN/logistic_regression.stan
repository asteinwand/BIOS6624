
data {
  int<lower=0> N;
  int<lower=0> P;
  matrix[N, P] X;
  array[N] int<lower=0, upper=1> y;
  vector[P] prior_mean;
  vector<lower=0>[P] prior_sd;
}
parameters {
  vector[P] beta;
}
model {
  beta ~ normal(prior_mean, prior_sd);
  y ~ bernoulli_logit(X * beta);
}
generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = bernoulli_logit_lpmf(y[n] | X[n] * beta);
  }
}
