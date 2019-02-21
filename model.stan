data {
  int<lower=0> J;          // number of securities
  int<lower=0> N;         // number of observations
  real returns[N];        // observed returns
  int security[N];       // associated security for retun i
}
parameters {
  real mu[J];
  real<lower=0> sigma[J];
}
model {
  mu ~ normal(0, 1);
  sigma ~ cauchy(0, 1);
  for (i in 1:N){
    returns[i] ~ normal(mu[security[i]], sigma[security[i]]);
  }
}