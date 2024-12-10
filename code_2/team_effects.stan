data {
  int<lower=1> n;                      // number of observations
  int<lower=1> num_teams;             // number of teams
  int<lower=1,upper=num_teams> tid[n];              // team id
  real y[n];                          // outcome variable
}
parameters {
  real<lower=0> sigma;
  real beta_0;
  real beta[num_teams];  
}
model {
  // priors
  beta_0 ~ normal(0,1);
  beta ~ normal(0,1);
  sigma ~ normal(0,1);
  // likelihood
  for (j in 1:n) {
    y[j] ~ normal(beta_0 + beta[tid[j]], sigma);
  }
}

