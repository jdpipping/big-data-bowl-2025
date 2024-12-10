data {
  int<lower=1> n;                      // number of observations
  int<lower=1> num_players;            // number of players
  int<lower=1,upper=num_players> pid1[n];              // player ids of first safety
  int<lower=0,upper=num_players> pid2[n];              // player ids of first safety
  int num_safeties[n];                // num safeties on that play
  real y[n];                          // outcome variable
}
parameters {
  real<lower=0> sigma;
  real beta_0;
  real beta[num_players];  
}
model {
  // priors
  beta_0 ~ normal(0,1);
  beta ~ normal(0,1);
  sigma ~ normal(0,1);
  // likelihood
  for (j in 1:n) {
    if (num_safeties[j] == 1) {
      y[j] ~ normal(beta_0 + beta[pid1[j]], sigma);
    } else if (num_safeties[j] == 2) {
      y[j] ~ normal(beta_0 + beta[pid1[j]]/2 + beta[pid1[j]]/2, sigma);
    }
  }
}


