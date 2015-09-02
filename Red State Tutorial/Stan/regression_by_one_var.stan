data{
  int<lower=1> N;
  real inc2[N];
  real inc3[N];
  real inc4[N];
  real inc5[N];
  int rep_votes[N];
  int rep_votes_sample_num[N];
}
parameters{
  real b_inc2;
  real b_inc3;
  real b_inc4;
  real b_inc5;
  real Intercept;
}
transformed parameters{
  }
model{
  vector[N] p;
  b_inc2 ~ normal(0, 10);
  b_inc3 ~ normal(0, 10);
  b_inc4 ~ normal(0, 10);
  b_inc5 ~ normal(0, 10);
  Intercept ~ normal(0, 10);
  for (i in 1:N) {
    p[i] <- Intercept + b_inc2 * inc2[i] + b_inc3 * inc3[i] + b_inc4 * inc4[i] + b_inc5 * inc5[i];
  }
  rep_votes ~ binomial_logit(rep_votes_sample_num, p);
}
generated quantities{
  vector[N] p;
  real dev;
  dev <- 0;
  for (i in 1:N) {
    p[i] <- Intercept + b_inc2 * inc2[i] + b_inc3 * inc3[i] + b_inc4 * inc4[i] + b_inc5 * inc5[i];
  }
  dev <- dev + (-2) * binomial_logit_log(rep_votes, rep_votes_sample_num, p);
}

