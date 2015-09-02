data{
  int<lower=1> N;
  real ind_inc2[N];
  real ind_inc3[N];
  real ind_inc4[N];
  real ind_inc5[N];
  real state_inc2[N];
  real state_inc3[N];
  real state_inc4[N];
  real state_inc5[N];
  int rep_votes[N];
  int rep_votes_sample_num[N];
}
parameters{
  real b_ind_inc2;
  real b_ind_inc3;
  real b_ind_inc4;
  real b_ind_inc5;
  real b_state_inc2;
  real b_state_inc3;
  real b_state_inc4;
  real b_state_inc5;
  real Intercept;
}
transformed parameters{
  }
model{
  vector[N] p;
  b_ind_inc2 ~ normal(0, 10);
  b_ind_inc3 ~ normal(0, 10);
  b_ind_inc4 ~ normal(0, 10);
  b_ind_inc5 ~ normal(0, 10);
  b_state_inc2 ~ normal(0, 10);
  b_state_inc3 ~ normal(0, 10);
  b_state_inc4 ~ normal(0, 10);
  b_state_inc5 ~ normal(0, 10);
  Intercept ~ normal(0, 10);
  for (i in 1:N) {
    p[i] <- Intercept + b_ind_inc2 * ind_inc2[i] + b_ind_inc3 * ind_inc3[i] + b_ind_inc4 * ind_inc4[i] + b_ind_inc5 * ind_inc5[i] + b_state_inc2 * state_inc2[i] + b_state_inc3 * state_inc3[i] + b_state_inc4 * state_inc4[i] + b_state_inc5 * state_inc5[i];
  }
  rep_votes ~ binomial_logit(rep_votes_sample_num, p);
}
generated quantities{
  vector[N] p;
  real dev;
  dev <- 0;
  for (i in 1:N) {
    p[i] <- Intercept + b_ind_inc2 * ind_inc2[i] + b_ind_inc3 * ind_inc3[i] + b_ind_inc4 * ind_inc4[i] + b_ind_inc5 * ind_inc5[i] + b_state_inc2 * state_inc2[i] + b_state_inc3 * state_inc3[i] + b_state_inc4 * state_inc4[i] + b_state_inc5 * state_inc5[i];
  }
  dev <- dev + (-2) * binomial_logit_log(rep_votes, rep_votes_sample_num, p);
}

