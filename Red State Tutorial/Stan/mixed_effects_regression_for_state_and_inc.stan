data{
  int<lower=1> N;
  int state[N];
  int<lower=1> N_state;
  real inc2[N];
  real inc3[N];
  real inc4[N];
  real inc5[N];
  int rep_votes[N];
  int rep_votes_sample_num[N];
}
parameters{
  vector<lower=0>[5] sigma_m_Intercept_inc2_inc3_inc4_inc5_by_state;
  matrix[5, N_state] m_Intercept_inc2_inc3_inc4_inc5_by_state_std;
  cholesky_factor_corr[5] L_Rho_m_Intercept_inc2_inc3_inc4_inc5_by_state;
}
transformed parameters{
  matrix[N_state, 5] m_Intercept_inc2_inc3_inc4_inc5_by_state;
  m_Intercept_inc2_inc3_inc4_inc5_by_state <- (diag_pre_multiply(sigma_m_Intercept_inc2_inc3_inc4_inc5_by_state, L_Rho_m_Intercept_inc2_inc3_inc4_inc5_by_state) * m_Intercept_inc2_inc3_inc4_inc5_by_state_std)';
}
model{
  vector[N] p_logit;
  sigma_m_Intercept_inc2_inc3_inc4_inc5_by_state ~ normal(0,1);
  to_vector(m_Intercept_inc2_inc3_inc4_inc5_by_state_std) ~ normal(0,1);
  L_Rho_m_Intercept_inc2_inc3_inc4_inc5_by_state ~ lkj_corr_cholesky(2);
  for (i in 1:N) {
    p_logit[i] <- m_Intercept_inc2_inc3_inc4_inc5_by_state[state[i], 1] + m_Intercept_inc2_inc3_inc4_inc5_by_state[state[i], 2] * inc2[i] + m_Intercept_inc2_inc3_inc4_inc5_by_state[state[i], 3] * inc3[i] + m_Intercept_inc2_inc3_inc4_inc5_by_state[state[i], 4] * inc4[i] + m_Intercept_inc2_inc3_inc4_inc5_by_state[state[i], 5] * inc5[i];
  }
  rep_votes ~ binomial_logit(rep_votes_sample_num, p_logit);
}
generated quantities{
  vector[N] p_logit;
  real dev;
  dev <- 0;
  for (i in 1:N) {
    p_logit[i] <- m_Intercept_inc2_inc3_inc4_inc5_by_state[state[i], 1] + m_Intercept_inc2_inc3_inc4_inc5_by_state[state[i], 2] * inc2[i] + m_Intercept_inc2_inc3_inc4_inc5_by_state[state[i], 3] * inc3[i] + m_Intercept_inc2_inc3_inc4_inc5_by_state[state[i], 4] * inc4[i] + m_Intercept_inc2_inc3_inc4_inc5_by_state[state[i], 5] * inc5[i];
  }
  dev <- dev + (-2) * binomial_logit_log(rep_votes, rep_votes_sample_num, p_logit);
}

