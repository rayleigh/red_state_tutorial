library(arm)
library(car)
library(dplyr)
source("~/Documents/Gelman Research/stanRegression rewrite/R Code/rl_stan_lmer.R")

#Get and process raw data
setwd("~/Documents/Gelman Research/Replication/Original Research/")

#Build individual data
dat.vot <- read.table("data/votechoice2000-04-08.dat", header=T, sep="\t")
dat.vot$weight[is.na(dat.vot$weight)] <- 1
ok <- apply(is.na(dat.vot[1:8]), 1, sum)==0 & 
  (dat.vot$cit==1 | is.na(dat.vot$cit)) & 
  (dat.vot$regist=="registered" | is.na(dat.vot$regist))
dat.vot <- dat.vot[ok,]
dat.vot$year <- recode(dat.vot$file, "'2000-ann'=2000; '2004-ann'=2004; '2008-pew'=2008", as.factor.result=F)
dat.vot <- dat.vot[dat.vot$year != 2000,]
dat.vot$ones <- 1

#Build state data
dat.stt <- read.table("data/state-stats.dat", header=T, sep="\t")
#Remove National information
dat.stt <- dat.stt[-52,]
cutoffs <- quantile(dat.stt$inc2004, c(.2, .4, .6, .8))
cutoff_summary <- lapply(cutoffs, function(cutoff) {dat.stt$inc2004 > cutoff})
dat.stt$"inc2004_cat" <- (rowSums(as.data.frame(cutoff_summary)) + 1)


#Build ind income
setwd("~/Documents/Gelman Research/Red State Tutorial/Red State Tutorial/")
vote_2004_ind_inc_data <- summarize(group_by(dat.vot, inc), num_votes = sum(ones), rep_votes = sum(rvotes))
vote_2004_ind_inc_data$inc <- factor(vote_2004_ind_inc_data$inc)

ind_inc_2004_stan_obj <- stan_lmer(cbind(rep_votes, num_votes - rep_votes) ~ inc, data = vote_2004_ind_inc_data, file = "regression_by_inc.stan", family = "binomial")
stan_2004_ind_inc_data <- ind_inc_2004_stan_obj$data
save(stan_2004_ind_inc_data, file = "Data/stan_inc_regression_data.Rdata")

#Build state income
setwd("~/Documents/Gelman Research/Red State Tutorial/Red State Tutorial/")

vote_2004_state_inc_data <- summarize(group_by(dat.stt, inc2004_cat), 
                                      num_votes = sum(vote2004), 
                                      rep_votes = sum(rep2004 * vote2004))
vote_2004_state_inc_data <- rename(vote_2004_state_inc_data, inc = inc2004_cat)
vote_2004_state_inc_data$inc <- factor(vote_2004_state_inc_data$inc)

state_inc_2004_stan_obj <- stan_lmer(cbind(rep_votes, num_votes - rep_votes) ~ inc, data = vote_2004_ind_inc_data, file = "regression_by_inc.stan", family = "binomial")
stan_2004_state_inc_data <- state_inc_2004_stan_obj$data
save(stan_2004_ind_inc_data, file = "Data/stan_state_regression_data.Rdata")

#Build combined income
setwd("~/Documents/Gelman Research/Red State Tutorial/Red State Tutorial/")
dat.stt$stt <- 1:51
vote_2004_state_and_ind_inc_data <- merge(x = dat.vot[, c("stt", "rvote", "inc", "ones")], y = dat.stt[, c("stt", "inc2004_cat")], by = "stt", all.x = T)
vote_2004_state_and_ind_inc_data <- rename(vote_2004_state_and_ind_inc_data, ind_inc = inc, state_inc = inc2004_cat)
vote_2004_state_and_ind_inc_data$grp <- apply(vote_2004_state_and_ind_inc_data[, c("state_inc", "ind_inc")], 1, paste, collapse="_")
vote_2004_state_and_ind_inc_data <- summarize(group_by(vote_2004_state_and_ind_inc_data, grp), 
                                              rep_votes = sum(rvote), 
                                              num_votes = sum(ones), 
                                              state_inc = unique(state_inc), 
                                              ind_inc = unique(ind_inc))
vote_2004_state_and_ind_inc_data$ind_inc <- factor(vote_2004_state_and_ind_inc_data$ind_inc)
vote_2004_state_and_ind_inc_data$state_inc <- factor(vote_2004_state_and_ind_inc_data$state_inc)

state_and_ind_inc_2004_stan_obj <- stan_lmer(cbind(rep_votes, num_votes - rep_votes) ~ ind_inc + state_inc, data = vote_2004_ind_inc_data, file =  "Stan/regression_by_state_inc_and_ind_inc.stan", family = "binomial")
stan_2004_state_and_ind_inc_data <- state_and_ind_inc_2004_stan_obj$data
save(stan_2004_state_and_ind_inc_data, file = "Data/stan_state_inc_and_ind_inc_regression_data.Rdata")

#Build hierarchical model
vote_2004_state_and_inc_data <- dat.vot[, c("stt", "inc", "rvote", "ones")]
vote_2004_state_and_inc_data$grp <- apply(vote_2004_state_and_ind_inc_data[, c("stt", "inc")], 1, paste, collapse="_")
vote_2004_state_and_inc_data <- summarize(group_by(vote_2004_state_and_inc_data), 
                                          num_votes = sum(ones), 
                                          rep_votes = sum(rvote),
                                          stt = unique(stt),
                                          inc = unique(inc))
vote_2004_state_and_inc_data$inc <- factor(vote_2004_state_and_inc_data$inc)
vote_2004_state_and_inc_data$stt <- factor(vote_2004_state_and_inc_data$stt)

state_and_inc_2004_stan_obj <- stan_lmer(bind(rep_votes, num_votes - rep_votes) ~ inc + stt, data = vote_2004_state_and_inc_data, file =  "Stan/mixed_effects_regression_for_state_and_inc.stan", family = "binomial")
stan_2004_state_and_income_data <- state_and_inc_2004_stan_obj$data
save(stan_2004_state_and_income_data, file = "Data/stan_state_and_inc_regression_data.Rdata")

original_vote_2004_state <- vote_2004_state_and_inc_data$stt
translated_vote_2004_state <- stan_2004_state_and_income_data$stt
save(original_vote_2004_state, translated_vote_2004_state, file = "state_guide.Rdata")