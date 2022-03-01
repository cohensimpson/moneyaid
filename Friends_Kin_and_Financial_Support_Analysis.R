################################# The Relational Bases of Informal Finance
################################# Replication Code: Analysis


library(rio)
library(reshape2)

library(network)
library(sna)

library(brms)
# # install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))  # https://mc-stan.org/cmdstanr/
# library(cmdstanr)
# # install_cmdstan()
# set_cmdstan_path(path = "/Users/cohen/.cmdstanr/cmdstan-2.28.2")

library(ggplot2)
library(jtools)
library(broom)
library(broom.mixed)

library(bayesplot)
library(cowplot) # Used to create ggplots w/ insets

library(VIM) # Used to visualise missing data
library(sbgcop) # Used for the Bayesian copula imputation


set.seed(20200127)
options(scipen = 8)
options(digits = 5)
options(max.print = 5000000)


# writeLines(capture.output(devtools::session_info()), "sessionInfo_Friends_Kin_and_Financial_Support.txt")


#################################### LOAD NETWORK AND ATTRIBUTE DATA + CREATE DATAFRAME FOR MULTILEVEL MODELS ####################################  
source("Friends_Kin_and_Financial_Support_DataPrep.R")

## First, save the many objects containing the raw data, the transformed data, and the imputed data, for easy accesss if needed.
save.image("Friends_Kin_and_Financial_Support_DataPrep.RData")


## Next, keep ONLY what is needed to fit the Bayesian multilevel models.
rm(list = setdiff(ls(), list("all_village_dyads"))) 


## Last, use garbage collecctor to clear memory
gc()



#################################### SET NUMBER OF AVALIABLE COMPUTING CORES ####################################  
# https://cran.r-project.org/web/packages/brms/vignettes/brms_threading.html
# https://paul-buerkner.github.io/brms/articles/brms_threading.html
# Total Number of CPU Cores Utilized = (brms.cores * cmdstanr.threads). For my analysis I use 10 CPU cores.
brms.cores <- 2
brms.chains <- 2
cmdstanr.threads <- 5

brms.iter <- 13000 ## How many iterations per chain?
brms.warmup <- 1000 ## How many warm-up iterations per chain?

# cmdstanr.grainsize <- nrow(all_village_dyads)/120  # Default: max(100, nrow(all_village_dyads) / (2 * cmdstanr.threads))


#################################### ESTIMATE BAYESIAN MODELS ####################################
########## A NOTE ON INITIAL (i.e., STARTING) VALUES FOR THE HAMILTONIAN MONTE CARLO CHAINS
# After estimation begins, sometimes brms/STAN will produce rather the following rather intimidating warning
        # Chain XX Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
        # Chain XX Exception: Exception: bernoulli_logit_glm_lpmf: Intercept[1] is inf, but must be finite! (in '/var/folders/tx/jmss9q7j10b7ljypxm0sl0sr0000gn/T/RtmpKdTU1w/model-c58682cfdb0.stan', line 28, column 4 to column 76) (in '/var/folders/tx/jmss9q7j10b7ljypxm0sl0sr0000gn/T/RtmpKdTU1w/model-c58682cfdb0.stan', line 81, column 4 to column 129)
        # Chain XX If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
        # Chain XX but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

# Based on posts by the architects of STAN (i.e., the probabilistic programming language underyling brms/rstan), particularly
# this post (https://discourse.mc-stan.org/t/variable-does-not-exist-error-for-user-defined-function-in-ode-rk45/18960/7), this warning
# is not concerning if it only occurs rarely and only during the warmup phase.
# Still, in light of the recommendation in this post (https://discourse.mc-stan.org/t/model-with-many-correlated-varying-slopes-sampling-not-done/6189/6),
# I have reduced the default range of the randomly generated initial values from [-2, +2] to [-1, +1] to address this warning using the arguments "init"/"init_r". 
# For another example of this warning behaviour, see this post: https://discourse.mc-stan.org/t/partial-pooling-scale-parameter-is-inf/21149/2
# For an briefing on initial values, see this excellent post as well: https://solomonkurz.netlify.app/post/2021-06-05-don-t-forget-your-inits/

# https://discourse.mc-stan.org/t/metropolis-rejection-proposal-due-to-incorrect-numerical-values-for-derived-parameter/3654/4
# https://discourse.mc-stan.org/t/metropolis-proposal-rejected-because-location-parameter-is-infinite/6371/2
# https://discourse.mc-stan.org/t/speeding-up-a-student-model-with-3-correlation-matrices/20627/4
# https://discourse.mc-stan.org/t/compilation-error-on-rstan/17972/14
# https://discourse.mc-stan.org/t/location-parameter-is-nan-but-must-be-finite/11460/7
# https://discourse.mc-stan.org/t/help-ensure-the-shape-parameter-is-positive-finite/26313/4



########## Model 1
fit.1 <- brm(formula = brmsformula(formula = lender_ij 
                                   
                                   ~ 0 + Intercept
                                   
                                   + friend_ij + family_ij + friend_ij:family_ij
                                   
                                   + lender_ji
                                   
                                   + log_distance_ij_Z
                                   
                                   + goodsgame_ij
                                   + problemsolver_ij
                                   
                                   + gender_i
                                   + gender_j
                                   + same_gender_ij
                                   
                                   + age_i_Z + age_i_squared_Z
                                   + age_j_Z + age_j_squared_Z
                                   + age_absdiff_ij_sqrt_Z
                                   
                                   + edu_full_i + edu_full_j + same_edu_full_ij
                                   
                                   + income_i
                                   
                                   + hasPhone_i
                                   + hasPhone_j
                                   
                                   + leader_i
                                   + leader_j
                                   
                                   + HH_Head_i
                                   + HH_Head_j
                                   
                                   + religion_i
                                   + religion_j
                                   + same_religion_ij
                                   
                                   + ethnicity_i
                                   + ethnicity_j
                                   
                                   # https://twitter.com/dingding_peng/status/1243197216233336832
                                   # https://discourse.mc-stan.org/t/brms-formula-for-population-level-effects-measured-at-the-group-level/20953/3
                                   # https://discourse.mc-stan.org/t/brms-approach-to-stan-models-for-nested-versus-non-nested-multi-level-models/9278/11
                                   # https://stats.stackexchange.com/a/228814
                                   # https://www.muscardinus.be/2017/07/lme4-random-effects/
                                   
                                   + (1 | village) + (1 | village:i_ID) + (1 | village:j_ID)

                                   + village_population_size_log_Z
                                   + village_census_employ_Z
                                   + village_census_noAgri_Z
                                   + village_census_poverty_Z
                                   + village_pg_savingsgroup
                                   + village_pg_market_any
                                   + village_distArua_log_Z
                                   
                                   , family = bernoulli(link = "logit")
                                   , nl = NULL
                                   , center = FALSE
)
, prior = c(
  set_prior(prior = "student_t(6, 0, 1)", class = "sd", coef = "", group = "",  dpar = ""), # brms::dstudent_t(x = 1:100, df = 9, mu = 0, sigma = 2.5, log = FALSE)
  set_prior(prior = "normal(0, 2.5)", class = "b", coef = "Intercept", group = "",  dpar = ""), # normal(0, 2.5); student_t(7, 0, 2.5)
  set_prior(prior = "normal(0, 1)", class = "b", group = "",  dpar = "")
)
, data = all_village_dyads
, iter = brms.iter, warmup = brms.warmup, thin = 1
, cores = brms.cores, chains = brms.chains
, backend = "cmdstan"
, threads = threading(threads = cmdstanr.threads, grainsize = 5670, static = TRUE) # https://cran.r-project.org/web/packages/brms/vignettes/brms_threading.html
# Let Stan/brms generate random initial values for all parameters. 
# The seed of the random number generator used by Stan can be specified via the seed argument. 
# If the seed for Stan is fixed, the same initial values are used. The default is to randomly 
# generate initial values between -2 and 2 on the unconstrained support. 
# The optional additional parameter init_r can be set to some value other than 2 to change the range of the randomly generated inits.
# As the "cmdstan" backend is used, changing "init = "random"" to "init = 1" is equivalent to setting "init_r = 1"; To clarify, RUN: ??cmdstanr::sample
, init = 0.5
, seed = 20200127
, silent = 0
, save_pars = save_pars(all = TRUE)
, file = "fit.1.full.model", file_refit = "on_change"
)

gc()
closeAllConnections()


# robust: If FALSE (the default) the mean is used as the measure of central tendency and the standard deviation 
# as the measure of variability. If TRUE, the median and the median absolute deviation (MAD) are applied instead.
brms:::print.brmsfit(fit.1, digits = 4, prob = 0.95, priors = TRUE, robust = FALSE, mc_se = TRUE)



########## Model 2
fit.2 <- brm(formula = brmsformula(formula = lender_ij 
                                   
                                   ~ 0 + Intercept
                                   
                                   # + friend_ij + family_ij + friend_ij:family_ij
                                   
                                   + lender_ji
                                   
                                   + log_distance_ij_Z
                                   
                                   + goodsgame_ij
                                   + problemsolver_ij
                                   
                                   + gender_i
                                   + gender_j
                                   + same_gender_ij
                                   
                                   + age_i_Z + age_i_squared_Z
                                   + age_j_Z + age_j_squared_Z
                                   + age_absdiff_ij_sqrt_Z
                                   
                                   + edu_full_i + edu_full_j + same_edu_full_ij
                                   
                                   + income_i
                                   
                                   + hasPhone_i
                                   + hasPhone_j
                                   
                                   + leader_i
                                   + leader_j
                                   
                                   + HH_Head_i
                                   + HH_Head_j
                                   
                                   + religion_i
                                   + religion_j
                                   + same_religion_ij
                                   
                                   + ethnicity_i
                                   + ethnicity_j
                                   
                                   # https://twitter.com/dingding_peng/status/1243197216233336832
                                   # https://discourse.mc-stan.org/t/brms-formula-for-population-level-effects-measured-at-the-group-level/20953/3
                                   # https://discourse.mc-stan.org/t/brms-approach-to-stan-models-for-nested-versus-non-nested-multi-level-models/9278/11
                                   # https://stats.stackexchange.com/a/228814
                                   # https://www.muscardinus.be/2017/07/lme4-random-effects/
                                   
                                   + (1 | village) + (1 | village:i_ID) + (1 | village:j_ID)
                                   
                                   + village_population_size_log_Z
                                   + village_census_employ_Z
                                   + village_census_noAgri_Z
                                   + village_census_poverty_Z
                                   + village_pg_savingsgroup
                                   + village_pg_market_any
                                   + village_distArua_log_Z
                                   
                                   , family = bernoulli(link = "logit")
                                   , nl = NULL
                                   , center = FALSE
)
, prior = c(
  set_prior(prior = "student_t(6, 0, 1)", class = "sd", coef = "", group = "",  dpar = ""), # brms::dstudent_t(x = 1:100, df = 9, mu = 0, sigma = 2.5, log = FALSE)
  set_prior(prior = "normal(0, 2.5)", class = "b", coef = "Intercept", group = "",  dpar = ""), # normal(0, 2.5); student_t(7, 0, 2.5)
  set_prior(prior = "normal(0, 1)", class = "b", group = "",  dpar = "")
)
, data = all_village_dyads
, iter = brms.iter, warmup = brms.warmup, thin = 1
, cores = brms.cores, chains = brms.chains
, backend = "cmdstan"
, threads = threading(threads = cmdstanr.threads, grainsize = 5670, static = TRUE) # https://cran.r-project.org/web/packages/brms/vignettes/brms_threading.html
# Let Stan/brms generate random initial values for all parameters. 
# The seed of the random number generator used by Stan can be specified via the seed argument. 
# If the seed for Stan is fixed, the same initial values are used. The default is to randomly 
# generate initial values between -2 and 2 on the unconstrained support. 
# The optional additional parameter init_r can be set to some value other than 2 to change the range of the randomly generated inits.
# As the "cmdstan" backend is used, changing "init = "random"" to "init = 1" is equivalent to setting "init_r = 1"; To clarify, RUN: ??cmdstanr::sample
, init = 0.5
, seed = 20200127
, silent = 0
, save_pars = save_pars(all = TRUE)
, file = "fit.2.controls.model", file_refit = "on_change"
)

gc()
closeAllConnections()

# robust: If FALSE (the default) the mean is used as the measure of central tendency and the standard deviation 
# as the measure of variability. If TRUE, the median and the median absolute deviation (MAD) are applied instead.
brms:::print.brmsfit(fit.2, digits = 4, prob = 0.95, priors = TRUE, robust = FALSE, mc_se = TRUE)



######## Diagnostics for Pareto Smoothed Importance Sampling Leave-one-out Cross Validation or PSIS-LOO
# Summarise the estimated Pareto shape parameters and PSIS effective sample sizes and 
# find the index of observations for which the estimated Pareto shape parameter k
# is greater than 0.5. This is akin to checking for outliers/influential observations.
# https://discourse.mc-stan.org/t/loo-package/3098/17
# https://discourse.mc-stan.org/t/pareto-k-diagnostics-and-kfold-model-comparison/7409
# https://discourse.mc-stan.org/t/a-quick-note-what-i-infer-from-p-loo-and-pareto-k-values/3446
# https://discourse.mc-stan.org/t/recommendations-for-what-to-do-when-k-exceeds-0-5-in-the-loo-package/3417
# https://discourse.mc-stan.org/t/using-loo-with-brms/6736


# Add LOO and Bayes R2 to the Model
# https://paul-buerkner.github.io/brms/reference/add_criterion.html
# https://discourse.mc-stan.org/t/questions-about-approximate-leave-one-out-validation-for-large-models/24898/8
# https://mc-stan.org/loo/reference/index.html
# https://paul-buerkner.github.io/brms/reference/loo.brmsfit.html

# https://paul-buerkner.github.io/brms/reference/loo_subsample.brmsfit.html
# https://mc-stan.org/loo/reference/loo_subsample.html


# https://mc-stan.org/loo/reference/loo-glossary.html
# https://discourse.mc-stan.org/t/how-bad-is-a-small-percentage-of-very-bad-pareto-k-diagnostic-of-loo-package/8504/2
# https://avehtari.github.io/bayes_R2/bayes_R2.html
# https://discourse.mc-stan.org/t/questions-about-approximate-leave-one-out-validation-for-large-models/24898/9

gc()

fit.1 <- add_criterion( 
  x = fit.1,
  criterion = c("loo", "bayes_R2"), # https://discourse.mc-stan.org/t/r2-vs-bayes-r2/12788/9
  ndraws = 1000,
  cores = 10,
  pointwise = FALSE,
  save_psis = TRUE,
  overwrite = TRUE
)
gc()

fit.1$criteria$loo
mean(fit.1$criteria$bayes_R2)
hist(fit.1$criteria$bayes_R2)

gc()

fit.2 <- add_criterion( 
  x = fit.2,
  criterion = c("loo", "bayes_R2"), # https://discourse.mc-stan.org/t/r2-vs-bayes-r2/12788/9
  ndraws = 1000,
  cores = 10,
  pointwise = FALSE,
  save_psis = TRUE,
  overwrite = TRUE
)
gc()

fit.2$criteria$loo
mean(fit.2$criteria$bayes_R2)
hist(fit.2$criteria$bayes_R2)

gc()


# https://avehtari.github.io/modelselection/CV-FAQ.html#15_How_to_interpret_in_Standard_error_(SE)_of_elpd_difference_(elpd_diff)
# https://discourse.mc-stan.org/t/clarifying-interpretation-of-loo-compare-output/19726/2
fit.1.vs.fit.2.loo <- loo_compare(fit.1, fit.2, criterion = "loo") 
print(fit.1.vs.fit.2.loo)

# https://discourse.mc-stan.org/t/if-elpd-diff-se-diff-2-is-this-noteworthy/20549/10
# https://discourse.mc-stan.org/t/if-elpd-diff-se-diff-2-is-this-noteworthy/20549/12
fit.1.vs.fit.2.loo[, "elpd_diff"][2] + (c(-1, 1) * 2 * fit.1.vs.fit.2.loo[, "se_diff"][2]) # Approximate 95% Interval for elpd_diff




#################################### FIGURE 1: COEFFICIENT PLOT ####################################
# https://jtools.jacob-long.com/reference/plot_summs.html
coefficient.plot <- plot_coefs(fit.1, fit.2,
                               ci_level = 0.95,
                               inner_ci_level = 0.68,
                               # conf.method = "quantile", ## Using this options eems to break plot_coefs(). May be related to broom/broom.mixed clashing.
                               model.names = c(paste0("Full (Model 1)\nBayes R^2 = ",
                                                      sprintf("%.3f", mean(fit.1$criteria$bayes_R2)),
                                                      "\n95% CI [", 
                                                      sprintf("%.3f", quantile(fit.1$criteria$bayes_R2, probs = c(0.025, 0.975)))[1],
                                                      ", ",
                                                      sprintf("%.3f", quantile(fit.1$criteria$bayes_R2, probs = c(0.025, 0.975)))[2],
                                                      "]"
                                                      ),
                                               paste0("Controls (Model 2)\nBayes R^2 = ",
                                                      sprintf("%.3f", mean(fit.2$criteria$bayes_R2)),
                                                      "\n95% CI [", 
                                                      sprintf("%.3f", quantile(fit.2$criteria$bayes_R2, probs = c(0.025, 0.975)))[1],
                                                      ", ",
                                                      sprintf("%.3f", quantile(fit.2$criteria$bayes_R2, probs = c(0.025, 0.975)))[2],
                                                      "]"
                                                      )
                                               ),
                               coefs = c( # "Intercept" = "(Intercept)", # Exclude as it skews the graph
                                          "Best Friend of Ego" = "friend_ijBestFriend",
                                          "Salient Kin of Ego" = "family_ijSalientKin",
                                          "Best Friend of Ego x Salient Kin of Ego" = "friend_ijBestFriend:family_ijSalientKin",
                                          "Money Lender for Alter" = "lender_ji",
                                          "Geographic Distance Between Ego and Alter (Log)" = "log_distance_ij_Z",
                                          "Preferred Money Handler of Ego" = "goodsgame_ijPreferredMoneyHandler",
                                          "Preferred Problem Solver of Ego" = "problemsolver_ijPreferredProblemSolver",
                                          "Gender (Ego): Female" = "gender_iFemale",
                                          "Gender (Alter): Female" = "gender_jFemale",
                                          "Same Gender (Ego & Alter)" = "same_gender_ijSameGender",
                                          "Age (Ego)" = "age_i_Z",
                                          "Age^2 (Ego)" = "age_i_squared_Z",
                                          "Age (Alter)" = "age_j_Z",
                                          "Age^2 (Alter)" = "age_j_squared_Z",
                                          "Absolute Difference (Sqrt): Age (Ego) - Age (Alter)" = "age_absdiff_ij_sqrt_Z",
                                          "Education Level (Ego)" = "edu_full_i",
                                          "Education Level (Alter)" = "edu_full_j",
                                          "Same Education Level (Ego & Alter)" = "same_edu_full_ijSameLevelofEducation",
                                          "Perceived Relative Income (Ego)" = "income_i",
                                          "Mobile Phone Owner (Ego): Yes" = "hasPhone_iYes",
                                          "Mobile Phone Owner (Alter): Yes" = "hasPhone_jYes",
                                          "Household Head (Ego): Yes" = "HH_Head_iYes",
                                          "Household Head (Alter): Yes" = "HH_Head_jYes",
                                          "Religion (Ego): Not Catholic (Dominant Religion)" = "religion_iNotCatholic",
                                          "Religion (Alter): Not Catholic (Dominant Religion)" = "religion_jNotCatholic",
                                          "Same Religion (Ego & Alter)" = "same_religion_ijSameReligion",
                                          "Ethnicity (Ego): Not Lugbara (Dominant Ethnicity)" = "ethnicity_iNotLugbara",
                                          "Ethnicity (Alter): Not Lugbara (Dominant Ethnicity)" = "ethnicity_jNotLugbara",
                                          "Village Leader (Ego): Yes" = "leader_iYes",
                                          "Village Leader (Alter): Yes" = "leader_jYes",
                                          "Village Population Size (Log)" = "village_population_size_log_Z",
                                          "Village Employement Rate" = "village_census_employ_Z",
                                          "Village %Employed in Non-Agricultural Work" = "village_census_noAgri_Z",
                                          "Village Poverty Rate" = "village_census_poverty_Z",
                                          "Village Savings Groups" = "village_pg_savingsgroup",
                                          "Village Market: General or Farmers (Ref: No Market)" = "village_pg_market_anyGeneralorFarmersMarket",
                                          "Village Distance to Arua (Market City)" = "village_distArua_log_Z"
                                          
                               ),
                               omit.coefs = NULL,
                               colors = "CUD Bright", # c("#ff9233", "#e85e5e"),
                               plot.distributions = FALSE,
                               exp = FALSE,
                               point.shape = TRUE,
                               legend.title = NULL 
                               
)  + labs(title = NULL,
          y = NULL,
          x = expression(Posterior~Mean~Log~Odds~Ratio~of~italic(Y)[ijk][" = "][1][" = "][Money~Lender][" (i = Ego, j = Alter, k = Village)"]~plain("+ 95% Quantile Interval [68% Inner]")), # https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html
          caption = paste0("\n", # http://paul-buerkner.github.io/brms/reference/VarCorr.brmsfit.html
                           "SD of Varying Intercepts for Village (Levels = 16) - ",
                           "Model 1 (",
                           "Estimate = ", sprintf("%.3f", VarCorr(x = fit.1, robust = FALSE, probs = c(0.025, 0.975))$village$sd["Intercept", "Estimate"]),
                           "; S.E. = ",  sprintf("%.3f", VarCorr(x = fit.1, robust = FALSE, probs = c(0.025, 0.975))$village$sd["Intercept", "Est.Error"]),
                           "; 95% CI = [", paste0( sprintf("%.3f", VarCorr(x = fit.1, robust = FALSE, probs = c(0.025, 0.975))$village$sd["Intercept", "Q2.5"]),
                                                   ", ",
                                                   sprintf("%.3f", VarCorr(x = fit.1, robust = FALSE, probs = c(0.025, 0.975))$village$sd["Intercept", "Q97.5"]),
                                                   "])"
                           ),
                           "; Model 2 (",
                           "Estimate = ", sprintf("%.3f", VarCorr(x = fit.2, robust = FALSE, probs = c(0.025, 0.975))$village$sd["Intercept", "Estimate"]),
                           "; S.E. = ",  sprintf("%.3f", VarCorr(x = fit.2, robust = FALSE, probs = c(0.025, 0.975))$village$sd["Intercept", "Est.Error"]),
                           "; 95% CI = [", paste0( sprintf("%.3f", VarCorr(x = fit.2, robust = FALSE, probs = c(0.025, 0.975))$village$sd["Intercept", "Q2.5"]),
                                                   ", ",
                                                   sprintf("%.3f", VarCorr(x = fit.2, robust = FALSE, probs = c(0.025, 0.975))$village$sd["Intercept", "Q97.5"]),
                                                   "]"
                           ),
                           "\n",
                           "SD of Varying Intercepts for Village:Ego (Levels = 3,184) - ",
                           "Model 1 (",
                           "Estimate = ", sprintf("%.3f", VarCorr(x = fit.1, robust = FALSE, probs = c(0.025, 0.975))$`village:i_ID`$sd["Intercept", "Estimate"]),
                           "; S.E. = ",  sprintf("%.3f", VarCorr(x = fit.1, robust = FALSE, probs = c(0.025, 0.975))$`village:i_ID`$sd["Intercept", "Est.Error"]),
                           "; 95% CI = [", paste0( sprintf("%.3f", VarCorr(x = fit.1, robust = FALSE, probs = c(0.025, 0.975))$`village:i_ID`$sd["Intercept", "Q2.5"]),
                                                   ", ",
                                                   sprintf("%.3f", VarCorr(x = fit.1, robust = FALSE, probs = c(0.025, 0.975))$`village:i_ID`$sd["Intercept", "Q97.5"]),
                                                   "])"
                           ),
                           "; Model 2 (",
                           "Estimate = ", sprintf("%.3f", VarCorr(x = fit.2, robust = FALSE, probs = c(0.025, 0.975))$`village:i_ID`$sd["Intercept", "Estimate"]),
                           "; S.E. = ",  sprintf("%.3f", VarCorr(x = fit.2, robust = FALSE, probs = c(0.025, 0.975))$`village:i_ID`$sd["Intercept", "Est.Error"]),
                           "; 95% CI = [", paste0( sprintf("%.3f", VarCorr(x = fit.2, robust = FALSE, probs = c(0.025, 0.975))$`village:i_ID`$sd["Intercept", "Q2.5"]),
                                                   ", ",
                                                   sprintf("%.3f", VarCorr(x = fit.2, robust = FALSE, probs = c(0.025, 0.975))$`village:i_ID`$sd["Intercept", "Q97.5"]),
                                                   "]"
                           ),
                           "\n",
                           "SD of Varying Intercepts for Village:Alter (Levels = 3,184) - ",
                           "Model 1 (",
                           "Estimate = ", sprintf("%.3f", VarCorr(x = fit.1, robust = FALSE, probs = c(0.025, 0.975))$`village:j_ID`$sd["Intercept", "Estimate"]),
                           "; S.E. = ",  sprintf("%.3f", VarCorr(x = fit.1, robust = FALSE, probs = c(0.025, 0.975))$`village:j_ID`$sd["Intercept", "Est.Error"]),
                           "; 95% CI = [", paste0( sprintf("%.3f", VarCorr(x = fit.1, robust = FALSE, probs = c(0.025, 0.975))$`village:j_ID`$sd["Intercept", "Q2.5"]),
                                                   ", ",
                                                   sprintf("%.3f", VarCorr(x = fit.1, robust = FALSE, probs = c(0.025, 0.975))$`village:j_ID`$sd["Intercept", "Q97.5"]),
                                                   "])"
                           ),
                           "; Model 2 (",
                           "Estimate = ", sprintf("%.3f", VarCorr(x = fit.2, robust = FALSE, probs = c(0.025, 0.975))$`village:j_ID`$sd["Intercept", "Estimate"]),
                           "; S.E. = ",  sprintf("%.3f", VarCorr(x = fit.2, robust = FALSE, probs = c(0.025, 0.975))$`village:j_ID`$sd["Intercept", "Est.Error"]),
                           "; 95% CI = [", paste0( sprintf("%.3f", VarCorr(x = fit.2, robust = FALSE, probs = c(0.025, 0.975))$`village:j_ID`$sd["Intercept", "Q2.5"]),
                                                   ", ",
                                                   sprintf("%.3f", VarCorr(x = fit.2, robust = FALSE, probs = c(0.025, 0.975))$`village:j_ID`$sd["Intercept", "Q97.5"]),
                                                   "]"
                           )


          )
) + theme_nice(style = "black") + theme(axis.line = element_line(color = "black"),
                                        legend.background = element_rect(fill = "transparent", colour = "transparent"), # get rid of legend bg
                                        legend.box.background = element_rect(fill = "transparent", colour = "transparent"), # get rid of legend panel bg
                                        legend.key = element_rect(fill = "transparent", colour = "transparent"),
                                        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot,
                                        panel.background = element_rect(fill = "transparent"), # bg of the panel
                                        panel.grid.major.y = element_blank(),
                                        panel.grid.minor.y = element_blank(),
                                        panel.grid.major.x = element_line(size = 0.35, linetype = 1),
                                        panel.grid.minor.x = element_blank(),
                                        panel.border = element_blank(),
                                        axis.text = element_text(size = 10),
                                        axis.title = element_text(size = 10),
                                        legend.position = "right",
                                        legend.title = element_blank()
) 
 

# plot(coefficient.plot)
ggsave(plot = coefficient.plot, 
       filename = "F1_Friends_Kin_FinancialSupport.png", device = "png", dpi = 1200,
       scale = 3, width = 4, height = 4, units = "in", bg = "transparent")





#################################### FIGURE 2: CONDITIONAL EFFECTS ####################################
typical_dyad <- rbind.data.frame(  
  "Ego (Female) - Alter (Female)" = cbind.data.frame(lender_ji = 0,
                                                     log_distance_ij_Z = 0,  ## Note the standardization/use of Z scores!
                                                     goodsgame_ij = "Not Preferred Money Handler",
                                                     problemsolver_ij = "Not Preferred Problem Solver",
                                                     gender_i = "Female",
                                                     gender_j = "Female",
                                                     same_gender_ij = "Same Gender",
                                                     age_i_Z = 0,
                                                     age_i_squared_Z = 0,
                                                     age_j_Z = 0,
                                                     age_j_squared_Z = 0,
                                                     age_absdiff_ij_sqrt_Z = min(all_village_dyads$age_absdiff_ij_sqrt_Z), ## We want two individuals of the SAME AGE, thus NO Difference. age_absdiff_ij_sqrt_Z == 0 == Mean sqrt(age difference)!
                                                     edu_full_i = 1,
                                                     edu_full_j = 1,
                                                     same_edu_full_ij = "Same Level of Education",
                                                     income_i = 0,
                                                     hasPhone_i = "Yes",
                                                     hasPhone_j = "Yes",
                                                     leader_i = "No",
                                                     leader_j = "No",
                                                     HH_Head_i = "No",
                                                     HH_Head_j = "No",
                                                     religion_i = "Catholic",
                                                     religion_j = "Catholic",
                                                     same_religion_ij = "Same Religion",
                                                     ethnicity_i = "Lugbara",
                                                     ethnicity_j = "Lugbara",
                                                     village_population_size_log_Z = 0,
                                                     village_census_employ_Z = 0,
                                                     village_census_noAgri_Z = 0,
                                                     village_census_poverty_Z = 0,
                                                     village_pg_savingsgroup = mean(all_village_dyads$village_pg_savingsgroup),
                                                     village_pg_market_any = "No Market",
                                                     village_distArua_log_Z = 0
                                                     
  ),
  
  "Ego (Male) - Alter (Male)" = cbind.data.frame(lender_ji = 0,
                                                 log_distance_ij_Z = 0,
                                                 goodsgame_ij = "Not Preferred Money Handler",
                                                 problemsolver_ij = "Not Preferred Problem Solver",
                                                 gender_i = "Male",
                                                 gender_j = "Male",
                                                 same_gender_ij = "Same Gender",
                                                 age_i_Z = 0,
                                                 age_i_squared_Z = 0,
                                                 age_j_Z = 0,
                                                 age_j_squared_Z = 0,
                                                 age_absdiff_ij_sqrt_Z = min(all_village_dyads$age_absdiff_ij_sqrt_Z), ## We want two individuals of the SAME AGE, thus NO Difference. age_absdiff_ij_sqrt_Z == 0 == Mean sqrt(age difference)!
                                                 edu_full_i = 1,
                                                 edu_full_j = 1,
                                                 same_edu_full_ij = "Same Level of Education",
                                                 income_i = 0,
                                                 hasPhone_i = "Yes",
                                                 hasPhone_j = "Yes",
                                                 leader_i = "No",
                                                 leader_j = "No",
                                                 HH_Head_i = "No",
                                                 HH_Head_j = "No",
                                                 religion_i = "Catholic",
                                                 religion_j = "Catholic",
                                                 same_religion_ij = "Same Religion",
                                                 ethnicity_i = "Lugbara",
                                                 ethnicity_j = "Lugbara",
                                                 village_population_size_log_Z = 0,
                                                 village_census_employ_Z = 0,
                                                 village_census_noAgri_Z = 0,
                                                 village_census_poverty_Z = 0,
                                                 village_pg_savingsgroup = mean(all_village_dyads$village_pg_savingsgroup),
                                                 village_pg_market_any = "No Market",
                                                 village_distArua_log_Z = 0
  ),
  
  "Ego (Male) - Alter (Female)" = cbind.data.frame(lender_ji = 0,
                                                   log_distance_ij_Z = 0,
                                                   goodsgame_ij = "Not Preferred Money Handler",
                                                   problemsolver_ij = "Not Preferred Problem Solver",
                                                   gender_i = "Male",
                                                   gender_j = "Female",
                                                   same_gender_ij = "Different Gender",
                                                   age_i_Z = 0,
                                                   age_i_squared_Z = 0,
                                                   age_j_Z = 0,
                                                   age_j_squared_Z = 0,
                                                   age_absdiff_ij_sqrt_Z = min(all_village_dyads$age_absdiff_ij_sqrt_Z), ## We want two individuals of the SAME AGE, thus NO Difference. age_absdiff_ij_sqrt_Z == 0 == Mean sqrt(age difference)!
                                                   edu_full_i = 1,
                                                   edu_full_j = 1,
                                                   same_edu_full_ij = "Same Level of Education",
                                                   income_i = 0,
                                                   hasPhone_i = "Yes",
                                                   hasPhone_j = "Yes",
                                                   leader_i = "No",
                                                   leader_j = "No",
                                                   HH_Head_i = "No",
                                                   HH_Head_j = "No",
                                                   religion_i = "Catholic",
                                                   religion_j = "Catholic",
                                                   same_religion_ij = "Same Religion",
                                                   ethnicity_i = "Lugbara",
                                                   ethnicity_j = "Lugbara",
                                                   village_population_size_log_Z = 0,
                                                   village_census_employ_Z = 0,
                                                   village_census_noAgri_Z = 0,
                                                   village_census_poverty_Z = 0,
                                                   village_pg_savingsgroup = mean(all_village_dyads$village_pg_savingsgroup),
                                                   village_pg_market_any = "No Market",
                                                   village_distArua_log_Z = 0
  ),
  
  "Ego (Female) - Alter (Male)" = cbind.data.frame(lender_ji = 0,
                                                   log_distance_ij_Z = 0,
                                                   goodsgame_ij = "Not Preferred Money Handler",
                                                   problemsolver_ij = "Not Preferred Problem Solver",
                                                   gender_i = "Female",
                                                   gender_j = "Male",
                                                   same_gender_ij = "Different Gender",
                                                   age_i_Z = 0,
                                                   age_i_squared_Z = 0,
                                                   age_j_Z = 0,
                                                   age_j_squared_Z = 0,
                                                   age_absdiff_ij_sqrt_Z = min(all_village_dyads$age_absdiff_ij_sqrt_Z), ## We want two individuals of the SAME AGE, thus NO Difference. age_absdiff_ij_sqrt_Z == 0 == Mean sqrt(age difference)!
                                                   edu_full_i = 1,
                                                   edu_full_j = 1,
                                                   same_edu_full_ij = "Same Level of Education",
                                                   income_i = 0,
                                                   hasPhone_i = "Yes",
                                                   hasPhone_j = "Yes",
                                                   leader_i = "No",
                                                   leader_j = "No",
                                                   HH_Head_i = "No",
                                                   HH_Head_j = "No",
                                                   religion_i = "Catholic",
                                                   religion_j = "Catholic",
                                                   same_religion_ij = "Same Religion",
                                                   ethnicity_i = "Lugbara",
                                                   ethnicity_j = "Lugbara",
                                                   village_population_size_log_Z = 0,
                                                   village_census_employ_Z = 0,
                                                   village_census_noAgri_Z = 0,
                                                   village_census_poverty_Z = 0,
                                                   village_pg_savingsgroup = mean(all_village_dyads$village_pg_savingsgroup),
                                                   village_pg_market_any = "No Market",
                                                   village_distArua_log_Z = 0
  )
)



condeff_plot <- conditional_effects(x = fit.1, 
                                    method = "posterior_epred",
                                    effects = c("friend_ij:family_ij"),
                                    conditions = typical_dyad,
                                    categorical = FALSE, 
                                    re_formula = NA,
                                    prob = 0.95,
                                    resolution = 1000,
                                    cores = 1,
                                    plot = FALSE
)


# Modify factor labels in data frame underlying plot constructed with conditional_effects()
# https://predictivehacks.com/rename-and-relevel-factors-in-r/
# levels(condeff_plot$`friend_ij:family_ij`$effect1__)[levels(condeff_plot$`friend_ij:family_ij`$effect1__) == "Friend"] <- "Best Friend"
# levels(condeff_plot$`friend_ij:family_ij`$effect2__)[levels(condeff_plot$`friend_ij:family_ij`$effect2__) == "Close Kin"] <- "Salient Kin"

# Construct Plot
condeff_plot_object <- brms:::plot.brms_conditional_effects(condeff_plot, facet_args = list("ncol" = 2), theme = theme_nice(style = "black"), plot = FALSE)
condeff_plot_object <- condeff_plot_object$`friend_ij:family_ij`
condeff_plot_object <- (condeff_plot_object + theme(axis.line = element_line(color = "black"),
                                                    legend.background = element_rect(fill = "transparent", colour = "transparent"), # get rid of legend bg
                                                    legend.box.background = element_rect(fill = "transparent", colour = "transparent"), # get rid of legend panel bg
                                                    legend.key = element_rect(fill = "transparent", colour = "transparent"),
                                                    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot,
                                                    panel.background = element_rect(fill = "transparent"), # bg of the panel
                                                    panel.grid.major = element_blank(),
                                                    panel.grid.minor = element_blank(),
                                                    panel.border = element_blank(),
                                                    axis.text = element_text(size = 10),
                                                    axis.title = element_text(size = 10),
                                                    legend.position = "bottom",
                                                    legend.title = element_blank()
) 
+ labs(title = NULL, x = NULL, y = expression(Predicted~Probability~of~italic(Y)[ijk][" = "][1][" = "][Money~Lender]))
+ scale_y_continuous(limits = c(0, 0.175), breaks = seq(0, 0.175, 0.025))
+ scale_color_manual(values = jtools::get_colors("CUD Bright", num.colors = 2))
+ guides(fill = "none") # https://statisticsglobe.com/remove-legend-ggplot2-r
) 

ggsave(plot = condeff_plot_object, 
       filename = "F2_Friends_Kin_FinancialSupport.png", device = "png", dpi = 1200,
       scale = 2.75, width = 2.25, height = 2.25, units = "in", bg = "transparent")




#################################### FIGURE 3: DIFFERENCE IN EXPECTED LOG POINTWISE PREDICTIVE DENSITIES ####################################
# https://discourse.mc-stan.org/t/group-level-vs-individual-level-model-comparison/8289/6
# Following Code by: Gabry, J., Simpson, D., Vehtari, A., Betancourt, M., & Gelman, A. (2019). Visualization in Bayesian Workflow. Journal of the Royal Statistical Society: Series A (Statistics in Society), 182(2), 389–402. https://doi.org/10.1111/rssa.12378
# Gabry et al. GitHub: https://github.com/jgabry/bayes-vis-paper/blob/master/bayes-vis.R#L489

elpdi1 <- fit.1$criteria$loo$pointwise[, "elpd_loo"] # Full Model
elpdi2 <- fit.2$criteria$loo$pointwise[, "elpd_loo"] # No Friendship & No Kinship

elpd_diffs <- data.frame(
  i_ID = fit.1$data$i_ID,
  j_ID = fit.1$data$i_ID,
  village = fit.1$data$village,
  diff12 = elpdi1 - elpdi2,
  Index = 1:nrow(fit.1$data)
)


elpd_diff_plot <- (ggplot(elpd_diffs, aes(x = Index, y = diff12)) 
                   + geom_point(aes(colour = factor(village)), size = 1, alpha = 0.8) # http://alumni.media.mit.edu/~wad/color/palette.html
                   + scale_color_manual(values = jtools::get_colors(colors = "Rainbow", num.colors = 64, reverse = FALSE, gradient = FALSE)[seq(1, 64, 4)][c(1, 2, 4, 3, 5:16)] )
                   # + scale_color_manual(values = c("7"  = "#000000",
                   #                                 "9"  = "#575757",
                   #                                 "11" = "#ad2323",
                   #                                 "14" = "#49b7fc", #"#2a4bd7"
                   #                                 "29" = "#1d6914",
                   #                                 "40" = "#814a19",
                   #                                 "46" = "#8126c0",
                   #                                 "47" = "#a0a0a0",
                   #                                 "50" = "#81c57a",
                   #                                 "51" = "#9dafff",
                   #                                 "55" = "#29d0d0",
                   #                                 "66" = "#ff7b00", #"#ff9233"
                   #                                 "75" = "#ffee33",
                   #                                 "77" = "#e9debb",
                   #                                 "80" = "#ffcdf3",
                   #                                 "81" = "#e85e5e"
                   # )
                   # )
                   + geom_hline(yintercept = 0, linetype = 2, size = 0.25)
                   + ylab(expression(ELPD[i~(Model~1)] - ELPD[i~(Model~2)])) # https://www.dataanalytics.org.uk/axis-labels-in-r-plots-using-expression/
                   + ylim(c(-5, 5)) 
                   + theme_nice(style = "black")
                   + theme(axis.line = element_line(color = "black"),
                           legend.background = element_rect(fill = "transparent", colour = "transparent"), # get rid of legend bg
                           legend.box.background = element_rect(fill = "transparent", colour = "transparent"), # get rid of legend panel bg
                           legend.key = element_rect(fill = "transparent", colour = "transparent"),
                           plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot,
                           panel.background = element_rect(fill = "transparent"), # bg of the panel
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(),
                           panel.border = element_blank(),
                           axis.text = element_text(size = 10),
                           axis.title = element_text(size = 10),
                           legend.position = "none"
                   )
                   # + annotate("text",
                   #            x = outlier_ids,
                   #            y = elpd_diffs$diff23[outlier_ids],
                   #            label = outlier_labs, 
                   #            size = 4
                   #            )
) + labs(title = NULL,
         x = "Dyad Index")


# plot(elpd_diff_plot)
ggsave(plot = elpd_diff_plot, 
       filename = "F3_Friends_Kin_FinancialSupport.png", device = "png", dpi = 1200,
       scale = 2.75, width = 2.25, height = 2, units = "in", bg = "transparent")




#################################### POSTERIOR PREDICTIVE CHECKS ####################################
## Overall/Unconditional Probability of Seeking Financial Aid Pr(y = 1) 
## Comparison of the proportion of 1s in the data vs the proportions of 1s in the posterior predictive distribution
## See: # https://discourse.mc-stan.org/t/simple-ppc-for-logistic-regression/17749/7

# The posterior predictive distribution is the distribution of the outcome implied by the model after 
# using the observed data to update our beliefs about the unknown parameters in the model. Simulating 
# data from the posterior predictive distribution using the observed predictors is useful for checking
# the fit of the model. Can be performed for the data used to fit the model (posterior predictive checks) 
# or for new data. By definition, these draws have higher variance than draws of the means of the posterior 
# predictive distribution computed by posterior_epred.brmsfit. This is because the residual error is 
# incorporated in posterior_predict.

# In univariate models, the output of posterior_predict() is as an S x N matrix, where S is the 
# number of posterior draws and N is the number of observations. 
# https://mc-stan.org/rstanarm/reference/posterior_predict.stanreg.html

# For those who are new to PP Checks, see this step-by-step guide: https://mc-stan.org/bayesplot/articles/graphical-ppcs.html

fit.1.pp <- posterior_predict(fit.1, newdata = NULL, re_formula = NULL, ndraws = 1000, cores = 10, summary = FALSE)
fit.2.pp <- posterior_predict(fit.2, newdata = NULL, re_formula = NULL, ndraws = 1000, cores = 10, summary = FALSE)

# https://mc-stan.org/bayesplot/reference/PPC-test-statistics.html
fit.1.pp.stat.grouped <- ppc_stat_grouped(
  y = all_village_dyads$lender_ij,
  yrep = fit.1.pp,
  group = factor(all_village_dyads$village, levels = as.character( sort( as.numeric( levels(all_village_dyads$village) ) ) ) ), 
  stat = "mean",
  facet_args = list(),
  binwidth = 0.0001,
  breaks = NULL,
  freq = TRUE
)

fit.2.pp.stat.grouped <- ppc_stat_grouped(
  y = all_village_dyads$lender_ij,
  yrep = fit.2.pp,
  group = factor(all_village_dyads$village, levels = as.character( sort( as.numeric( levels(all_village_dyads$village) ) ) ) ), 
  stat = "mean",
  facet_args = list(),
  binwidth = 0.0001,
  breaks = NULL,
  freq = TRUE
)

fit.1.pp.stat.grouped + theme(text = element_text(family = "Arial", size = 14)) + labs(title = expression("(Model 1)"~Expected~Unconditional~Probability~of~italic(Y)[ijk][" = "][1][" = "][Money~Lender])) # https://stackoverflow.com/a/43010647
fit.2.pp.stat.grouped + theme(text = element_text(family = "Arial", size = 14)) + labs(title = expression("(Model 2)"~Expected~Unconditional~Probability~of~italic(Y)[ijk][" = "][1][" = "][Money~Lender]))


fit.1.and.fit.2.pp.mean <- rbind.data.frame(cbind.data.frame(Model = "Full (Model 1)", Y_Synth_Mean = rowMeans(fit.1.pp) ),
                                            cbind.data.frame(Model = "Controls (Model 2)", Y_Synth_Mean = rowMeans(fit.2.pp) )
)


fit.1.fit.2.pp.stat.custom <- (ggplot(fit.1.and.fit.2.pp.mean, aes(x = Y_Synth_Mean, color = Model, fill = Model)) 
                               + geom_histogram(binwidth = 0.00001, alpha = 0.30, size = 0.25, position = "identity") # https://nkha149.github.io/stat385-sp2020/files/notes/html/graphics-p4.html
                               # + geom_vline(xintercept = tapply(X = fit.1.and.fit.2.pp.mean$Y_Synth_Mean, INDEX = fit.1.and.fit.2.pp.mean$Model, FUN = mean)["Full (Model 1)"], linetype = "dashed", color = jtools::get_colors("CUD Bright", num.colors = 2, reverse = TRUE)[1])
                               # + geom_vline(xintercept = tapply(X = fit.1.and.fit.2.pp.mean$Y_Synth_Mean, INDEX = fit.1.and.fit.2.pp.mean$Model, FUN = mean)["Controls (Model 2)"], linetype = "dashed", color = jtools::get_colors("CUD Bright", num.colors = 2, reverse = TRUE)[2]) 
                               + geom_vline(xintercept = mean(all_village_dyads$lender_ij), linetype = 2, size = 0.25) 
                               + theme_nice(style = "black", base_rect_size = 0)
                               + theme(axis.line = element_line(color = "black"),
                                       legend.background = element_rect(fill = "transparent", colour = "transparent"), # get rid of legend bg
                                       legend.box.background = element_rect(fill = "transparent", colour = "transparent"), # get rid of legend panel bg
                                       legend.key = element_rect(fill = "transparent", colour = "transparent"),
                                       plot.background = element_rect(fill = "white", color = NA), # bg of the plot,
                                       panel.background = element_rect(fill = "white", colour = NA), # bg of the panel
                                       panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank(),
                                       panel.border = element_blank(),
                                       legend.position = "none",
                                       legend.title = element_blank(),
                                       axis.text.y = element_blank(), #element_text(size = 10),
                                       text = element_text(size = 9.5),
                                       plot.title = element_text(size = 9.5)
                               ) 
                               + scale_color_manual(values = jtools::get_colors("CUD Bright", num.colors = 2, reverse = TRUE))
                               + scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 10))
                               + labs(y = NULL,
                                      x = expression(bar(italic(Y))[Synthetic]~"(1000 Posterior Draws; Binwidth = 0.00001)"), 
                                      title = expression("Posterior Predictive Check:"~Unconditional~Probability~of~italic(Y)[ijk][" = "][1]),
                                      subtitle = expression(bar(italic(Y))[Observed]~"(Dashed Line)"~vs.~bar(italic(Y))[Synthetic]~Under~"Models 1 and 2") # https://stackoverflow.com/a/43010647
                               )
)


# Modified Version of Figure 1 with PPC Histogram Inset
# https://stackoverflow.com/a/52841737
coefficient.plot.with.inset <- (ggdraw() 
                                + draw_plot(coefficient.plot) 
                                + draw_plot(fit.1.fit.2.pp.stat.custom, x = 0.685, y = .125, width = .325, height = .325)
)



# plot(coefficient.plot)
ggsave(plot = coefficient.plot.with.inset, 
       filename = "F1_Friends_Kin_FinancialSupport_Inset.png", device = "png", dpi = 1200,
       scale = 3, width = 4, height = 4, units = "in", bg = "transparent")




#################################### MODEL DIAGNOSTICS ####################################
######## Assessment of Effective Sample Sizes
## See: https://discourse.mc-stan.org/t/new-r-hat-and-ess/8165
summary(rstan::summary(fit.1$fit)$summary[,"n_eff"]) ## Standard ESS
summary(apply(brms:::as.array.brmsfit(fit.1), MARGIN = 3, FUN = rstan::ess_bulk)) ## Bulk (Rank-Normalised) ESS
summary(apply(brms:::as.array.brmsfit(fit.1), MARGIN = 3, FUN = rstan::ess_tail)) ## Tail (Rank-Normalised) ESS

summary(rstan::summary(fit.2$fit)$summary[,"n_eff"]) ## Standard ESS
summary(apply(brms:::as.array.brmsfit(fit.2), MARGIN = 3, FUN = rstan::ess_bulk)) ## Bulk (Rank-Normalised) ESS
summary(apply(brms:::as.array.brmsfit(fit.2), MARGIN = 3, FUN = rstan::ess_tail)) ## Tail (Rank-Normalised) ESS



######## Is Rhat (R̂) for all parameters (including random effects) less than 1.01?
# R̂ is the potential scale reduction factor on split chains.
## See: https://discourse.mc-stan.org/t/new-r-hat-and-ess/8165
table(rstan::summary(fit.1$fit)$summary[,"Rhat"] < 1.01)
table(rstan::summary(fit.2$fit)$summary[,"Rhat"] < 1.01)


######## Hamiltonian Monte Carlo Diagnostics
## Quick check of Bayesian Fraction of Missing Information (BFMI), divergent transitions and violation of the tree depth
## See: https://mc-stan.org/misc/warnings.html
rstan::check_hmc_diagnostics(fit.1$fit) 
rstan::check_hmc_diagnostics(fit.2$fit) 





