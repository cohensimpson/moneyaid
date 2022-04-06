################################# The Relational Bases of Informal Financial Cooperation
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

library(VIM) # Used to visualize missing data
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


## Last, use garbage collector to clear memory
gc()



#################################### SET NUMBER OF AVALIABLE COMPUTING CORES ####################################  
# https://cran.r-project.org/web/packages/brms/vignettes/brms_threading.html
# https://paul-buerkner.github.io/brms/articles/brms_threading.html
# Total Number of CPU Cores Utilized = (brms.cores * cmdstanr.threads). For my analysis I use 10 CPU cores.
brms.cores <- 2
brms.chains <- 4
cmdstanr.threads <- 5

brms.iter <- 5000 ## How many iterations per chain?
brms.warmup <- 1000 ## How many warm-up iterations per chain?

cmdstanr.grainsize <- nrow(all_village_dyads)/120  # Default: max(100, nrow(all_village_dyads) / (2 * cmdstanr.threads))



#################################### ESTIMATE BAYESIAN MODELS ####################################
########## A NOTE ON WARNINGS AT THE BEGGINING OF SAMPLING
# At the very beginning of estimation, brms/Stan/cmdstanr would sometimes produces the following rather intimidating warning:
      # Chain XX Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
      # Chain XX Exception: Exception: bernoulli_logit_glm_lpmf: Intercept[1] is inf, but must be finite! 
      # Chain XX If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
      # Chain XX but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

# As mentioned in my paper, this warning appears to stem from inclusion of the parameter for the standard deviation of the random/varying intercepts for the 16 villages.
# Crucially, this warning is not concerning if it only occurs during the warmup phase and the diagnostics after estimation are good (i.e., there are no pathologies as indicated by 
# full convergence, low autocorrelation etc.) This is discussed in threads on the official Stan help forum, sometimes with direct input by the Stan developers themselves. 
# See in particular the threads:
      # https://discourse.mc-stan.org/t/metropolis-proposal-rejected-because-location-parameter-is-infinite/6371
      # https://discourse.mc-stan.org/t/manage-warnings-a-production-mode-for-warnings-and-errors-managing-for-the-end-user/1588
      # https://discourse.mc-stan.org/t/variable-does-not-exist-error-for-user-defined-function-in-ode-rk45/18960/7
      # https://discourse.mc-stan.org/t/metropolis-rejection-proposal-due-to-incorrect-numerical-values-for-derived-parameter/3654/4
      # https://discourse.mc-stan.org/t/compilation-error-on-rstan/17972/14
      # https://discourse.mc-stan.org/t/help-ensure-the-shape-parameter-is-positive-finite/26313/4
      # https://discourse.mc-stan.org/t/model-with-many-correlated-varying-slopes-sampling-not-done/6189/6

# Regardless, some threads on the forum suggest that such a warning is likely the result of the starting/initial values values being too extreme
# and recommend reducing the possible range of the randomly-selected initial values (i.e., [-2, +2]) or simply choosing specific initial values
# problematic parameters. See also: https://solomonkurz.netlify.app/post/2021-06-05-don-t-forget-your-inits/ 

# Accordingly, for all models, I use the following value as the initial value for the standard deviation of the random/varying intercepts.
# Note that there are three grouping factors — i.e., Egos/I_ID, Alters_J_ID, and Village. For consistency, I use the same starting value for each.

# init_func <- function(chain_id = 1){ # https://stackoverflow.com/a/70349499
#   list("sd_1" =  0.0001,
#        "sd_2" =  0.0001,
#        "sd_3" =  0.0001
#   )
# }
# 
# init_list <- list(
#   init_func(chain_id = 1),
#   init_func(chain_id = 2),
#   init_func(chain_id = 3),
#   init_func(chain_id = 4)
# )


# Here I follow the **truly-excellent** tutorial on setting initial values by Solomon Kurtz: https://solomonkurz.netlify.app/post/2021-06-05-don-t-forget-your-inits/ 
# In it, it use code similar to what appears here to randomly select starting values to one's specific specifications. This is key as initial values across chains
# are ideally not identical. And Solomon's little method allows one to still maintain reproducibility.
set_inits <- function(chain_id = chain, seed = chosen_seed, ...) { ## Function to randomly-select initial values for the standard deviation of the random intercepts
  
  set.seed(seed)
  
  list(
    "sd_1" = runif(n = 1, min = 0, max = 0.0002), # Standard deviation must be non-negative, and, following reccomendations to deal with the warning above, its possible initial values should be rather restricted.
    "sd_2" = runif(n = 1, min = 0, max = 0.0002),
    "sd_3" = runif(n = 1, min = 0, max = 0.0002)
  )
  
}

init_list <- list(
  # different seed values will return different results
  set_inits(chain_id = 1, seed = 20200127),
  set_inits(chain_id = 2, seed = 20190511),
  set_inits(chain_id = 3, seed = 20180228),
  set_inits(chain_id = 4, seed = 20160916)
)

set.seed(20200127) # Reset to main seed for the analysis. 


# Finally, depending on the versions of brms/rstan/cmdstanr you are running, you may see the error: "placing brackets after a type is deprecated and will be removed"
# This error is benign. See this GitHub exchange for details: https://github.com/paul-buerkner/brms/issues/1291



########## Model 1
fit.1 <- brm(formula = brmsformula(formula = lender_ij 
                                   
                                   ~ friend_ij + family_ij + friend_ij:family_ij
                                   
                                   + lender_ji 
                                   
                                   # + friend_ij:lender_ji + family_ij:lender_ji + friend_ij:family_ij:lender_ji
                                   
                                   + log_distance_ij_Z
                                   
                                   + goodsgame_ij
                                   + problemsolver_ij
                                   
                                   + gender_i
                                   + gender_j
                                   + same_gender_ij
                                   
                                   + age_i_Z + age_i_squared_Z
                                   + age_j_Z + age_j_squared_Z
                                   + age_absdiff_ij_sqrt_Z
                                   
                                   + edu_full_i 
                                   + edu_full_j 
                                   + same_edu_full_ij
                                   
                                   + income_i
                                   
                                   + hasPhone_i
                                   + hasPhone_j
                                   
                                   + HH_Head_i
                                   + HH_Head_j
                                   
                                   + religion_i
                                   + religion_j
                                   + same_religion_ij
                                   
                                   + leader_i
                                   + leader_j
                                   
                                   # https://stats.stackexchange.com/a/228814
                                   # https://www.muscardinus.be/2017/07/lme4-random-effects/ (see entry on "Implicit Nesting")
                                   + (1 | i_ID) + (1 | j_ID) + (1 | village) 

                                   + village_population_size_log_Z
                                   + village_census_employ_Z
                                   + village_census_noAgri_Z
                                   + village_census_poverty_Z
                                   + village_pg_savingsgroup
                                   + village_pg_market_any
                                   + village_distArua_log_Z
                                   
                                   , family = bernoulli(link = "logit")
                                   , nl = NULL
                                   , center = TRUE
)
, prior = c(
  set_prior(prior = "exponential(2)", class = "sd", coef = "", group = "",  dpar = ""), # brms::dstudent_t(x = 1:10, df = 3, mu = 0, sigma = 2.5, log = FALSE) # dexp(1:10, rate = 2) # dnorm(1:10, mean = 0, sd = 1) 
  set_prior(prior = "normal(0, 2.5)", class = "Intercept", coef = "", group = "",  dpar = ""),
  set_prior(prior = "normal(0, 1)", class = "b", group = "",  dpar = "") 
)
, data = all_village_dyads
, iter = brms.iter, warmup = brms.warmup, thin = 1
, cores = brms.cores, chains = brms.chains
, inits = init_list
, backend = "cmdstanr"
, threads = threading(threads = cmdstanr.threads, grainsize = 5670, static = TRUE) # https://cran.r-project.org/web/packages/brms/vignettes/brms_threading.html
, seed = 20200127
, silent = 0
, save_pars = save_pars(all = TRUE)
, file = "fit.1.full.model", file_refit = "on_change" ## Note that upon the completion of model estimation, brm() will save the fitted model object in your working directory as "file.rds".
)

gc()
closeAllConnections()

# robust: If FALSE (the default) the mean is used as the measure of central tendency and the standard deviation 
# as the measure of variability. If TRUE, the median and the median absolute deviation (MAD) are applied instead.
brms:::print.brmsfit(fit.1, digits = 4, prob = 0.95, priors = TRUE, robust = FALSE, mc_se = TRUE)
# View(rstan::summary(fit.1$fit)$summary) ## Results for *all* estimated parameters



########## Model 2
fit.2 <- brm(formula = brmsformula(formula = lender_ij 
                                   
                                   #~ friend_ij + family_ij + friend_ij:family_ij
                                   
                                   ~ lender_ji 
                                   
                                   # + friend_ij:lender_ji + family_ij:lender_ji + friend_ij:family_ij:lender_ji
                                   
                                   + log_distance_ij_Z
                                   
                                   + goodsgame_ij
                                   + problemsolver_ij
                                   
                                   + gender_i
                                   + gender_j
                                   + same_gender_ij
                                   
                                   + age_i_Z + age_i_squared_Z
                                   + age_j_Z + age_j_squared_Z
                                   + age_absdiff_ij_sqrt_Z
                                   
                                   + edu_full_i 
                                   + edu_full_j 
                                   + same_edu_full_ij
                                   
                                   + income_i
                                   
                                   + hasPhone_i
                                   + hasPhone_j
                                   
                                   + HH_Head_i
                                   + HH_Head_j
                                   
                                   + religion_i
                                   + religion_j
                                   + same_religion_ij
                                   
                                   + leader_i
                                   + leader_j
                                   
                                   # https://stats.stackexchange.com/a/228814
                                   # https://www.muscardinus.be/2017/07/lme4-random-effects/ (see entry on "Implicit Nesting")
                                   + (1 | i_ID) + (1 | j_ID) + (1 | village) 
                                   
                                   + village_population_size_log_Z
                                   + village_census_employ_Z
                                   + village_census_noAgri_Z
                                   + village_census_poverty_Z
                                   + village_pg_savingsgroup
                                   + village_pg_market_any
                                   + village_distArua_log_Z
                                   
                                   , family = bernoulli(link = "logit")
                                   , nl = NULL
                                   , center = TRUE
)
, prior = c(
  set_prior(prior = "exponential(2)", class = "sd", coef = "", group = "",  dpar = ""), # brms::dstudent_t(x = 1:10, df = 3, mu = 0, sigma = 2.5, log = FALSE) # dexp(1:10, rate = 2) # dnorm(1:10, mean = 0, sd = 1) 
  set_prior(prior = "normal(0, 2.5)", class = "Intercept", coef = "", group = "",  dpar = ""),
  set_prior(prior = "normal(0, 1)", class = "b", group = "",  dpar = "") 
)
, data = all_village_dyads
, iter = brms.iter, warmup = brms.warmup, thin = 1
, cores = brms.cores, chains = brms.chains
, inits = init_list
, backend = "cmdstanr"
, threads = threading(threads = cmdstanr.threads, grainsize = 5670, static = TRUE) # https://cran.r-project.org/web/packages/brms/vignettes/brms_threading.html
, seed = 20200127
, silent = 0
, save_pars = save_pars(all = TRUE)
, file = "fit.2.controls.model", file_refit = "on_change" ## Note that upon the completion of model estimation, brm() will save the fitted model object in your working directory as "file.rds".
)

gc()
closeAllConnections()

# robust: If FALSE (the default) the mean is used as the measure of central tendency and the standard deviation 
# as the measure of variability. If TRUE, the median and the median absolute deviation (MAD) are applied instead.
brms:::print.brmsfit(fit.2, digits = 4, prob = 0.95, priors = TRUE, robust = FALSE, mc_se = TRUE)
# View(rstan::summary(fit.2$fit)$summary) ## Results for *all* estimated parameters



########## Model 3
fit.3 <- brm(formula = brmsformula(formula = lender_ij 
                                   
                                   ~ friend_ij + family_ij + friend_ij:family_ij
                                   
                                   + lender_ji 
                                   
                                   # + friend_ij:lender_ji + family_ij:lender_ji + friend_ij:family_ij:lender_ji
                                   
                                   + log_distance_ij_Z
                                   
                                   # + goodsgame_ij
                                   # + problemsolver_ij
                                   
                                   + gender_i
                                   + gender_j
                                   + same_gender_ij
                                   
                                   + age_i_Z + age_i_squared_Z
                                   + age_j_Z + age_j_squared_Z
                                   + age_absdiff_ij_sqrt_Z
                                   
                                   # + edu_full_i 
                                   # + edu_full_j 
                                   # + same_edu_full_ij
                                   # 
                                   # + income_i
                                   # 
                                   # + hasPhone_i
                                   # + hasPhone_j
                                   # 
                                   # + HH_Head_i
                                   # + HH_Head_j
                                   # 
                                   # + religion_i
                                   # + religion_j
                                   # + same_religion_ij
                                   # 
                                   # + leader_i
                                   # + leader_j
                                   
                                   # https://stats.stackexchange.com/a/228814
                                   # https://www.muscardinus.be/2017/07/lme4-random-effects/ (see entry on "Implicit Nesting")
                                   + (1 | i_ID) + (1 | j_ID) + (1 | village) 
                                   
                                   + village_population_size_log_Z
                                   + village_census_employ_Z
                                   + village_census_noAgri_Z
                                   + village_census_poverty_Z
                                   + village_pg_savingsgroup
                                   + village_pg_market_any
                                   + village_distArua_log_Z
                                   
                                   , family = bernoulli(link = "logit")
                                   , nl = NULL
                                   , center = TRUE
)
, prior = c(
  set_prior(prior = "exponential(2)", class = "sd", coef = "", group = "",  dpar = ""), # brms::dstudent_t(x = 1:10, df = 3, mu = 0, sigma = 2.5, log = FALSE) # dexp(1:10, rate = 2) # dnorm(1:10, mean = 0, sd = 1)  
  set_prior(prior = "normal(0, 2.5)", class = "Intercept", coef = "", group = "",  dpar = ""),
  set_prior(prior = "normal(0, 1)", class = "b", group = "",  dpar = "") 
)
, data = all_village_dyads
, iter = brms.iter, warmup = brms.warmup, thin = 1
, cores = brms.cores, chains = brms.chains
, inits = init_list
, backend = "cmdstanr"
, threads = threading(threads = cmdstanr.threads, grainsize = 5670, static = TRUE) # https://cran.r-project.org/web/packages/brms/vignettes/brms_threading.html
, seed = 20200127
, silent = 0
, save_pars = save_pars(all = TRUE)
, file = "fit.3.simple.model", file_refit = "on_change" ## Note that upon the completion of model estimation, brm() will save the fitted model object in your working directory as "file.rds".
)

gc()
closeAllConnections()

# robust: If FALSE (the default) the mean is used as the measure of central tendency and the standard deviation 
# as the measure of variability. If TRUE, the median and the median absolute deviation (MAD) are applied instead.
brms:::print.brmsfit(fit.3, digits = 4, prob = 0.95, priors = TRUE, robust = FALSE, mc_se = TRUE)
# View(rstan::summary(fit.3$fit)$summary) ## Results for *all* estimated parameters



########## Model 4
fit.4 <- brm(formula = brmsformula(formula = lender_ij 
                                   
                                   ~ friend_ij + family_ij + friend_ij:family_ij
                                   
                                   + lender_ji 
                                   
                                   + friend_ij:lender_ji + family_ij:lender_ji + friend_ij:family_ij:lender_ji
                                   
                                   + log_distance_ij_Z
                                   
                                   + goodsgame_ij
                                   + problemsolver_ij
                                   
                                   + gender_i
                                   + gender_j
                                   + same_gender_ij
                                   
                                   + age_i_Z + age_i_squared_Z
                                   + age_j_Z + age_j_squared_Z
                                   + age_absdiff_ij_sqrt_Z
                                   
                                   + edu_full_i
                                   + edu_full_j
                                   + same_edu_full_ij

                                   + income_i

                                   + hasPhone_i
                                   + hasPhone_j

                                   + HH_Head_i
                                   + HH_Head_j

                                   + religion_i
                                   + religion_j
                                   + same_religion_ij

                                   + leader_i
                                   + leader_j
                                   
                                   # https://stats.stackexchange.com/a/228814
                                   # https://www.muscardinus.be/2017/07/lme4-random-effects/ (see entry on "Implicit Nesting")
                                   + (1 | i_ID) + (1 | j_ID) + (1 | village) 
                                   
                                   + village_population_size_log_Z
                                   + village_census_employ_Z
                                   + village_census_noAgri_Z
                                   + village_census_poverty_Z
                                   + village_pg_savingsgroup
                                   + village_pg_market_any
                                   + village_distArua_log_Z
                                   
                                   , family = bernoulli(link = "logit")
                                   , nl = NULL
                                   , center = TRUE
)
, prior = c(
  set_prior(prior = "exponential(2)", class = "sd", coef = "", group = "",  dpar = ""), # brms::dstudent_t(x = 1:10, df = 3, mu = 0, sigma = 2.5, log = FALSE) # dexp(1:10, rate = 2) # dnorm(1:10, mean = 0, sd = 1)  
  set_prior(prior = "normal(0, 2.5)", class = "Intercept", coef = "", group = "",  dpar = ""),
  set_prior(prior = "normal(0, 1)", class = "b", group = "",  dpar = "") 
)
, data = all_village_dyads
, iter = brms.iter, warmup = brms.warmup, thin = 1
, cores = brms.cores, chains = brms.chains
, inits = init_list
, backend = "cmdstanr"
, threads = threading(threads = cmdstanr.threads, grainsize = 5670, static = TRUE) # https://cran.r-project.org/web/packages/brms/vignettes/brms_threading.html
, seed = 20200127
, silent = 0
, save_pars = save_pars(all = TRUE)
, file = "fit.4.extended.model", file_refit = "on_change" ## Note that upon the completion of model estimation, brm() will save the fitted model object in your working directory as "file.rds".
)

gc()
closeAllConnections()

# robust: If FALSE (the default) the mean is used as the measure of central tendency and the standard deviation 
# as the measure of variability. If TRUE, the median and the median absolute deviation (MAD) are applied instead.
brms:::print.brmsfit(fit.4, digits = 4, prob = 0.95, priors = TRUE, robust = FALSE, mc_se = TRUE)
# View(rstan::summary(fit.4$fit)$summary) ## Results for *all* estimated parameters

# View(village_networks_coresidence$`11` * village_networks_geodist$`11`)



#################################### DIAGNOSTICS FOR BAYESIAN MODELS ####################################
######## Diagnostics for Leave-One-Out Cross Validation with Pareto Smoothed Importance Sampling (PSIS-LOO)
# Summarise the estimated Pareto shape parameters and PSIS effective sample sizes and 
# find the index of observations for which the estimated Pareto shape parameter k
# is greater than 0.5. This is akin to checking for outliers/influential observations.

# https://discourse.mc-stan.org/t/a-quick-note-what-i-infer-from-p-loo-and-pareto-k-values/3446
# https://mc-stan.org/loo/reference/loo-glossary.html
# https://avehtari.github.io/bayes_R2/bayes_R2.html
# https://discourse.mc-stan.org/t/r2-vs-bayes-r2/12788/9

# https://paul-buerkner.github.io/brms/reference/add_criterion.html
# https://mc-stan.org/loo/reference/index.html
# https://paul-buerkner.github.io/brms/reference/loo.brmsfit.html


## Next, clear the environment of all objects save those that are required to run fit the brms models and then clear working memory.
rm(list = setdiff(ls(), list("all_village_dyads", "brms.cores", "brms.chains", "cmdstanr.threads", "brms.iter", "brms.warmup", "cmdstanr.grainsize", "set_inits", "init_list"))) 
gc()

fit.1 <- readRDS("fit.1.full.model.rds") ## Load only the model object needed to preserve memory for PSIS-LOO

fit.1 <- add_criterion( 
  x = fit.1,
  criterion = c("loo", "bayes_R2"), 
  ndraws = 1000,
  cores = 4,
  pointwise = FALSE,
  save_psis = TRUE,
  overwrite = TRUE ## Save a new version of the object for the fitted model that has diagnostic criterion appended
)
gc()

fit.1$criteria$loo
mean(fit.1$criteria$bayes_R2)
hist(fit.1$criteria$bayes_R2)



rm(list = setdiff(ls(), list("all_village_dyads", "brms.cores", "brms.chains", "cmdstanr.threads", "brms.iter", "brms.warmup", "cmdstanr.grainsize", "set_inits", "init_list"))) 
gc()

fit.2 <- readRDS("fit.2.controls.model.rds")

fit.2 <- add_criterion( 
  x = fit.2,
  criterion = c("loo", "bayes_R2"),
  ndraws = 1000,
  cores = 4,
  pointwise = FALSE,
  save_psis = TRUE,
  overwrite = TRUE
)
gc()

fit.2$criteria$loo
mean(fit.2$criteria$bayes_R2)
hist(fit.2$criteria$bayes_R2)



rm(list = setdiff(ls(), list("all_village_dyads", "brms.cores", "brms.chains", "cmdstanr.threads", "brms.iter", "brms.warmup", "cmdstanr.grainsize", "set_inits", "init_list"))) 
gc()

fit.3 <- readRDS("fit.3.simple.model.rds")

fit.3 <- add_criterion( 
  x = fit.3,
  criterion = c("loo", "bayes_R2"), 
  ndraws = 1000,
  cores = 4,
  pointwise = FALSE,
  save_psis = TRUE,
  overwrite = TRUE
)
gc()

fit.3$criteria$loo
mean(fit.3$criteria$bayes_R2)
hist(fit.3$criteria$bayes_R2)

gc()



rm(list = setdiff(ls(), list("all_village_dyads", "brms.cores", "brms.chains", "cmdstanr.threads", "brms.iter", "brms.warmup", "cmdstanr.grainsize", "set_inits", "init_list"))) 
gc()

fit.4 <- readRDS("fit.4.extended.model.rds")

fit.4 <- add_criterion( 
  x = fit.4,
  criterion = c("loo", "bayes_R2"), 
  ndraws = 1000,
  cores = 4,
  pointwise = FALSE,
  save_psis = TRUE,
  overwrite = TRUE
)
gc()

fit.4$criteria$loo
mean(fit.4$criteria$bayes_R2)
hist(fit.4$criteria$bayes_R2)

gc()


#Re-load the rest of the fitted model objects with appended diagnostic criteria
fit.1 <- readRDS("fit.1.full.model.rds") 
fit.2 <- readRDS("fit.2.controls.model.rds")
fit.3 <- readRDS("fit.3.simple.model.rds")


# https://avehtari.github.io/modelselection/CV-FAQ.html#15_How_to_interpret_in_Standard_error_(SE)_of_elpd_difference_(elpd_diff)
# https://discourse.mc-stan.org/t/clarifying-interpretation-of-loo-compare-output/19726/2
fit.1.vs.fit.2.loo <- loo_compare(fit.1, fit.2, criterion = "loo") 
print(fit.1.vs.fit.2.loo)

fit.1.vs.fit.3.loo <- loo_compare(fit.1, fit.3, criterion = "loo") 
print(fit.1.vs.fit.3.loo)

fit.1.vs.fit.4.loo <- loo_compare(fit.1, fit.4, criterion = "loo") 
print(fit.1.vs.fit.4.loo)



# https://discourse.mc-stan.org/t/if-elpd-diff-se-diff-2-is-this-noteworthy/20549/10
# https://discourse.mc-stan.org/t/if-elpd-diff-se-diff-2-is-this-noteworthy/20549/12
fit.1.vs.fit.2.loo[, "elpd_diff"][2] + (c(-1, 1) * 1.96 * fit.1.vs.fit.2.loo[, "se_diff"][2]) # Approximate 95% Interval for elpd_diff
fit.1.vs.fit.3.loo[, "elpd_diff"][2] + (c(-1, 1) * 1.96 * fit.1.vs.fit.3.loo[, "se_diff"][2]) 
fit.1.vs.fit.4.loo[, "elpd_diff"][2] + (c(-1, 1) * 1.96 * fit.1.vs.fit.4.loo[, "se_diff"][2]) 




#################################### FIGURE 1: COEFFICIENT PLOT ####################################
# https://jtools.jacob-long.com/reference/plot_summs.html
# https://jtools.jacob-long.com/reference/jtools_colors.html
# https://personal.sron.nl/~pault/


coefficient.plot <- plot_coefs(fit.1, fit.2, fit.3, fit.4,
                               ci_level = 0.95,
                               inner_ci_level = 0.68,
                               # conf.method = "quantile", ## Using this option seems to break plot_coefs().Possibly related to broom/broom.mixed clashing.
                               model.names = c(paste0("Full (Model 1)\nBayes R^2 = ",
                                                      sprintf("%.3f", mean(fit.1$criteria$bayes_R2)),
                                                      "\n95% QI [", 
                                                      sprintf("%.3f", quantile(fit.1$criteria$bayes_R2, probs = c(0.025, 0.975)))[1],
                                                      ", ",
                                                      sprintf("%.3f", quantile(fit.1$criteria$bayes_R2, probs = c(0.025, 0.975)))[2],
                                                      "]"
                                                      ),
                                               paste0("Controls (Model 2)\nBayes R^2 = ",
                                                      sprintf("%.3f", mean(fit.2$criteria$bayes_R2)),
                                                      "\n95% QI [", 
                                                      sprintf("%.3f", quantile(fit.2$criteria$bayes_R2, probs = c(0.025, 0.975)))[1],
                                                      ", ",
                                                      sprintf("%.3f", quantile(fit.2$criteria$bayes_R2, probs = c(0.025, 0.975)))[2],
                                                      "]"
                                                      ),
                                               paste0("Simple (Model 3)\nBayes R^2 = ",
                                                      sprintf("%.3f", mean(fit.3$criteria$bayes_R2)),
                                                      "\n95% QI [", 
                                                      sprintf("%.3f", quantile(fit.3$criteria$bayes_R2, probs = c(0.025, 0.975)))[1],
                                                      ", ",
                                                      sprintf("%.3f", quantile(fit.3$criteria$bayes_R2, probs = c(0.025, 0.975)))[2],
                                                      "]"
                                               ),
                                               paste0("Extended (Model 4)\nBayes R^2 = ",
                                                      sprintf("%.3f", mean(fit.4$criteria$bayes_R2)),
                                                      "\n95% QI [", 
                                                      sprintf("%.3f", quantile(fit.4$criteria$bayes_R2, probs = c(0.025, 0.975)))[1],
                                                      ", ",
                                                      sprintf("%.3f", quantile(fit.4$criteria$bayes_R2, probs = c(0.025, 0.975)))[2],
                                                      "]"
                                               )
                                               ),
                               coefs = c( # "Intercept" = "(Intercept)", # Exclude as it skews the graph
                                          "Best Friend of Ego" = "friend_ijBestFriend",
                                          "Salient Kin" = "family_ijSalientKin",
                                          "Best Friend of Ego x Salient Kin" = "friend_ijBestFriend:family_ijSalientKin",
                                          "Money Lender for Alter" = "lender_jiYes",
                                          "Best Friend of Ego x Money Lender for Alter" = "friend_ijBestFriend:lender_jiYes",
                                          "Salient Kin x Money Lender for Alter" = "family_ijSalientKin:lender_jiYes",
                                          "Best Friend of Ego x Salient Kin x Money Lender for Alter" = "friend_ijBestFriend:family_ijSalientKin:lender_jiYes",
                                          "Geographic Distance Between Ego and Alter (Log)" = "log_distance_ij_Z",
                                          "Alter Preferred Money Handler for Ego" = "goodsgame_ijPreferredMoneyHandler",
                                          "Alter Preferred Problem Solver for Ego" = "problemsolver_ijPreferredProblemSolver",
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
                                          "Religion (Ego): Not Catholic (Ref: Catholic)" = "religion_iNotCatholic",
                                          "Religion (Alter): Not Catholic (Ref: Catholic)" = "religion_jNotCatholic",
                                          "Same Religion (Ego & Alter)" = "same_religion_ijSameReligion",
                                          "Village Leader (Ego): Yes (Ref: No)" = "leader_iYes",
                                          "Village Leader (Alter): Yes (Ref: No)" = "leader_jYes",
                                          "Village Population Size (Log)" = "village_population_size_log_Z",
                                          "Village Employement Rate" = "village_census_employ_Z",
                                          "Village %Employed in Non-Agricultural Work" = "village_census_noAgri_Z",
                                          "Village Poverty Rate" = "village_census_poverty_Z",
                                          "Village Savings Groups" = "village_pg_savingsgroup",
                                          "Village Market: General or Farmers (Ref: No Market)" = "village_pg_market_anyGeneralorFarmersMarket",
                                          "Village Distance to Arua (Market City)" = "village_distArua_log_Z"
                                          
                               ),
                               omit.coefs = NULL,
                               colors = c("#EE7733", "#009988", "#CC3311", "#33BBEE"),
                               plot.distributions = FALSE,
                               exp = FALSE,
                               point.shape = TRUE,
                               legend.title = NULL 
                               
)  + labs(title = NULL,
          y = NULL,
          x = expression(Posterior~Mean~Log~Odds~Ratio~of~italic(Y)[ijk][" = "][1][" = "][Money~Lender][" (i = Ego, j = Alter, k = Village)"]~plain("+ 95% Quantile Interval [QI] + 68% Inner QI")), # https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html
          
          ## This sections uses the paste0 funciton to combine various bits of information about the random intercepts together to create the big caption at the bottom of Figure 1.
          caption = paste0("\n", # http://paul-buerkner.github.io/brms/reference/VarCorr.brmsfit.html
                           "SD of Varying Intercepts for Village (Levels = 16) - ",
                           "Model 1 (",
                           "Estimate = ", sprintf("%.3f", VarCorr(x = fit.1, robust = FALSE, probs = c(0.025, 0.975))$village$sd["Intercept", "Estimate"]),
                           "; S.E. = ",  sprintf("%.3f", VarCorr(x = fit.1, robust = FALSE, probs = c(0.025, 0.975))$village$sd["Intercept", "Est.Error"]),
                           "])",
                           
                           "; Model 2 (",
                           "Estimate = ", sprintf("%.3f", VarCorr(x = fit.2, robust = FALSE, probs = c(0.025, 0.975))$village$sd["Intercept", "Estimate"]),
                           "; S.E. = ",  sprintf("%.3f", VarCorr(x = fit.2, robust = FALSE, probs = c(0.025, 0.975))$village$sd["Intercept", "Est.Error"]),
                           "])",
                           
                           "; Model 3 (",
                           "Estimate = ", sprintf("%.3f", VarCorr(x = fit.3, robust = FALSE, probs = c(0.025, 0.975))$village$sd["Intercept", "Estimate"]),
                           "; S.E. = ",  sprintf("%.3f", VarCorr(x = fit.3, robust = FALSE, probs = c(0.025, 0.975))$village$sd["Intercept", "Est.Error"]),
                           "])",
                           
                           "; Model 4 (",
                           "Estimate = ", sprintf("%.3f", VarCorr(x = fit.4, robust = FALSE, probs = c(0.025, 0.975))$village$sd["Intercept", "Estimate"]),
                           "; S.E. = ",  sprintf("%.3f", VarCorr(x = fit.4, robust = FALSE, probs = c(0.025, 0.975))$village$sd["Intercept", "Est.Error"]),
                           "])",
                           
                           "\n",
                           "SD of Varying Intercepts for Village:Ego (Levels = 3,184) - ",
                           "Model 1 (",
                           "Estimate = ", sprintf("%.3f", VarCorr(x = fit.1, robust = FALSE, probs = c(0.025, 0.975))$i_ID$sd["Intercept", "Estimate"]),
                           "; S.E. = ",  sprintf("%.3f", VarCorr(x = fit.1, robust = FALSE, probs = c(0.025, 0.975))$i_ID$sd["Intercept", "Est.Error"]),
                           "])",
                           
                           "; Model 2 (",
                           "Estimate = ", sprintf("%.3f", VarCorr(x = fit.2, robust = FALSE, probs = c(0.025, 0.975))$i_ID$sd["Intercept", "Estimate"]),
                           "; S.E. = ",  sprintf("%.3f", VarCorr(x = fit.2, robust = FALSE, probs = c(0.025, 0.975))$i_ID$sd["Intercept", "Est.Error"]),
                           "])",
                           
                           "; Model 3 (",
                           "Estimate = ", sprintf("%.3f", VarCorr(x = fit.3, robust = FALSE, probs = c(0.025, 0.975))$i_ID$sd["Intercept", "Estimate"]),
                           "; S.E. = ",  sprintf("%.3f", VarCorr(x = fit.3, robust = FALSE, probs = c(0.025, 0.975))$i_ID$sd["Intercept", "Est.Error"]),
                           "])",
                           
                           "; Model 4 (",
                           "Estimate = ", sprintf("%.3f", VarCorr(x = fit.4, robust = FALSE, probs = c(0.025, 0.975))$i_ID$sd["Intercept", "Estimate"]),
                           "; S.E. = ",  sprintf("%.3f", VarCorr(x = fit.4, robust = FALSE, probs = c(0.025, 0.975))$i_ID$sd["Intercept", "Est.Error"]),
                           "])",
                           
                           
                           "\n",
                           "SD of Varying Intercepts for Village:Alter (Levels = 3,184) - ",
                           "Model 1 (",
                           "Estimate = ", sprintf("%.3f", VarCorr(x = fit.1, robust = FALSE, probs = c(0.025, 0.975))$j_ID$sd["Intercept", "Estimate"]),
                           "; S.E. = ",  sprintf("%.3f", VarCorr(x = fit.1, robust = FALSE, probs = c(0.025, 0.975))$j_ID$sd["Intercept", "Est.Error"]),
                           "])",
                           
                           "; Model 2 (",
                           "Estimate = ", sprintf("%.3f", VarCorr(x = fit.2, robust = FALSE, probs = c(0.025, 0.975))$j_ID$sd["Intercept", "Estimate"]),
                           "; S.E. = ",  sprintf("%.3f", VarCorr(x = fit.2, robust = FALSE, probs = c(0.025, 0.975))$j_ID$sd["Intercept", "Est.Error"]),
                           "])",
                           
                           "; Model 3 (",
                           "Estimate = ", sprintf("%.3f", VarCorr(x = fit.3, robust = FALSE, probs = c(0.025, 0.975))$j_ID$sd["Intercept", "Estimate"]),
                           "; S.E. = ",  sprintf("%.3f", VarCorr(x = fit.3, robust = FALSE, probs = c(0.025, 0.975))$j_ID$sd["Intercept", "Est.Error"]),
                           "])",
                           
                           "; Model 4 (",
                           "Estimate = ", sprintf("%.3f", VarCorr(x = fit.4, robust = FALSE, probs = c(0.025, 0.975))$j_ID$sd["Intercept", "Estimate"]),
                           "; S.E. = ",  sprintf("%.3f", VarCorr(x = fit.4, robust = FALSE, probs = c(0.025, 0.975))$j_ID$sd["Intercept", "Est.Error"]),
                           "])"


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
                                        axis.text.y = element_text(hjust = 1),
                                        axis.title = element_text(size = 10),
                                        legend.position = "right",
                                        legend.title = element_blank()
) + scale_x_continuous(limits = c(-2.5, 3.5), breaks = c(-2, -1, 0, 1, 2, 3))
 

# plot(coefficient.plot)
# ggsave(plot = coefficient.plot, 
#        filename = "F1_Friends_Kin_FinancialSupport.png", device = "png", dpi = 1200,
#        scale = 3, width = 4.75, height = 4, units = "in", bg = "transparent")




#################################### FIGURE 2a: CONDITIONAL EFFECTS FOR MODEL 1 ####################################
typical_dyad <- cbind.data.frame(lender_ji = "No",
                                 log_distance_ij_Z = 0, ## Note the standardization/use of Z scores!
                                 goodsgame_ij = "Not Preferred Money Handler",
                                 problemsolver_ij = "Not Preferred Problem Solver",
                                 gender_i = "Female",
                                 gender_j = "Female",
                                 same_gender_ij = "Same Gender",
                                 age_i_Z = 0,
                                 age_i_squared_Z = 0,
                                 age_j_Z = 0,
                                 age_j_squared_Z = 0,
                                 age_absdiff_ij_sqrt_Z = min(fit.1$data$age_absdiff_ij_sqrt_Z), ## We want two individuals of the SAME AGE, thus age_absdiff_ij equal to zero, where age_absdiff_ij_sqrt_Z == 0 == Mean sqrt(age difference)!
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
                                 village_population_size_log_Z = 0,
                                 village_census_employ_Z = 0,
                                 village_census_noAgri_Z = 0,
                                 village_census_poverty_Z = 0,
                                 village_pg_savingsgroup = mean(fit.1$data$village_pg_savingsgroup),
                                 village_pg_market_any = "No Market",
                                 village_distArua_log_Z = 0
                                 
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



# Construct Plot
condeff_plot_object <- brms:::plot.brms_conditional_effects(condeff_plot, facet_args = list("ncol" = 1), theme = theme_nice(style = "black"), plot = FALSE)
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
                                                    panel.spacing.x = unit(0.25, "cm"),
                                                    axis.text = element_text(size = 10),
                                                    axis.title = element_text(size = 10),
                                                    legend.position = "bottom",
                                                    legend.title = element_blank()
) 
+ labs(title = NULL, x = NULL, y = expression(Predicted~Probability~of~italic(Y)[ijk][" = "][1][" = "][Money~Lender]))
+ scale_y_continuous(limits = c(0, 0.175), breaks = seq(0, 0.175, 0.025))
+ scale_color_manual(values = c("#4393C3", "#B2182B"))
+ guides(fill = "none") # https://statisticsglobe.com/remove-legend-ggplot2-r
) 


# plot(condeff_plot_object)
# ggsave(plot = condeff_plot_object, 
#        filename = "F2b_Friends_Kin_FinancialSupport.png", device = "png", dpi = 1200,
#        scale = 2.75, width = 2.25, height = 2.25, units = "in", bg = "transparent")




#################################### FIGURE 2b: DIFFERENCE IN EXPECTED LOG POINTWISE PREDICTIVE DENSITIES (MODEL 1 vs. MODEL 4) ####################################
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
                   + geom_point(aes(colour = factor(village)), size = 1, alpha = 0.8) 
                   + scale_color_manual(values = c("7"  = "#2166AC",
                                                   "9"  = "#4393C3",
                                                   "11" = "#FFEE99",
                                                   "14" = "#D1E5F0", 
                                                   "29" = "#B2182B",
                                                   "40" = "#D6604D",
                                                   "46" = "#F4A582",
                                                   "47" = "#FDDBC7",
                                                   "50" = "#762A83",
                                                   "51" = "#9970AB",
                                                   "55" = "#C2A5CF",
                                                   "66" = "#E7D4E8", 
                                                   "75" = "#1B7837",
                                                   "77" = "#5AAE61",
                                                   "80" = "#ACD39E",
                                                   "81" = "#D9F0D3"
                   )
                   )
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
) + labs(title = NULL,
         x = "Dyad Index")



# plot(elpd_diff_plot)
# ggsave(plot = elpd_diff_plot, 
#        filename = "F2c_Friends_Kin_FinancialSupport.png", device = "png", dpi = 1200,
#        scale = 2.75, width = 2.25, height = 2, units = "in", bg = "transparent")




#################################### FIGURE 2c: CONDITIONAL EFFECTS FOR MODEL 4 ####################################
typical_dyad.2 <- rbind.data.frame(  
  "Ego Money Lender for Alter" = cbind.data.frame(lender_ji = "Yes",
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
                                              age_absdiff_ij_sqrt_Z = min(fit.1$data$age_absdiff_ij_sqrt_Z), ## We want two individuals of the SAME AGE, thus age_absdiff_ij equal to zero, where age_absdiff_ij_sqrt_Z == 0 == Mean sqrt(age difference)!
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
                                              village_population_size_log_Z = 0,
                                              village_census_employ_Z = 0,
                                              village_census_noAgri_Z = 0,
                                              village_census_poverty_Z = 0,
                                              village_pg_savingsgroup = mean(fit.1$data$village_pg_savingsgroup),
                                              village_pg_market_any = "No Market",
                                              village_distArua_log_Z = 0
                                              
  ),
  
  "Ego Not a Money Lender for Alter" = cbind.data.frame(lender_ji = "No",
                                                    log_distance_ij_Z = 0,  ## Note the standardization/use of Z scores!
                                                    goodsgame_ij = "Not Preferred Money Handler",
                                                    problemsolver_ij = "Not Preferred Problem Solver",
                                                    gender_i = "Male",
                                                    gender_j = "Male",
                                                    same_gender_ij = "Same Gender",
                                                    age_i_Z = 0,
                                                    age_i_squared_Z = 0,
                                                    age_j_Z = 0,
                                                    age_j_squared_Z = 0,
                                                    age_absdiff_ij_sqrt_Z = min(fit.1$data$age_absdiff_ij_sqrt_Z), ## We want two individuals of the SAME AGE, thus age_absdiff_ij equal to zero, where age_absdiff_ij_sqrt_Z == 0 == Mean sqrt(age difference)!
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
                                                    village_population_size_log_Z = 0,
                                                    village_census_employ_Z = 0,
                                                    village_census_noAgri_Z = 0,
                                                    village_census_poverty_Z = 0,
                                                    village_pg_savingsgroup = mean(fit.1$data$village_pg_savingsgroup),
                                                    village_pg_market_any = "No Market",
                                                    village_distArua_log_Z = 0
  )
)

# On plotting three-way interactiosn w/ brms, see: https://discourse.mc-stan.org/t/how-do-i-use-the-conditions-argument-for-conditional-effects/26884/4
condeff_plot.2 <- conditional_effects(x = fit.4,
                                      method = "posterior_epred",
                                      effects = c("friend_ij:family_ij"),
                                      conditions = typical_dyad.2,
                                      categorical = FALSE,
                                      re_formula = NA,
                                      prob = 0.95,
                                      resolution = 1000,
                                      cores = 1,
                                      plot = FALSE
)



# Construct Plot
condeff_plot_object.2 <- brms:::plot.brms_conditional_effects(condeff_plot.2, facet_args = list("ncol" = 2), theme = theme_nice(style = "black"), plot = FALSE)
condeff_plot_object.2 <- condeff_plot_object.2$`friend_ij:family_ij`
condeff_plot_object.2 <- (condeff_plot_object.2 + theme(axis.line = element_line(color = "black"),
                                                        legend.background = element_rect(fill = "transparent", colour = "transparent"), # get rid of legend bg
                                                        legend.box.background = element_rect(fill = "transparent", colour = "transparent"), # get rid of legend panel bg
                                                        legend.key = element_rect(fill = "transparent", colour = "transparent"),
                                                        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot,
                                                        panel.background = element_rect(fill = "transparent"), # bg of the panel
                                                        panel.grid.major = element_blank(),
                                                        panel.grid.minor = element_blank(),
                                                        panel.border = element_blank(),
                                                        panel.spacing.x = unit(0.25, "cm"),
                                                        axis.text = element_text(size = 10),
                                                        axis.title = element_text(size = 10),
                                                        legend.position = "bottom",
                                                        legend.title = element_blank()
) 
+ labs(title = NULL, x = NULL, y = expression(Predicted~Probability~of~italic(Y)[ijk][" = "][1][" = "][Money~Lender]))
+ scale_y_continuous(limits = c(0, 0.25), breaks = seq(0, 0.25, 0.025))
+ scale_color_manual(values = c("#4393C3", "#B2182B"))
+ guides(fill = "none") # https://statisticsglobe.com/remove-legend-ggplot2-r
) 


# plot(condeff_plot_object.2)
# ggsave(plot = condeff_plot_object.2, 
#        filename = "F2b_Friends_Kin_FinancialSupport.png", device = "png", dpi = 1200,
#        scale = 2.75, width = 2.25, height = 2.25, units = "in", bg = "transparent")




#################################### FIGURE 2a + 2b + 2c ####################################
## Combine the panels to make Figure 2
# https://wilkelab.org/cowplot/articles/plot_grid.html

condeff_plot_object_and_elpd_diff_plot <- plot_grid(condeff_plot_object, elpd_diff_plot, condeff_plot_object.2,
                                                    labels = c("a", "b", "c"), 
                                                    ncol = 3,
                                                    label_size = 13,
                                                    # label_fontfamily = "sans",
                                                    label_fontface = "bold",
                                                    vjust = 1,
                                                    rel_widths = c(0.65, 1.30, 1.10), 
                                                    rel_heights = c(0.75, 1, 0.75)
)



# plot(coefficient.plot)
ggsave(plot = condeff_plot_object_and_elpd_diff_plot, 
       filename = "F2_Friends_Kin_FinancialSupport.png", device = "png", dpi = 1200,
       scale = 3.5, width = 5, height = 1.5, units = "in", bg = "transparent")




#################################### POSTERIOR PREDICTIVE CHECKS ####################################
## Overall/Unconditional Probability of Seeking Financial Aid p(y_ijk = 1) 
## Comparison of the proportion of 1s in the data vs the proportions of 1s in the posterior predictive distribution
## See: # https://discourse.mc-stan.org/t/simple-ppc-for-logistic-regression/17749/7

# The posterior predictive distribution is the distribution of the outcome implied by the model after 
# using the observed data to update our beliefs about the unknown parameters in the model. Simulating 
# data from the posterior predictive distribution using the observed predictors is useful for checking
# the fit of the model. This can be performed for the data used to fit the model (posterior predictive checks) 
# or for new data. By definition, these draws have higher variance than draws of the means of the posterior 
# predictive distribution computed by posterior_epred.brmsfit. This is because the residual error is 
# incorporated in posterior_predict.

# In univariate models, the output of posterior_predict() is as an S x N matrix, where S is the 
# number of posterior draws and N is the number of observations. 
# https://mc-stan.org/rstanarm/reference/posterior_predict.stanreg.html


## First, grab the draws from the posterior under each model
# For those who are new to PP Checks, see this step-by-step guide: https://mc-stan.org/bayesplot/articles/graphical-ppcs.html
# Note that the returned result will be quite large in terms of memory and setting ndraws higher can result in R/Rstudio crashing due to memory availability
fit.1.pp <- posterior_predict(fit.1, newdata = NULL, re_formula = NULL, ndraws = 500, cores = 1, summary = FALSE)
fit.2.pp <- posterior_predict(fit.2, newdata = NULL, re_formula = NULL, ndraws = 500, cores = 1, summary = FALSE)
fit.3.pp <- posterior_predict(fit.3, newdata = NULL, re_formula = NULL, ndraws = 500, cores = 1, summary = FALSE)
fit.4.pp <- posterior_predict(fit.4, newdata = NULL, re_formula = NULL, ndraws = 500, cores = 1, summary = FALSE) # RUN: format(object.size(fit.4.pp), units = "Gb")


## Second, create model-specific pp-checks for the within-village unconditional mean of seeking financial aid
# https://mc-stan.org/bayesplot/reference/PPC-test-statistics.html
fit.1.pp.stat.grouped <- ppc_stat_grouped(
  y = fit.1$data$lender_ij,
  yrep = fit.1.pp,
  group = factor(fit.1$data$village, levels = as.character( sort( as.numeric( levels(fit.1$data$village) ) ) ) ), 
  stat = "mean",
  facet_args = list(),
  binwidth = 0.0001,
  breaks = NULL,
  freq = TRUE
)



fit.2.pp.stat.grouped <- ppc_stat_grouped(
  y = fit.2$data$lender_ij,
  yrep = fit.2.pp,
  group = factor(fit.2$data$village, levels = as.character( sort( as.numeric( levels(fit.2$data$village) ) ) ) ), 
  stat = "mean",
  facet_args = list(),
  binwidth = 0.0001,
  breaks = NULL,
  freq = TRUE
)



fit.3.pp.stat.grouped <- ppc_stat_grouped(
  y = fit.3$data$lender_ij,
  yrep = fit.3.pp,
  group = factor(fit.3$data$village, levels = as.character( sort( as.numeric( levels(fit.3$data$village) ) ) ) ), 
  stat = "mean",
  facet_args = list(),
  binwidth = 0.0001,
  breaks = NULL,
  freq = TRUE
)



fit.4.pp.stat.grouped <- ppc_stat_grouped(
  y = fit.4$data$lender_ij,
  yrep = fit.4.pp,
  group = factor(fit.4$data$village, levels = as.character( sort( as.numeric( levels(fit.4$data$village) ) ) ) ), 
  stat = "mean",
  facet_args = list(),
  binwidth = 0.0001,
  breaks = NULL,
  freq = TRUE
)


fit.1.pp.stat.grouped + theme(text = element_text(family = "Arial", size = 14)) + labs(title = expression("(Model 1)"~Expected~Unconditional~Probability~of~italic(Y)[ijk][" = "][1][" = "][Money~Lender])) # https://stackoverflow.com/a/43010647
fit.2.pp.stat.grouped + theme(text = element_text(family = "Arial", size = 14)) + labs(title = expression("(Model 2)"~Expected~Unconditional~Probability~of~italic(Y)[ijk][" = "][1][" = "][Money~Lender]))
fit.3.pp.stat.grouped + theme(text = element_text(family = "Arial", size = 14)) + labs(title = expression("(Model 3)"~Expected~Unconditional~Probability~of~italic(Y)[ijk][" = "][1][" = "][Money~Lender]))
fit.4.pp.stat.grouped + theme(text = element_text(family = "Arial", size = 14)) + labs(title = expression("(Model 4)"~Expected~Unconditional~Probability~of~italic(Y)[ijk][" = "][1][" = "][Money~Lender]))



## Third, create model-specific pp-checks for the overall unconditional mean of seeking financial aid
## This is the graph that is inset within Figure 1 which depicts the coefficient plot depicting results from the models
fit.1.and.fit.2.and.fit.3.and.fit.4.pp.mean <- rbind.data.frame(cbind.data.frame(Model = "Full (Model 1)", Y_Synth_Mean = rowMeans(fit.1.pp) ),
                                                                cbind.data.frame(Model = "Controls (Model 2)", Y_Synth_Mean = rowMeans(fit.2.pp) ),
                                                                cbind.data.frame(Model = "Simple (Model 3)", Y_Synth_Mean = rowMeans(fit.3.pp) ),
                                                                cbind.data.frame(Model = "Extended (Model 4)", Y_Synth_Mean = rowMeans(fit.4.pp) )
)

fit.1.and.fit.2.and.fit.3.and.fit.4.pp.mean$Model <- factor(fit.1.and.fit.2.and.fit.3.and.fit.4.pp.mean$Model, levels = c("Full (Model 1)", "Controls (Model 2)", "Simple (Model 3)", "Extended (Model 4)"))



fit.1.and.fit.2.and.fit.3.and.fit.4.pp.stat.custom <- (ggplot(fit.1.and.fit.2.and.fit.3.and.fit.4.pp.mean, aes(x = Y_Synth_Mean, color = Model, fill = NULL)) # , linetype = Model
                                                       + geom_density(kernel = "gaussian", bw = 0.00003, position = "identity", size = 0.60, trim = TRUE) # See: https://r-coder.com/density-plot-r/
                                                       + geom_rug(sides = "b", alpha = 0.2, linetype = "solid") 
                                                       + geom_vline(xintercept = mean(fit.1$data$lender_ij), linetype = 2, size = 0.15) 
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
                                                               text = element_text(size = 8.25),
                                                               plot.title = element_text(size = 8.25),
                                                               strip.text = element_blank()
                                                       ) 
                                                       + scale_color_manual(values = c("#EE7733", "#009988", "#CC3311", "#33BBEE"))
                                                       + scale_x_continuous(limits = c(0.0088, 0.0100), breaks = seq(0.0088, 0.0100, 0.0002))
                                                       + labs(y = NULL,
                                                              x = expression(bar(italic(Y))[Synthetic]~"(Based on 500 Posterior Draws per Model [Rug Plot]; Bandwidth = 0.00003)"),
                                                              title = expression("Posterior Predictive Check:"~Unconditional~Probability~of~italic(Y)[ijk][" = "][1]),
                                                              subtitle = expression(bar(italic(Y))[Observed]~"(Dashed Line)"~vs.~"Gaussian Kernel Density Plots of"~bar(italic(Y))[Synthetic]~"Under Models 1, 2, 3, and 4") # https://stackoverflow.com/a/43010647
                                                       )
) 



## Fourth, create the modified version  of Figure 1 with the pp-check density plots inset
# https://stackoverflow.com/a/52841737
coefficient.plot.with.inset <- (ggdraw() 
                                + draw_plot(coefficient.plot) 
                                + draw_plot(fit.1.and.fit.2.and.fit.3.and.fit.4.pp.stat.custom, x = 0.625, y = .115, width = .355, height = .275)
)



# plot(coefficient.plot)
ggsave(plot = coefficient.plot.with.inset, 
       filename = "F1_Friends_Kin_FinancialSupport_Inset.png", device = "png", dpi = 1200,
       scale = 3, width = 4.75, height = 4.40, units = "in", bg = "transparent")




#################################### SUMMARISE CHAIN DIAGNOSTICS ####################################
######## Assessment of Effective Sample Sizes
## See: https://discourse.mc-stan.org/t/new-r-hat-and-ess/8165
summary(rstan::summary(fit.1$fit)$summary[,"n_eff"]) ## Standard ESS
summary(apply(brms:::as.array.brmsfit(fit.1), MARGIN = 3, FUN = rstan::ess_bulk)) ## Bulk (Rank-Normalised) ESS
summary(apply(brms:::as.array.brmsfit(fit.1), MARGIN = 3, FUN = rstan::ess_tail)) ## Tail (Rank-Normalised) ESS


summary(rstan::summary(fit.2$fit)$summary[,"n_eff"]) 
summary(apply(brms:::as.array.brmsfit(fit.2), MARGIN = 3, FUN = rstan::ess_bulk)) 
summary(apply(brms:::as.array.brmsfit(fit.2), MARGIN = 3, FUN = rstan::ess_tail)) 


summary(rstan::summary(fit.3$fit)$summary[,"n_eff"]) 
summary(apply(brms:::as.array.brmsfit(fit.3), MARGIN = 3, FUN = rstan::ess_bulk)) 
summary(apply(brms:::as.array.brmsfit(fit.3), MARGIN = 3, FUN = rstan::ess_tail))


summary(rstan::summary(fit.4$fit)$summary[,"n_eff"]) 
summary(apply(brms:::as.array.brmsfit(fit.4), MARGIN = 3, FUN = rstan::ess_bulk)) 
summary(apply(brms:::as.array.brmsfit(fit.4), MARGIN = 3, FUN = rstan::ess_tail))




######## Is Rhat (R̂) for all parameters (including random effects) less than 1.01?
# R̂ is the potential scale reduction factor on split chains.
## See: https://discourse.mc-stan.org/t/new-r-hat-and-ess/8165
table(rstan::summary(fit.1$fit)$summary[,"Rhat"] < 1.01)
table(rstan::summary(fit.2$fit)$summary[,"Rhat"] < 1.01)
table(rstan::summary(fit.3$fit)$summary[,"Rhat"] < 1.01)
table(rstan::summary(fit.4$fit)$summary[,"Rhat"] < 1.01)




######## Hamiltonian Monte Carlo Diagnostics
## Quick check of Bayesian Fraction of Missing Information (BFMI), divergent transitions and violation of the tree depth
## See: https://mc-stan.org/misc/warnings.html
rstan::check_hmc_diagnostics(fit.1$fit) 
rstan::check_hmc_diagnostics(fit.2$fit) 
rstan::check_hmc_diagnostics(fit.3$fit) 
rstan::check_hmc_diagnostics(fit.4$fit) 



