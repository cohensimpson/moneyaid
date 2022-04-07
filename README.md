# The Relational Bases of Informal Financial Cooperation <br> (Simpson, In Prep.)


## Abstract
Access to money is vital for day-to-day human survival. But who do we turn to for the cash we need when formal avenues are unavailable, undesirable, or malicious? The evolutionary theory of kin selection and the network theory of multiplexity (link superimposition) suggest that financial aid will freely flow from friends and family. Yet, sociologists theorise that economic action is highly context-specific such that friendship and kinship need not systematically mix with money. Using data from 3,184 adults (680,378 dyads) in 16 poor Ugandan villages, I show that friends and kin are preferred money lenders over strangers, where non-kin friends outrank non-friend kin and friends-who-are-kin are most-probable patrons. Results are inconsistent with the sociological notion that money is not fungible across relational scenarios as friendship, kinship, and lending appear to be readily combined — possibly due to a lack of other viable strategies for informal financial cooperation amongst the poor.

<br>
<br>

![](https://github.com/cohensimpson/moneyaid/blob/main/F2_Friends_Kin_FinancialSupport.png "Fig. 2a. Predicted probability (Model 1; 1000 posterior draws; 95% quantile intervals) of ego (i) naming alter (j), as a money lender — i.e., p(y_ijk=1) when alter is a stranger, a Best Friend, Salient Kin, or a Best Friend who is Salient Kin for the typical dyad (i.e., i and j are both women aged 37, who live 651.38 metres apart in a village (k) with typical features, where neither i or j are leaders or a preferred money handler or fixer for the other). Fig. 2a. Predictive performance of Model 1 (see Figure 1) vs. Model 2 using leave-one-out cross-validation (LOO-CV) and pareto-smoothed importance sampling (PSIS). For each of the 680,378 ordered dyads ijk ∈ N, the bullets indicate the difference between the expected log pointwise predictive density (ELPD) of y_ijk — i.e., the log of p(y_ijk | y_(-ijk) ) — under Model 1 and under Model 2. ELPD obtained using 1000 draws from the posterior predictive distribution. Colours (chosen for legibility) in Fig. 2a denote the 16 villages. Nota bene, log_e⁡(x)-log_e⁡(y)=log_e⁡(x ÷ y). Fig. 2c. p(y_ijk=1) for the typical dyad vis-à-vis the relational bases of lending given interactions with patronage in Model 4 (Fig. 1).") <br><br> _(Panel a. & c.) Predicted probability (Model 1 (see Figure 1); 1000 posterior draws; 95% quantile intervals) of ego (i) naming a within-village alter (j) as a money lender when alter is a stranger, a Best Friend, Salient Kin, or a Best Friend who is Salient Kin for the global typical dyad. (Panel b.) Predictive performance of Model 1 (Full Model w/ Interactions) vs. Model 2 (Controls Only) using leave-one-out cross-validation (LOO-CV) and pareto-smoothed importance sampling (PSIS). (Hover over image for full caption.)_



## R Code
Here, you will find two R Scripts — i.e., "Friends_Kin_and_Financial_Support_DataPrep.R" and "Friends_Kin_and_Financial_Support_Analysis.R" — in addition to four ".csv" data files. Throughout the scripts, you will find code to carry out the analyses reported in my paper alongside commands used to produce useful print out (e.g., descriptive statistics, small tables, plots, etc.) and comments that (hopefully) give you insight into the thinking behind the decisions I take.

**_After_** you have placed the data files and the R scripts in the same R working directory, installed the necessary packages (see Lines 1-26 of R scripts for a list of all necessary packages), and set the number of available computing cores for your machine (see circa Lines 59-61 of the analysis R script), you should be able to simply hit the "source" button in RStudio or run "source("Friends_Kin_and_Financial_Support_Analysis.R")" to redo my analyses. This will also carry out all of the goodness-of-fit tests and generate the figures in the paper.

Finally, when re-running my analyses, some numerical results may differ slightly from those reported in the paper due to stochastic perturbations. I have used the same random seed (20200127) to ensure exact reproducibility wherever possible. However, this is not always an option depending on the function.


## Summary of Files in Repository
 1) Friends_Kin_and_Financial_Support_DataPrep.R (Script for Data Preparation and Transformation)
 
 2) Friends_Kin_and_Financial_Support_Analysis.R (Script for Analyses, Goodness-of-Fit, and Visualisation)

 3) nodes.csv (Monadic covariates for the individual villagers collected by Ferrali et al. (2020)[2] for their paper in the _American Journal of Political Science_ [2]) 

 4) nodes_CPS_Version_1.2.csv (Dataset on monadic covariates that includes household membership which was used by Ferrali et al. (2021)[3] for their paper in _Comparative Political Studies_) 

 5) ties.csv (Network data and dyadic covariates for the individual villagers collected by Ferrali et al. [2]) 

 6) villages.csv (Monadic Covariates for the 16 _villages_ collected by Ferrali et al. [2])  

 7) Ferrali_et_al_2020.pdf [2]
 
 8) Ferrali_et_al_2021.pdf [3]
 
 9) dataverse_files_Ferrali_et_al_AJPS_Version_1 (2019-08-07).zip (Ferrali et al.'s [1] [original replication materials](https://doi.org/10.7910/DVN/NOYBCQ))
 
 10) dataverse_files_Ferrali_et_al_CPS_Version_1.2 (2021-06-01).zip (Ferrali et al.'s [2] [original replication materials](https://doi.org/10.7910/DVN/YEFRPC))
 
 
## Key Citations for Replicators
[1] Simpson, C.R. In Prep. "The Relational Bases of Informal Financial Cooperation". Working Paper.

[2] Ferrali, R., Grossman, G., Platas, M. R., & Rodden, J. (2020). It Takes a Village: Peer Effects and Externalities in Technology Adoption. _American Journal of Political Science_, 64(3), 536–553. https://doi.org/10.1111/ajps.12471

[3] Ferrali, R., Grossman, G., Platas, M. R., & Rodden, J. (2021). Who Registers? Village Networks, Household Dynamics, and Voter Registration in Rural Uganda. _Comparative Political Studies_, 001041402110360. https://doi.org/10.1177/00104140211036048


## Notes
1) Thank you for your interest in my work! Please do let me know if something goes wrong. I am more than happy to help and you can always email me.

2) The estimation of the Bayesian multilevel models — which uses the fabulous package [BRMS](https://paul-buerkner.github.io/brms/) — does take some time (about five hours per model) and it is strongly recommend that replicators use multiple CPU cores and that they estimate the models using [cmdstanr](https://mc-stan.org/cmdstanr/) as a [backend for BRMS](https://paul-buerkner.github.io/brms/articles/brms_threading.html). For an introduction to BRMS, [see this tutorial on multilevel models](https://journal.r-project.org/archive/2018/RJ-2018-017/index.html)

3) Owing to the large size of the analysed dataset (680,378 records), I regularly faced memory issues/stuttering/sometimes fatal crashes when estimating the Bayesian multilevel models and when sampling from the posterior predictive distribution (e.g., for posterior predictive checks and leave-one-out cross-validation). To help with this, [I increased the available swap memory for R/RStudio](https://stackoverflow.com/a/52612921). And I judiciously managed the amount of working memory occupied by R objects by keeping only what is necessary in the R environment and using the garbage collector function [gc()](https://stackoverflow.com/a/8813862). Please keep this in mind when replicating my work (n.b., relevant prompts about memory throughout the analysis R script). Also, to help gauge whether you might run into memory issues, note that I carried out my research using the Apple M1 Max Macbook Pro which has 10 CPU cores and 32GB of Ram.

