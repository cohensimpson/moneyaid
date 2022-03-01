# The Relational Bases of Informal Finance (Simpson, In Prep.)


## Abstract
Who do we turn to for the cash we need when formal avenues are unavailable, undesirable, or malicious? The evolutionary theory of kin selection and the network theory of multiplexity (link superimposition) suggest that financial aid will readily flow from friends and family. Yet, economic sociologists contend that finance is highly context-specific such that “unrelated friends”, “non-friend kin”, and “friends who are kin” are distinct relations that need not equally mix with money. Using data from 3,184 adults (680,378 dyads) in 16 poor Ugandan villages, I show that friends and kin are preferred money lenders versus strangers, however, “unrelated friends” outrank “non-friend kin” and “friends who are kin” are probable but less-desirable patrons. Findings point to the value of impersonal cash aid and suggest that combing friendship, kinship, and informal finance is not additive — presumably due to a redoubling of the shadow costs incurred when mixing money and intimate ties.


## R Code
Here, you will find two R Scripts — i.e., "Friends_Kin_and_Financial_Support_DataPrep.R" and "Friends_Kin_and_Financial_Support_Analysis.R" — in addition to four ".csv" data files. Throughout the scripts, you will find code to carry out the analyses reported in my paper alongside commands used to produce useful print out (e.g., descriptive statistics, small tables, plots, etc.) and comments that (hopefully) give you insight into the thinking behind the decisions I take.

**_After_** you have placed the data files and the R scripts in the same R working directory, installed the necessary packages (see below for a complete list of all necessary packages and their versions), and set the number of available computing cores for your machine (see circa Line 61 of the analysis R script), you should be able to simply hit the "source" button in RStudio or run "source("Friends_Kin_and_Financial_Support_Analysis.R")" to redo my analyses. This will also carry out all of the goodness-of-fit tests and generate the figures in the paper.

Finally, when re-running my analyses, some numerical results may differ slightly from those reported in the paper due to stochastic perturbations. I have used the same random seed (20200127) to ensure exact reproducibility wherever possible. However, this is not always an option depending on the function.


## Summary of Files in Repository
 1) Friends_Kin_and_Financial_Support_DataPrep.R (Script for Data Preparation and Transformation)
 
 2) Friends_Kin_and_Financial_Support_Analysis.R (Script for Analyses, Goodness-of-Fit, and Visualisation)

 3) nodes.csv (Monadic covariates for the individual villagers collected by Ferrali et al. (2020)[2] for their paper in the _American Journal of Political Science_ [2]) 

 4) nodes_CPS_Version_1.2.csv (Monadic covariates data that includes household membership which was used by Ferrali et al. (2021)[3] for their paper in _Comparative Political Studies_) 

 5) ties.csv (Network data and dyadic covariates for the individual villagers collected by Ferrali et al. [2]) 

 6) villages.csv (Monadic Covariates for the 16 _villages_ collected by Ferrali et al. [2])  

 7) Ferrali_et_al_2020.pdf [2]
 
 8) Ferrali_et_al_2020.pdf [3]
 
 9) dataverse_files_Ferrali_et_al_AJPS_Version_1 (2019-08-07).zip (Ferrali et al.'s [1] [original replication materials](https://doi.org/10.7910/DVN/NOYBCQ))
 
 10) dataverse_files_Ferrali_et_al_CPS_Version_1.2 (2021-06-01).zip (Ferrali et al.'s [2] [original replication materials](https://doi.org/10.7910/DVN/YEFRPC))
 
 
## Key Citations for Replicators
[1] Simpson, C.R. In Prep. "The Relational Bases of Informal Finance". Working Paper.

[2] Ferrali, R., Grossman, G., Platas, M. R., & Rodden, J. (2020). It Takes a Village: Peer Effects and Externalities in Technology Adoption. American Journal of Political Science, 64(3), 536–553. https://doi.org/10.1111/ajps.12471

[3] Ferrali, R., Grossman, G., Platas, M. R., & Rodden, J. (2021). Who Registers? Village Networks, Household Dynamics, and Voter Registration in Rural Uganda. Comparative Political Studies, 001041402110360. https://doi.org/10.1177/00104140211036048


## Notes
1) Thank you for your interest in my work! Please do let me know if something goes wrong. I am more than happy to help and you can always email me.

2) The estimation of the Bayesian multilevel models — which uses the fabulous package [BRMS](https://paul-buerkner.github.io/brms/) — does take some time and it is strongly recommend that replicators use multiple CPU cores and that they estimate the models using [cmdstanr](https://mc-stan.org/cmdstanr/) as a [backend for BRMS](https://paul-buerkner.github.io/brms/articles/brms_threading.html). For an introduction to BRMS, [see this tutorial on multilevel models](https://journal.r-project.org/archive/2018/RJ-2018-017/index.html)

3) Owing to the large size of the analysed dataset (680,378 records), I regularly faced memory issues/stuttering/sometimes fatal crashes when estimating the Bayesian multilevel models and when sampling from the posterior predictive distribution (e.g., for posterior predictive checks and leave-one-out cross-validation). To help with this, [I increased the available swap memory for R/RStudio](https://stackoverflow.com/a/52612921). And I judiciously managed the amount of working memory occupied by R objects by keeping only what is necessary in the R environment and using the garbage collector function [gc()](https://stackoverflow.com/a/8813862). Please keep this in mind when replicating my work. Also, to help gauge whether you might run into memory issues, do note that I carried out my research using the Apple M1 Max Macbook Pro which has 10 CPU cores and 32GB of Ram.


## R Packages Used in Analysis (* = Packaged Loaded Explicitly)


package          version date (UTC) lib source

abind            1.4-5   2016-07-21 [2] CRAN (R 4.1.0)

assertthat       0.2.1   2019-03-21 [2] CRAN (R 4.1.0)

backports        1.4.1   2021-12-13 [2] CRAN (R 4.1.1)

base64enc        0.1-3   2015-07-28 [2] CRAN (R 4.1.0)

bayesplot      * 1.8.1   2021-06-14 [2] CRAN (R 4.1.0)

boot             1.3-28  2021-05-03 [2] CRAN (R 4.1.2)

bridgesampling   1.1-2   2021-04-16 [2] CRAN (R 4.1.0)

brio             1.1.3   2021-11-30 [2] CRAN (R 4.1.1)

brms           * 2.16.3  2021-11-22 [1] CRAN (R 4.1.1)

Brobdingnag      1.2-7   2022-02-03 [2] CRAN (R 4.1.1)

broom          * 0.7.12  2022-01-28 [2] CRAN (R 4.1.1)

broom.mixed    * 0.2.7   2021-07-07 [1] CRAN (R 4.1.1)

cachem           1.0.6   2021-08-19 [2] CRAN (R 4.1.1)

callr            3.7.0   2021-04-20 [2] CRAN (R 4.1.0)

car              3.0-12  2021-11-06 [1] CRAN (R 4.1.1)

carData          3.0-5   2022-01-06 [1] CRAN (R 4.1.1)

cellranger       1.1.0   2016-07-27 [2] CRAN (R 4.1.0)

checkmate        2.0.0   2020-02-06 [2] CRAN (R 4.1.1)

class            7.3-20  2022-01-13 [2] CRAN (R 4.1.1)

cli              3.2.0   2022-02-14 [1] CRAN (R 4.1.1)

cmdstanr         0.4.0   2022-02-03 [1] local

coda             0.19-4  2020-09-30 [2] CRAN (R 4.1.0)

codetools        0.2-18  2020-11-04 [2] CRAN (R 4.1.2)

colorspace     * 2.0-3   2022-02-21 [1] CRAN (R 4.1.1)

colourpicker     1.1.1   2021-10-04 [2] CRAN (R 4.1.1)

cowplot        * 1.1.1   2020-12-30 [1] CRAN (R 4.1.1)

crayon           1.5.0   2022-02-14 [1] CRAN (R 4.1.1)

crosstalk        1.2.0   2021-11-04 [2] CRAN (R 4.1.1)

curl             4.3.2   2021-06-23 [2] CRAN (R 4.1.0)

data.table       1.14.2  2021-09-27 [2] CRAN (R 4.1.1)

DBI              1.1.2   2021-12-20 [2] CRAN (R 4.1.1)

DEoptimR         1.0-10  2022-01-03 [1] CRAN (R 4.1.1)

desc             1.4.0   2021-09-28 [2] CRAN (R 4.1.1)

devtools         2.4.3   2021-11-30 [1] CRAN (R 4.1.1)

digest           0.6.29  2021-12-01 [2] CRAN (R 4.1.1)

distributional   0.3.0   2022-01-05 [2] CRAN (R 4.1.1)

dplyr            1.0.8   2022-02-08 [2] CRAN (R 4.1.2)

DT               0.21    2022-02-26 [2] CRAN (R 4.1.2)

dygraphs         1.1.1.6 2018-07-11 [2] CRAN (R 4.1.0)

e1071            1.7-9   2021-09-16 [1] CRAN (R 4.1.1)

ellipsis         0.3.2   2021-04-29 [2] CRAN (R 4.1.0)

emmeans          1.7.2   2022-01-04 [1] CRAN (R 4.1.1)

estimability     1.3     2018-02-11 [1] CRAN (R 4.1.0)

fansi            1.0.2   2022-01-14 [2] CRAN (R 4.1.1)

farver           2.1.0   2021-02-28 [2] CRAN (R 4.1.0)

fastmap          1.1.0   2021-01-25 [2] CRAN (R 4.1.0)

forcats          0.5.1   2021-01-27 [2] CRAN (R 4.1.1)

foreign          0.8-82  2022-01-13 [2] CRAN (R 4.1.1)

fs               1.5.2   2021-12-08 [2] CRAN (R 4.1.1)

gamm4            0.2-6   2020-04-03 [2] CRAN (R 4.1.0)

generics         0.1.2   2022-01-31 [2] CRAN (R 4.1.1)

ggplot2        * 3.3.5   2021-06-25 [2] CRAN (R 4.1.1)

ggridges         0.5.3   2021-01-08 [2] CRAN (R 4.1.1)

ggstance         0.3.5   2020-12-17 [1] CRAN (R 4.1.0)

glue             1.6.2   2022-02-24 [1] CRAN (R 4.1.2)

gridExtra        2.3     2017-09-09 [2] CRAN (R 4.1.1)

gtable           0.3.0   2019-03-25 [2] CRAN (R 4.1.1)

gtools           3.9.2   2021-06-06 [2] CRAN (R 4.1.0)

haven            2.4.3   2021-08-04 [2] CRAN (R 4.1.1)

hms              1.1.1   2021-09-26 [2] CRAN (R 4.1.1)

htmltools        0.5.2   2021-08-25 [2] CRAN (R 4.1.1)

htmlwidgets      1.5.4   2021-09-08 [2] CRAN (R 4.1.1)

httpuv           1.6.5   2022-01-05 [2] CRAN (R 4.1.1)

igraph           1.2.11  2022-01-04 [1] CRAN (R 4.1.1)

inline           0.3.19  2021-05-31 [2] CRAN (R 4.1.0)

jsonlite         1.8.0   2022-02-22 [1] CRAN (R 4.1.1)

jtools         * 2.1.4   2022-02-08 [1] Github (jacob-long/jtools@e655636)

knitr            1.37    2021-12-16 [2] CRAN (R 4.1.1)

labeling         0.4.2   2020-10-20 [2] CRAN (R 4.1.0)

laeken           0.5.2   2021-10-06 [1] CRAN (R 4.1.1)

later            1.3.0   2021-08-18 [2] CRAN (R 4.1.1)

lattice          0.20-45 2021-09-22 [2] CRAN (R 4.1.2)

lifecycle        1.0.1   2021-09-24 [2] CRAN (R 4.1.1)

lme4             1.1-28  2022-02-05 [2] CRAN (R 4.1.2)

lmtest           0.9-39  2021-11-07 [1] CRAN (R 4.1.1)

loo              2.4.1   2020-12-09 [2] CRAN (R 4.1.0)

magrittr         2.0.2   2022-01-26 [2] CRAN (R 4.1.1)

markdown         1.1     2019-08-07 [2] CRAN (R 4.1.0)

MASS             7.3-55  2022-01-13 [2] CRAN (R 4.1.1)

Matrix           1.4-0   2021-12-08 [2] CRAN (R 4.1.1)

matrixStats      0.61.0  2021-09-17 [2] CRAN (R 4.1.1)

memoise          2.0.1   2021-11-26 [1] CRAN (R 4.1.1)

mgcv             1.8-39  2022-02-24 [2] CRAN (R 4.1.2)

mime             0.12    2021-09-28 [2] CRAN (R 4.1.1)

miniUI           0.1.1.1 2018-05-18 [2] CRAN (R 4.1.0)

minqa            1.2.4   2014-10-09 [2] CRAN (R 4.1.0)

multcomp         1.4-18  2022-01-04 [1] CRAN (R 4.1.1)

munsell          0.5.0   2018-06-12 [2] CRAN (R 4.1.0)

mvtnorm          1.1-3   2021-10-08 [2] CRAN (R 4.1.1)

network        * 1.17.1  2021-06-14 [1] CRAN (R 4.1.0)

nlme             3.1-155 2022-01-13 [2] CRAN (R 4.1.1)

nloptr           2.0.0   2022-01-26 [2] CRAN (R 4.1.1)

nnet             7.3-17  2022-01-13 [2] CRAN (R 4.1.1)

openxlsx         4.2.5   2021-12-14 [1] CRAN (R 4.1.1)

pander           0.6.4   2021-06-13 [1] CRAN (R 4.1.0)

pillar           1.7.0   2022-02-01 [2] CRAN (R 4.1.1)

pkgbuild         1.3.1   2021-12-20 [2] CRAN (R 4.1.1)

pkgconfig        2.0.3   2019-09-22 [2] CRAN (R 4.1.0)

pkgload          1.2.4   2021-11-30 [2] CRAN (R 4.1.1)

plyr             1.8.6   2020-03-03 [2] CRAN (R 4.1.0)

posterior        1.2.0   2022-01-05 [2] CRAN (R 4.1.1)

prettyunits      1.1.1   2020-01-24 [2] CRAN (R 4.1.0)

processx         3.5.2   2021-04-30 [2] CRAN (R 4.1.0)

projpred         2.0.2   2020-10-28 [2] CRAN (R 4.1.0)

promises         1.2.0.1 2021-02-11 [2] CRAN (R 4.1.0)

proxy            0.4-26  2021-06-07 [1] CRAN (R 4.1.0)

ps               1.6.0   2021-02-28 [2] CRAN (R 4.1.0)

purrr            0.3.4   2020-04-17 [2] CRAN (R 4.1.0)

R6               2.5.1   2021-08-19 [2] CRAN (R 4.1.1)

ranger           0.13.1  2021-07-14 [1] CRAN (R 4.1.0)

Rcpp           * 1.0.8   2022-01-13 [2] CRAN (R 4.1.1)

RcppParallel     5.1.5   2022-01-05 [2] CRAN (R 4.1.1)

readxl           1.3.1   2019-03-13 [2] CRAN (R 4.1.0)

remotes          2.4.2   2021-11-30 [1] CRAN (R 4.1.1)

reshape          0.8.8   2018-10-23 [1] CRAN (R 4.1.0)

reshape2       * 1.4.4   2020-04-09 [2] CRAN (R 4.1.0)

rio            * 0.5.29  2021-11-22 [1] CRAN (R 4.1.1)

rlang            1.0.1   2022-02-03 [1] CRAN (R 4.1.1)

robustbase       0.93-9  2021-09-27 [1] CRAN (R 4.1.1)

rprojroot        2.0.2   2020-11-15 [2] CRAN (R 4.1.0)

rsconnect        0.8.25  2021-11-19 [2] CRAN (R 4.1.1)

rstan            2.26.6  2022-01-30 [1] local

rstantools       2.1.1   2020-07-06 [2] CRAN (R 4.1.0)

rstudioapi       0.13    2020-11-12 [2] CRAN (R 4.1.0)

sandwich         3.0-1   2021-05-18 [1] CRAN (R 4.1.0)

sbgcop         * 0.980   2018-05-29 [1] CRAN (R 4.1.0)

scales           1.1.1   2020-05-11 [2] CRAN (R 4.1.0)

sessioninfo      1.2.2   2021-12-06 [1] CRAN (R 4.1.1)

shiny            1.7.1   2021-10-02 [2] CRAN (R 4.1.1)

shinyjs          2.1.0   2021-12-23 [2] CRAN (R 4.1.1)

shinystan        2.5.0   2018-05-01 [2] CRAN (R 4.1.0)

shinythemes      1.2.0   2021-01-25 [2] CRAN (R 4.1.0)

sna            * 2.6     2020-10-06 [1] CRAN (R 4.1.0)

sp               1.4-6   2021-11-14 [1] CRAN (R 4.1.1)

StanHeaders      2.26.6  2022-01-30 [1] local

statnet.common * 4.5.0   2021-06-05 [1] CRAN (R 4.1.0)

stringi          1.7.6   2021-11-29 [2] CRAN (R 4.1.1)

stringr          1.4.0   2019-02-10 [2] CRAN (R 4.1.1)

survival         3.2-13  2021-08-24 [2] CRAN (R 4.1.2)

tensorA          0.36.2  2020-11-19 [2] CRAN (R 4.1.0)

testthat         3.1.2   2022-01-20 [2] CRAN (R 4.1.1)

TH.data          1.1-0   2021-09-27 [1] CRAN (R 4.1.1)

threejs          0.3.3   2020-01-21 [2] CRAN (R 4.1.0)

tibble           3.1.6   2021-11-07 [2] CRAN (R 4.1.1)

tidyr            1.2.0   2022-02-01 [2] CRAN (R 4.1.1)

tidyselect       1.1.2   2022-02-21 [1] CRAN (R 4.1.1)

usethis          2.1.5   2021-12-09 [1] CRAN (R 4.1.1)

utf8             1.2.2   2021-07-24 [2] CRAN (R 4.1.0)

V8               4.1.0   2022-02-06 [1] CRAN (R 4.1.2)

vcd              1.4-9   2021-10-18 [1] CRAN (R 4.1.1)

vctrs            0.3.8   2021-04-29 [2] CRAN (R 4.1.0)

VIM            * 6.1.1   2021-07-22 [1] CRAN (R 4.1.1)

withr            2.4.3   2021-11-30 [2] CRAN (R 4.1.1)

xfun             0.29    2021-12-14 [2] CRAN (R 4.1.1)

xtable           1.8-4   2019-04-21 [2] CRAN (R 4.1.0)

xts              0.12.1  2020-09-09 [2] CRAN (R 4.1.0)

zip              2.2.0   2021-05-31 [1] CRAN (R 4.1.0)

zoo              1.8-9   2021-03-09 [2] CRAN (R 4.1.0)

