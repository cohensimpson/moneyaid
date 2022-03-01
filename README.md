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

package          version date       source  <br>
abind            1.4-5   2016-07-21 CRAN (R 4.1.0) <br>
assertthat       0.2.1   2019-03-21 CRAN (R 4.1.0) <br>
backports        1.4.1   2021-12-13 CRAN (R 4.1.1) <br>
base64enc        0.1-3   2015-07-28 CRAN (R 4.1.0) <br>
bayesplot      * 1.8.1   2021-06-14 CRAN (R 4.1.0) <br>
boot             1.3-28  2021-05-03 CRAN (R 4.1.2) <br>
bridgesampling   1.1-2   2021-04-16 CRAN (R 4.1.0) <br>
brio             1.1.3   2021-11-30 CRAN (R 4.1.1) <br>
brms           * 2.16.3  2021-11-22 CRAN (R 4.1.1) <br>
Brobdingnag      1.2-7   2022-02-03 CRAN (R 4.1.1) <br>
broom          * 0.7.12  2022-01-28 CRAN (R 4.1.1) <br>
broom.mixed    * 0.2.7   2021-07-07 CRAN (R 4.1.1) <br>
cachem           1.0.6   2021-08-19 CRAN (R 4.1.1) <br>
callr            3.7.0   2021-04-20 CRAN (R 4.1.0) <br>
car              3.0-12  2021-11-06 CRAN (R 4.1.1) <br>
carData          3.0-5   2022-01-06 CRAN (R 4.1.1) <br>
cellranger       1.1.0   2016-07-27 CRAN (R 4.1.0) <br>
checkmate        2.0.0   2020-02-06 CRAN (R 4.1.1) <br>
class            7.3-20  2022-01-13 CRAN (R 4.1.1) <br>
cli              3.2.0   2022-02-14 CRAN (R 4.1.1) <br>
cmdstanr         0.4.0   2022-02-03 local <br>
coda             0.19-4  2020-09-30 CRAN (R 4.1.0) <br>
codetools        0.2-18  2020-11-04 CRAN (R 4.1.2) <br>
colorspace     * 2.0-3   2022-02-21 CRAN (R 4.1.1) <br>
colourpicker     1.1.1   2021-10-04 CRAN (R 4.1.1) <br>
cowplot        * 1.1.1   2020-12-30 CRAN (R 4.1.1) <br>
crayon           1.5.0   2022-02-14 CRAN (R 4.1.1) <br>
crosstalk        1.2.0   2021-11-04 CRAN (R 4.1.1) <br>
curl             4.3.2   2021-06-23 CRAN (R 4.1.0) <br>
data.table       1.14.2  2021-09-27 CRAN (R 4.1.1) <br>
DBI              1.1.2   2021-12-20 CRAN (R 4.1.1) <br>
DEoptimR         1.0-10  2022-01-03 CRAN (R 4.1.1) <br>
desc             1.4.0   2021-09-28 CRAN (R 4.1.1) <br>
devtools         2.4.3   2021-11-30 CRAN (R 4.1.1) <br>
digest           0.6.29  2021-12-01 CRAN (R 4.1.1) <br>
distributional   0.3.0   2022-01-05 CRAN (R 4.1.1) <br>
dplyr            1.0.8   2022-02-08 CRAN (R 4.1.2) <br>
DT               0.21    2022-02-26 CRAN (R 4.1.2) <br>
dygraphs         1.1.1.6 2018-07-11 CRAN (R 4.1.0) <br>
e1071            1.7-9   2021-09-16 CRAN (R 4.1.1) <br>
ellipsis         0.3.2   2021-04-29 CRAN (R 4.1.0) <br>
emmeans          1.7.2   2022-01-04 CRAN (R 4.1.1) <br>
estimability     1.3     2018-02-11 CRAN (R 4.1.0) <br>
fansi            1.0.2   2022-01-14 CRAN (R 4.1.1) <br>
farver           2.1.0   2021-02-28 CRAN (R 4.1.0) <br>
fastmap          1.1.0   2021-01-25 CRAN (R 4.1.0) <br>
forcats          0.5.1   2021-01-27 CRAN (R 4.1.1) <br>
foreign          0.8-82  2022-01-13 CRAN (R 4.1.1) <br>
fs               1.5.2   2021-12-08 CRAN (R 4.1.1) <br>
gamm4            0.2-6   2020-04-03 CRAN (R 4.1.0) <br>
generics         0.1.2   2022-01-31 CRAN (R 4.1.1) <br>
ggplot2        * 3.3.5   2021-06-25 CRAN (R 4.1.1) <br>
ggridges         0.5.3   2021-01-08 CRAN (R 4.1.1) <br>
ggstance         0.3.5   2020-12-17 CRAN (R 4.1.0) <br>
glue             1.6.2   2022-02-24 CRAN (R 4.1.2) <br>
gridExtra        2.3     2017-09-09 CRAN (R 4.1.1) <br>
gtable           0.3.0   2019-03-25 CRAN (R 4.1.1) <br>
gtools           3.9.2   2021-06-06 CRAN (R 4.1.0) <br>
haven            2.4.3   2021-08-04 CRAN (R 4.1.1) <br>
hms              1.1.1   2021-09-26 CRAN (R 4.1.1) <br>
htmltools        0.5.2   2021-08-25 CRAN (R 4.1.1) <br>
htmlwidgets      1.5.4   2021-09-08 CRAN (R 4.1.1) <br>
httpuv           1.6.5   2022-01-05 CRAN (R 4.1.1) <br>
igraph           1.2.11  2022-01-04 CRAN (R 4.1.1) <br>
inline           0.3.19  2021-05-31 CRAN (R 4.1.0) <br>
jsonlite         1.8.0   2022-02-22 CRAN (R 4.1.1) <br>
jtools         * 2.1.4   2022-02-08 Github (jacob-long/jtools@e655636) <br>
knitr            1.37    2021-12-16 CRAN (R 4.1.1) <br>
labeling         0.4.2   2020-10-20 CRAN (R 4.1.0) <br>
laeken           0.5.2   2021-10-06 CRAN (R 4.1.1) <br>
later            1.3.0   2021-08-18 CRAN (R 4.1.1) <br>
lattice          0.20-45 2021-09-22 CRAN (R 4.1.2) <br>
lifecycle        1.0.1   2021-09-24 CRAN (R 4.1.1) <br>
lme4             1.1-28  2022-02-05 CRAN (R 4.1.2) <br>
lmtest           0.9-39  2021-11-07 CRAN (R 4.1.1) <br>
loo              2.4.1   2020-12-09 CRAN (R 4.1.0) <br>
magrittr         2.0.2   2022-01-26 CRAN (R 4.1.1) <br>
markdown         1.1     2019-08-07 CRAN (R 4.1.0) <br>
MASS             7.3-55  2022-01-13 CRAN (R 4.1.1) <br>
Matrix           1.4-0   2021-12-08 CRAN (R 4.1.1) <br>
matrixStats      0.61.0  2021-09-17 CRAN (R 4.1.1) <br>
memoise          2.0.1   2021-11-26 CRAN (R 4.1.1) <br>
mgcv             1.8-39  2022-02-24 CRAN (R 4.1.2) <br>
mime             0.12    2021-09-28 CRAN (R 4.1.1) <br>
miniUI           0.1.1.1 2018-05-18 CRAN (R 4.1.0) <br>
minqa            1.2.4   2014-10-09 CRAN (R 4.1.0) <br>
multcomp         1.4-18  2022-01-04 CRAN (R 4.1.1) <br>
munsell          0.5.0   2018-06-12 CRAN (R 4.1.0) <br>
mvtnorm          1.1-3   2021-10-08 CRAN (R 4.1.1) <br>
network        * 1.17.1  2021-06-14 CRAN (R 4.1.0) <br>
nlme             3.1-155 2022-01-13 CRAN (R 4.1.1) <br>
nloptr           2.0.0   2022-01-26 CRAN (R 4.1.1) <br>
nnet             7.3-17  2022-01-13 CRAN (R 4.1.1) <br>
openxlsx         4.2.5   2021-12-14 CRAN (R 4.1.1) <br>
pander           0.6.4   2021-06-13 CRAN (R 4.1.0) <br>
pillar           1.7.0   2022-02-01 CRAN (R 4.1.1) <br>
pkgbuild         1.3.1   2021-12-20 CRAN (R 4.1.1) <br>
pkgconfig        2.0.3   2019-09-22 CRAN (R 4.1.0) <br>
pkgload          1.2.4   2021-11-30 CRAN (R 4.1.1) <br>
plyr             1.8.6   2020-03-03 CRAN (R 4.1.0) <br>
posterior        1.2.0   2022-01-05 CRAN (R 4.1.1) <br>
prettyunits      1.1.1   2020-01-24 CRAN (R 4.1.0) <br>
processx         3.5.2   2021-04-30 CRAN (R 4.1.0) <br>
projpred         2.0.2   2020-10-28 CRAN (R 4.1.0) <br>
promises         1.2.0.1 2021-02-11 CRAN (R 4.1.0) <br>
proxy            0.4-26  2021-06-07 CRAN (R 4.1.0) <br>
ps               1.6.0   2021-02-28 CRAN (R 4.1.0) <br>
purrr            0.3.4   2020-04-17 CRAN (R 4.1.0) <br>
R6               2.5.1   2021-08-19 CRAN (R 4.1.1) <br>
ranger           0.13.1  2021-07-14 CRAN (R 4.1.0) <br>
Rcpp           * 1.0.8   2022-01-13 CRAN (R 4.1.1) <br>
RcppParallel     5.1.5   2022-01-05 CRAN (R 4.1.1) <br>
readxl           1.3.1   2019-03-13 CRAN (R 4.1.0) <br>
remotes          2.4.2   2021-11-30 CRAN (R 4.1.1) <br>
reshape          0.8.8   2018-10-23 CRAN (R 4.1.0) <br>
reshape2       * 1.4.4   2020-04-09 CRAN (R 4.1.0) <br>
rio            * 0.5.29  2021-11-22 CRAN (R 4.1.1) <br>
rlang            1.0.1   2022-02-03 CRAN (R 4.1.1) <br>
robustbase       0.93-9  2021-09-27 CRAN (R 4.1.1) <br>
rprojroot        2.0.2   2020-11-15 CRAN (R 4.1.0) <br>
rsconnect        0.8.25  2021-11-19 CRAN (R 4.1.1) <br>
rstan            2.26.6  2022-01-30 local <br>
rstantools       2.1.1   2020-07-06 CRAN (R 4.1.0) <br>
rstudioapi       0.13    2020-11-12 CRAN (R 4.1.0) <br>
sandwich         3.0-1   2021-05-18 CRAN (R 4.1.0) <br>
sbgcop         * 0.980   2018-05-29 CRAN (R 4.1.0) <br>
scales           1.1.1   2020-05-11 CRAN (R 4.1.0) <br>
sessioninfo      1.2.2   2021-12-06 CRAN (R 4.1.1) <br>
shiny            1.7.1   2021-10-02 CRAN (R 4.1.1) <br>
shinyjs          2.1.0   2021-12-23 CRAN (R 4.1.1) <br>
shinystan        2.5.0   2018-05-01 CRAN (R 4.1.0) <br>
shinythemes      1.2.0   2021-01-25 CRAN (R 4.1.0) <br>
sna            * 2.6     2020-10-06 CRAN (R 4.1.0) <br>
sp               1.4-6   2021-11-14 CRAN (R 4.1.1) <br>
StanHeaders      2.26.6  2022-01-30 local <br>
statnet.common * 4.5.0   2021-06-05 CRAN (R 4.1.0) <br>
stringi          1.7.6   2021-11-29 CRAN (R 4.1.1) <br>
stringr          1.4.0   2019-02-10 CRAN (R 4.1.1) <br>
survival         3.2-13  2021-08-24 CRAN (R 4.1.2) <br>
tensorA          0.36.2  2020-11-19 CRAN (R 4.1.0) <br>
testthat         3.1.2   2022-01-20 CRAN (R 4.1.1) <br>
TH.data          1.1-0   2021-09-27 CRAN (R 4.1.1) <br>
threejs          0.3.3   2020-01-21 CRAN (R 4.1.0) <br>
tibble           3.1.6   2021-11-07 CRAN (R 4.1.1) <br>
tidyr            1.2.0   2022-02-01 CRAN (R 4.1.1) <br>
tidyselect       1.1.2   2022-02-21 CRAN (R 4.1.1) <br>
usethis          2.1.5   2021-12-09 CRAN (R 4.1.1) <br>
utf8             1.2.2   2021-07-24 CRAN (R 4.1.0) <br>
V8               4.1.0   2022-02-06 CRAN (R 4.1.2) <br>
vcd              1.4-9   2021-10-18 CRAN (R 4.1.1) <br>
vctrs            0.3.8   2021-04-29 CRAN (R 4.1.0) <br>
VIM            * 6.1.1   2021-07-22 CRAN (R 4.1.1) <br>
withr            2.4.3   2021-11-30 CRAN (R 4.1.1) <br>
xfun             0.29    2021-12-14 CRAN (R 4.1.1) <br>
xtable           1.8-4   2019-04-21 CRAN (R 4.1.0) <br>
xts              0.12.1  2020-09-09 CRAN (R 4.1.0) <br>
zip              2.2.0   2021-05-31 CRAN (R 4.1.0) <br>
zoo              1.8-9   2021-03-09 CRAN (R 4.1.0) <br>

