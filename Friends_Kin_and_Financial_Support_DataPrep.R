################################# The Relational Bases of Informal Financial Cooperation
################################# Replication Code: Data Preparation


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




#################################### LOAD ATTRIBUTE DATA ####################################
#### Villager's Monadic Attribute Data from: Ferrali, R., Grossman, G., Platas, M. R., & Rodden, J. (2020). It Takes a Village: Peer Effects and Externalities in Technology Adoption. American Journal of Political Science, 64(3), 536–553. https://doi.org/10.1111/ajps.12471
survey_responses <- import("nodes.csv") ## Monadic data for each villager in the 16 villages where network data were collected
survey_responses$ID <- survey_responses$i
survey_responses$ID <- as.character(survey_responses$ID) ## Convert villagers' IDs to character strings 
survey_responses$i <- NULL

survey_responses$village_ID <- survey_responses$villageId
survey_responses$village_ID <- as.character(survey_responses$village_ID) ## Convert village IDs to character strings 
survey_responses$villageId <- NULL


village_IDs <- unique(survey_responses$village_ID) ## 16 villages with network data


survey_responses$gender <- survey_responses$female ## Rename "female" as "gender"


survey_responses <- survey_responses[c("village_ID", "ID", "gender", "age", "income", "hasPhone", "leader")] ## Retain only the actor attributes that we need
rownames(survey_responses) <- survey_responses$ID




#### Supplementary Attribute Data from: Ferrali, R., Grossman, G., Platas, M. R., & Rodden, J. (2021). Who Registers? Village Networks, Household Dynamics, and Voter Registration in Rural Uganda. Comparative Political Studies, 001041402110360. https://doi.org/10.1177/00104140211036048
survey_responses_CPS <- import("nodes_CPS_Version_1.2.csv")
survey_responses_CPS$ID <- survey_responses_CPS$i
survey_responses_CPS$ID <- as.character(survey_responses_CPS$ID) 
survey_responses_CPS$i <- NULL

survey_responses_CPS$village_ID <- survey_responses_CPS$villageId
survey_responses_CPS$village_ID <- as.character(survey_responses_CPS$village_ID) 
survey_responses_CPS$villageId <- NULL


survey_responses_CPS <- survey_responses_CPS[c("village_ID", "villageTxt", "ID", "hh", "edu_full", "head", "rlg", "eth")] 
rownames(survey_responses_CPS) <- survey_responses_CPS$ID


## Add supplementary variables from the data used by Ferrali et al. (2022) to the data used by Ferrali et al. (2020)
survey_responses$villageTxt <- survey_responses_CPS$villageTxt[match(survey_responses$ID, survey_responses_CPS$ID)] 
survey_responses$HH_ID <- survey_responses_CPS$hh[match(survey_responses$ID, survey_responses_CPS$ID)] 
survey_responses$edu_full <- survey_responses_CPS$edu_full[match(survey_responses$ID, survey_responses_CPS$ID)] 
survey_responses$HH_Head <- survey_responses_CPS$head[match(survey_responses$ID, survey_responses_CPS$ID)] 
survey_responses$religion <- survey_responses_CPS$rlg[match(survey_responses$ID, survey_responses_CPS$ID)] 
survey_responses$ethnicity <- survey_responses_CPS$eth[match(survey_responses$ID, survey_responses_CPS$ID)] 


survey_responses$HH_ID <- as.character(survey_responses$HH_ID)
survey_responses <- survey_responses[c("village_ID", "villageTxt", "ID", "HH_ID", "gender", "age", "edu_full", "income", "hasPhone", "leader", "HH_Head", "religion", "ethnicity")]




#################################### DESCRIPTION OF VILLAGER ATTRIBUTE DATA USED IN MODELS ####################################
# gender: 1 == Female; 0 Male
# age: Respondent's Age  
# edu_full: Level of schooling (No Schooling, Some Primary, Some Secondary, Some Tertiary)
# income: "In comparison to other typical households in this village, how would you describe your household’s economic situation?" Survey answers: 1 = "Much worse", 2 = "Somewhat worse", 3 = "About the same", 4 = "Somewhat better", 5 = "Much better"
# hasPhone: 1 = Owns Phone; 0 = Does Not Own Phone
# leader: 1 = Respondent occupies a formal leadership position within the village
# HH_Head: 1 = the head of the household;  0 for all other household members
# religion: 1 = Catholic (dominant local religion); 0 = Other religion
# ethnicity: 1 = Lugbara (dominant local ethnicity); 0 = Other ethnic group




#################################### RELABEL ORDINAL ATTRIBUTE DATA ####################################
survey_responses$edu_full[survey_responses$edu_full == "No schooling"] <- 0 
survey_responses$edu_full[survey_responses$edu_full == "Some Primary"] <- 1 
survey_responses$edu_full[survey_responses$edu_full == "Some Secondary"] <- 2
survey_responses$edu_full[survey_responses$edu_full == "Some Tertiary"] <- 3 
survey_responses$edu_full <- as.numeric(survey_responses$edu_full)

survey_responses$income[survey_responses$income == 1] <- -2 # "Much Worse" 
survey_responses$income[survey_responses$income == 2] <- -1 # "Somewhat Worse" 
survey_responses$income[survey_responses$income == 3] <- 0 # "About the Same" 
survey_responses$income[survey_responses$income == 4] <- 1 # "Somewhat Better" 
survey_responses$income[survey_responses$income == 5] <- 2 # "Much Better" 
survey_responses$income <- as.numeric(survey_responses$income)




#################################### LOAD NETWORK DATA ####################################
nominations <- import("ties.csv") ## Load all nomination data
nominations$village_ID <- nominations$villageId
nominations$village_ID <- as.character(nominations$village_ID) ## Convert IDs to character strings 
nominations$villageId <- NULL
nominations$i <- as.character(nominations$i) ## Convert IDs to character strings 
nominations$j <- as.character(nominations$j) ## Convert IDs to character strings 

nominations$ij_ID <- paste0(nominations$i, "_", nominations$j)


networks.of.study <- sort(unique( nominations$type )) ## The names of networks measured in the villages




#################################### IMPUTE ATTRIBUTE AND INCORPORATE DATA ####################################
# D. Hoff, P. (2007). Extending the Rank Likelihood for Semiparametric Copula Estimation. The Annals of Applied Statistics, 1(1). https://doi.org/10.1214/07-AOAS107
# Hollenbach, F. M., Bojinov, I., Minhas, S., Metternich, N. W., Ward, M. D., & Volfovsky, A. (2021). Multiple Imputation Using Gaussian Copulas. Sociological Methods & Research, 50(3), 1259–1283. https://doi.org/10.1177/0049124118799381
# https://cran.r-project.org/web/packages/sbgcop/
# https://cran.r-project.org/web/packages/VIM/index.html

print(aggr(survey_responses, plot = FALSE))
aggr(survey_responses, plot = TRUE, labels = TRUE)


survey_responses.imputation <- survey_responses
survey_responses.imputation$village_ID <- NULL ## Retain only binary, ordinal, and continuous variables
survey_responses.imputation$villageTxt <- NULL 
survey_responses.imputation$HH_ID <- NULL 
survey_responses.imputation$ID <- NULL 



cat("\nStart Imputation of Missing Values Using sbgcop.mcmc:\n")
survey_responses.imputation <- sbgcop.mcmc(Y = survey_responses.imputation, 
                                           nsamp = 200000,
                                           impute = TRUE,
                                           seed = 20200127, 
                                           verb = TRUE
)
print(summary(survey_responses.imputation))
plot.psgc(survey_responses.imputation)
par(mfrow = c(1, 1))



## Extract the imputed values which are the posterior mean values obtained using sbgcop.mcmc
gender.imputed.values <- survey_responses.imputation$Y.pmean[is.na(survey_responses$gender), "gender"] # 154 Missing
edu_full.imputed.values <- survey_responses.imputation$Y.pmean[is.na(survey_responses$edu_full), "edu_full"] # 4 Missing
income.imputed.values <- survey_responses.imputation$Y.pmean[is.na(survey_responses$income), "income"] # 8 Missing
hasPhone.imputed.values <- survey_responses.imputation$Y.pmean[is.na(survey_responses$hasPhone), "hasPhone"] # 2 Missing
HH_Head.imputed.values <- survey_responses.imputation$Y.pmean[is.na(survey_responses$HH_Head), "HH_Head"] # 275 Missing


survey_responses[names(gender.imputed.values), "gender"] <- ifelse(gender.imputed.values > 0.95, 1, 0)
survey_responses[names(edu_full.imputed.values), "edu_full"] <- round(edu_full.imputed.values)
survey_responses[names(income.imputed.values), "income"] <- income.imputed.values
survey_responses[names(hasPhone.imputed.values), "hasPhone"] <- ifelse(hasPhone.imputed.values > 0.95, 1, 0)
survey_responses[names(HH_Head.imputed.values), "HH_Head"] <- ifelse(HH_Head.imputed.values > 0.95, 1, 0)




#################################### LABEL CATEGORICAL ATTRIBUTE DATA ####################################
survey_responses$gender <- ifelse(survey_responses$gender == 1, "Female", "Male")

survey_responses$hasPhone <- ifelse(survey_responses$hasPhone == 1, "Yes", "No")
survey_responses$leader <- ifelse(survey_responses$leader == 1, "Yes", "No")
survey_responses$HH_Head <- ifelse(survey_responses$HH_Head == 1, "Yes", "No")

survey_responses$religion <- ifelse(survey_responses$religion == 1, "Catholic", "Not Catholic")
survey_responses$ethnicity <- ifelse(survey_responses$ethnicity == 1, "Lugbara", "Not Lugbara")




#################################### LOAD VILLAGE-LEVEL DATA ####################################
#### Variables with the "census_" prefix come from the 2014 Uganda National Population and Housing Census
villages <- import("villages.csv") 
villages$village_ID <- villages$villageId
villages$village_ID <- as.character(villages$village_ID) ## Convert village IDs to character strings 
villages$villageId <- NULL
villages <- subset(villages, villages$village_ID %in% village_IDs) ## Retain attributes only for those villages for which there is network data

villages$pg_market_any <- ifelse((villages$pg_market + villages$pg_market_crops) > 0, 1, 0)

villages <- villages[c("village_ID", "census_pop", "census_employ", "census_noAgri", "census_poverty", "pg_savingsgroup", "pg_market_any", "distArua")] ## Retain only the village attributes that we need




#################################### BUILD NETWORKS ####################################
village_networks_lending <- list()
village_networks_friendship <- list()
village_networks_kinship <- list()
village_networks_geodist <- list()
village_networks_goodsgame <- list()
village_networks_solver <- list()

village_networks_kinship_assymetric <- list() # Untransformed kinship nominations
village_networks_coresidence <- list()


for(v in village_IDs){  ## For each village with network data, construct the network of interest
   
  cat(paste0("\n", "Village ID: ", v, "\n"))
  
  villagers.data <- subset(survey_responses, survey_responses$village_ID == v)
  villagers <- villagers.data$ID ## Subset the monadic covariates for the surveyed villagers in "v" and retain their IDs
  villagers_nominations <- subset(nominations, nominations$i %in% villagers & nominations$j %in% villagers & nominations$village_ID == v) ## We only want intra-village measured relationships amongst those who were surveyed in village "v"
  
  
  
  ## LENDING NETWORK CONSTRUCTION ##
  # “Think about up to five people in this village that you would ask to 
  # borrow a significant amount of money if you had a personal emergency.”
  lending.matrix <- matrix(data = 0, nrow = length(villagers), ncol = length(villagers))
  rownames(lending.matrix) <- villagers
  colnames(lending.matrix) <- villagers
  
  lending_nominations <- subset(villagers_nominations, villagers_nominations$type == "lender") ## NB: "villagers_nominations" = nominations for all of the various measured network types. Here we sub-set for village "v"
  
  for(i in 1:nrow(lending_nominations)){
    
    source <- lending_nominations$i[i] ## Nominating villager (i.e., unique respondent identifier)
    target <- lending_nominations$j[i] ## Nominated villager (i.e., unique identifier of named alter)
    
    lending.matrix[which( rownames(lending.matrix) == source ), which( colnames(lending.matrix) == target )] <- 1
    
  }
  rm(i, source, target, lending_nominations)
  
  
  
  ## FRIENDSHIP NETWORK CONSTRUCTION ##
  # “Think about up to five of your best friends in this village. By friends I mean someone who will 
  # help you when you have a problem or who spends much of his or her free time with you. If there are less than five, that is okay too.”
  friendship.matrix <- matrix(data = 0, nrow = length(villagers), ncol = length(villagers))
  rownames(friendship.matrix) <- villagers
  colnames(friendship.matrix) <- villagers
  
  friendship_nominations <- subset(villagers_nominations, villagers_nominations$type == "friend") 
  
  for(i in 1:nrow(friendship_nominations)){
    
    source <- friendship_nominations$i[i] 
    target <- friendship_nominations$j[i] 
    
    friendship.matrix[which( rownames(friendship.matrix) == source ), which( colnames(friendship.matrix) == target )] <- 1
    
  }
  rm(i, source, target, friendship_nominations)
  
  
  
  ## CONSTRUCT CORESIDENCE MATRIX TO ADD TO "CLOSE" KINSHIP MATRIX ##
  coresidence <- data.frame()
  for(i in 1:nrow(villagers.data)){
    house <- villagers.data$HH_ID[i] ## In which house does i live?
    house.match <- villagers.data$HH_ID == house ## Match i's house against the vector of all residents' houses across the village
    house.match <- as.numeric(house.match) ## Convert the TRUE/FALSE vectors into zeros (FALSE) and ones (TRUE)
    
    coresidence <- rbind(coresidence, house.match) 
    #print(house.match)
    rm(i, house, house.match)
  }
  coresidence <- as.matrix(coresidence)
  colnames(coresidence) <- villagers
  rownames(coresidence) <- villagers
  diag(coresidence) <- 0 
  
  
  
  ## CONSTRUCT "CLOSE" KINSHIP MATRIX (ASSYMETRIC!) ##
  # “Think about up to five family members in this village not living in your household with whom you most frequently spend time. 
  # For instance, you might visit one another, eat meals together, or attend events together.”
  
  # Note that not all of the ties in each network constructed with the household membership data — i.e., the "coresidence" networks create above which include all possible asymmetric ties between people who live together — appear in the “family” nominations in “ties.csv”. 
  # As detailed in personal communication (i.e, emails) with Romain Ferrali (25 February 2022), the gentleman who collected the Ugandan data, there are three sources of information for kinship.
  # There is the baseline household membership data that Romain and his colleagues gathered prior to their full survey and cleaned ex-post.
  # There is the household membership data that Romain and his colleagues collected during the survey.
  # And, finally, there is the data on non-coresident kin elicited using the sociometric question for family/kinship (above).
  # According to Romain, the discrepancy between coresidents and the “family” nominations in “ties.csv” is likely due to his team failing to
  # properly clean the original/baseline household membership data by updating these data through the addition of all new household members discovered during their survey but who did not appear in the baseline household membership data.
  
  # Note that the “family” nominations in “ties.csv” already includes the connections with the household members uncovered by Romain and his colleagues during the survey. 
  # Accordingly, for my analysis for each village, I simply add the symmetric networks constructed with the “family” nominations in “ties.csv” to a symmetric network 
  # constructed using the original/baseline household membership data — i.e., coresidence networks that only include all possible asymmetric ties between people who live together according to "nodes_CPS_Version_1.2.csv". 
  kinship.matrix <- matrix(data = 0, nrow = length(villagers), ncol = length(villagers))
  rownames(kinship.matrix) <- villagers
  colnames(kinship.matrix) <- villagers
  
  kinship.nominations <- subset(villagers_nominations, villagers_nominations$type == "family")
  
  for(i in 1:nrow(kinship.nominations)){
    
    source <- kinship.nominations$i[i]
    target <- kinship.nominations$j[i] 
    
    kinship.matrix[which( rownames(kinship.matrix) == source ), which( colnames(kinship.matrix) == target )] <- 1
    
  }
  rm(i, source, target, kinship.nominations)
  
  
  ## Intersection of coresidence and unilateral nominations of kinship
  # print(table(kinship.matrix, coresidence)) 
  
  
  ## Create a binary network indicating a familial link when this link is acknowledge by either party involved
  kinship.matrix.assymetric <- kinship.matrix
  kinship.matrix <- sna::symmetrize(kinship.matrix, rule = "weak") 
  
  
  rownames(kinship.matrix) <- villagers ## symmetrize removes row/column names
  colnames(kinship.matrix) <- villagers
  
  kinship.matrix <- kinship.matrix + coresidence
  kinship.matrix[kinship.matrix > 0] <- 1
  
  
  
  ## CONSTRUCT MATRIX OF DISTANCES BETWEEN THE HOMES OF I AND J ##
  geodist.matrix <- matrix(data = 0, nrow = length(villagers), ncol = length(villagers))
  rownames(geodist.matrix) <- villagers
  colnames(geodist.matrix) <- villagers
  
  geographic_distance <- subset(villagers_nominations, villagers_nominations$type == "geo")
  
  for(i in 1:nrow(geographic_distance)){
    
    source <- geographic_distance$i[i] ## Villager i
    target <- geographic_distance$j[i] ## Villager j
    distance <- geographic_distance$dist[i]
    
    geodist.matrix[which( rownames(geodist.matrix) == source ), which( colnames(geodist.matrix) == target )] <- distance
    geodist.matrix[which( rownames(geodist.matrix) == target ), which( colnames(geodist.matrix) == source )] <- distance ## The distance data in villagers_nominations only includes unique dyads
    
  }
  rm(i, source, target, distance, geographic_distance)
  
  
  
  ## PUBLIC GOODS GAME NETWORK CONSTRUCTION ##
  # "To explore the mediating role of concentrated leadership, we conducted a modified public goods game in all sixteen villages. Following conventional practice, 
  # villagers were given an opportunity to contribute to the village any share of their survey participation remuneration, and the research team matched those 
  # contributions. In our version of the public goods game, villagers were asked to name which individual they would like to handle funds on behalf of the village, 
  # regardless of whether that individual holds formal leadership position."
  
  goodsgame.matrix <- matrix(data = 0, nrow = length(villagers), ncol = length(villagers))
  rownames(goodsgame.matrix) <- villagers
  colnames(goodsgame.matrix) <- villagers
  
  goodsgame_nominations <- subset(villagers_nominations, villagers_nominations$type == "Contgame") 
  
  for(i in 1:nrow(goodsgame_nominations)){
    
    source <- goodsgame_nominations$i[i] 
    target <- goodsgame_nominations$j[i] 
    
    goodsgame.matrix[which( rownames(goodsgame.matrix) == source ), which( colnames(goodsgame.matrix) == target )] <- 1
    
  }
  rm(i, source, target, goodsgame_nominations)
  
  
  
  ## PROBLEM SOLVER NETWORK CONSTRUCTION ##
  # “Imagine there is a problem with public services in this village. For example, you might imagine 
  # that a teacher has not come to school for several days or that a borehole in your village needs to
  # be repaired. Think about up to five people in this village whom you would be most likely to approach
  # to help solve these kinds of problems.”
  
  solver.matrix <- matrix(data = 0, nrow = length(villagers), ncol = length(villagers))
  rownames(solver.matrix) <- villagers
  colnames(solver.matrix) <- villagers
  
  solver_nominations <- subset(villagers_nominations, villagers_nominations$type == "solver")
  
  for(i in 1:nrow(solver_nominations)){
    
    source <- solver_nominations$i[i] 
    target <- solver_nominations$j[i]
    
    solver.matrix[which( rownames(solver.matrix) == source ), which( colnames(solver.matrix) == target )] <- 1
    
  }
  rm(i, source, target, solver_nominations)
  
  
  village_networks_lending[[v]] <- lending.matrix 
  village_networks_friendship[[v]] <- friendship.matrix 
  village_networks_kinship[[v]] <- kinship.matrix 
  village_networks_geodist[[v]] <- geodist.matrix 
  village_networks_goodsgame[[v]] <- goodsgame.matrix 
  village_networks_solver[[v]] <- solver.matrix 
  
  village_networks_kinship_assymetric[[v]] <- kinship.matrix.assymetric 
  village_networks_coresidence[[v]] <- coresidence 
  
  
}
rm(v, villagers, villagers_nominations, lending.matrix, friendship.matrix, coresidence, kinship.matrix, kinship.matrix.assymetric, geodist.matrix, goodsgame.matrix, solver.matrix)




#################################### BUILD VILLAGE-SPECIFIC DYADIC DATAFRAMES ####################################
village_dyads <- list()


for(v in village_IDs){ ## For each village with network data, construct the friendship network, the matrix of inter-household distances, and the matrix indicating kinship (unilateral acknoledgement)
  
  villagers_data <- subset(survey_responses, survey_responses$village_ID == v)
  villagers <- villagers_data$ID ## Pull out the monadic covariates for the surveyed villagers in "v"
  villagers_nominations <- subset(nominations, nominations$i %in% villagers & nominations$j %in% villagers & nominations$village_ID == v) ## We only want intra-village measured relationships amongst those who were surveyed in village "v"
  
  
  
  all_villager_pairs <- expand.grid(villagers_data$ID, villagers_data$ID, stringsAsFactors = FALSE) ## This generates all possible ordered pairs of villager IDs
  colnames(all_villager_pairs) <- c("j_ID", "i_ID")
  all_villager_pairs <- all_villager_pairs[c("i_ID", "j_ID")] ## Reverse order so that the long data frame is ordered by focal actor and then by all possible alters
  all_villager_pairs <- all_villager_pairs[-which(all_villager_pairs$i_ID == all_villager_pairs$j_ID), ] ## Remove same-self dyads or "self-loops"
  all_villager_pairs$ij_ID <- paste0(all_villager_pairs$i_ID, "_", all_villager_pairs$j_ID) ## Unique ID for each ordered dyad
  rownames(all_villager_pairs) <- all_villager_pairs$ij_ID
  
  ## https://stackoverflow.com/questions/52316998/create-unique-id-for-dyads-non-directional
  all_villager_pairs$ij_ID_unordered <- paste0(pmin(all_villager_pairs$i_ID, all_villager_pairs$j_ID), "_", pmax(all_villager_pairs$i_ID, all_villager_pairs$j_ID))
  
         
  all_villager_pairs$village <- v
  
  
  
  melted.lenders <- melt(village_networks_lending[[v]])
  colnames(melted.lenders) <- c("i_ID", "j_ID", "lender_ij")
  melted.lenders <- melted.lenders[-which(melted.lenders$i_ID == melted.lenders$j_ID), ] ## Remove same-self dyads or "self-loops"
  melted.lenders$ij_ID <- paste0(melted.lenders$i_ID, "_", melted.lenders$j_ID) ## Unique ID for each ordered dyad
  rownames(melted.lenders) <- melted.lenders$ij_ID
  
  
  melted.lenders.transpose <- melt(t(village_networks_lending[[v]])) ## Transpose to assess reciprocity (https://faculty.ucr.edu/~hanneman/nettext/C5_%20Matrices.html#transpose)
  colnames(melted.lenders.transpose) <- c("i_ID", "j_ID", "lender_ji")
  melted.lenders.transpose <- melted.lenders.transpose[-which(melted.lenders.transpose$i_ID == melted.lenders.transpose$j_ID), ] ## Remove same-self dyads or "self-loops"
  melted.lenders.transpose$ij_ID <- paste0(melted.lenders.transpose$i_ID, "_", melted.lenders.transpose$j_ID) ## Unique ID for each ordered dyad
  rownames(melted.lenders.transpose) <- melted.lenders.transpose$ij_ID
  
  
  melted.friends <- melt(village_networks_friendship[[v]])
  colnames(melted.friends) <- c("i_ID", "j_ID", "friend_ij")
  melted.friends <- melted.friends[-which(melted.friends$i_ID == melted.friends$j_ID), ]
  melted.friends$ij_ID <- paste0(melted.friends$i_ID, "_", melted.friends$j_ID)
  rownames(melted.friends) <- melted.friends$ij_ID
  
  
  melted.kin <- melt(village_networks_kinship[[v]])
  colnames(melted.kin) <- c("i_ID", "j_ID", "family_ij")
  melted.kin <- melted.kin[-which(melted.kin$i_ID == melted.kin$j_ID), ] 
  melted.kin$ij_ID <- paste0(melted.kin$i_ID, "_", melted.kin$j_ID) 
  rownames(melted.kin) <- melted.kin$ij_ID
  
  
  melted.distance <- melt(village_networks_geodist[[v]])
  colnames(melted.distance) <- c("i_ID", "j_ID", "distance_ij")
  melted.distance <- melted.distance[-which(melted.distance$i_ID == melted.distance$j_ID), ] 
  melted.distance$ij_ID <- paste0(melted.distance$i_ID, "_", melted.distance$j_ID) 
  rownames(melted.distance) <- melted.distance$ij_ID
  
  
  melted.goods <- melt(village_networks_goodsgame[[v]])
  colnames(melted.goods) <- c("i_ID", "j_ID", "goodsgame_ij")
  melted.goods <- melted.goods[-which(melted.goods$i_ID == melted.goods$j_ID), ] 
  melted.goods$ij_ID <- paste0(melted.goods$i_ID, "_", melted.goods$j_ID) 
  rownames(melted.goods) <- melted.goods$ij_ID
  
  
  melted.problems <- melt(village_networks_solver[[v]])
  colnames(melted.problems) <- c("i_ID", "j_ID", "problemsolver_ij")
  melted.problems <- melted.problems[-which(melted.problems$i_ID == melted.problems$j_ID), ] 
  melted.problems$ij_ID <- paste0(melted.problems$i_ID, "_", melted.problems$j_ID) 
  rownames(melted.problems) <- melted.problems$ij_ID
  
  
  
  ## Fast data merging using match()
  ## https://stackoverflow.com/questions/4322219/whats-the-fastest-way-to-merge-join-data-frames-in-r
  all_villager_pairs$lender_ij <- melted.lenders$lender_ij[match(all_villager_pairs$ij_ID, melted.lenders$ij_ID)]
  all_villager_pairs$lender_ji <- melted.lenders.transpose$lender_ji[match(all_villager_pairs$ij_ID, melted.lenders.transpose$ij_ID)]
  
  all_villager_pairs$friend_ij <- melted.friends$friend_ij[match(all_villager_pairs$ij_ID, melted.friends$ij_ID)] 
  all_villager_pairs$family_ij <- melted.kin$family_ij[match(all_villager_pairs$ij_ID, melted.kin$ij_ID)] 
  all_villager_pairs$distance_ij <- melted.distance$distance_ij[match(all_villager_pairs$ij_ID, melted.distance$ij_ID)] 
  all_villager_pairs$goodsgame_ij <- melted.goods$goodsgame_ij[match(all_villager_pairs$ij_ID, melted.goods$ij_ID)] 
  all_villager_pairs$problemsolver_ij <- melted.problems$problemsolver_ij[match(all_villager_pairs$ij_ID, melted.problems$ij_ID)] 
  
  all_villager_pairs$gender_i <- villagers_data$gender[match(all_villager_pairs$i_ID, villagers_data$ID)]
  all_villager_pairs$age_i <- villagers_data$age[match(all_villager_pairs$i_ID, villagers_data$ID)] 
  all_villager_pairs$edu_full_i <- villagers_data$edu_full[match(all_villager_pairs$i_ID, villagers_data$ID)]
  all_villager_pairs$income_i <- villagers_data$income[match(all_villager_pairs$i_ID, villagers_data$ID)] 
  all_villager_pairs$hasPhone_i <- villagers_data$hasPhone[match(all_villager_pairs$i_ID, villagers_data$ID)]
  all_villager_pairs$leader_i <- villagers_data$leader[match(all_villager_pairs$i_ID, villagers_data$ID)] 
  all_villager_pairs$HH_Head_i <- villagers_data$HH_Head[match(all_villager_pairs$i_ID, villagers_data$ID)] 
  all_villager_pairs$religion_i <- villagers_data$religion[match(all_villager_pairs$i_ID, villagers_data$ID)]

  all_villager_pairs$gender_j <- villagers_data$gender[match(all_villager_pairs$j_ID, villagers_data$ID)] 
  all_villager_pairs$age_j <- villagers_data$age[match(all_villager_pairs$j_ID, villagers_data$ID)]
  all_villager_pairs$edu_full_j <- villagers_data$edu_full[match(all_villager_pairs$j_ID, villagers_data$ID)]
  all_villager_pairs$income_j <- villagers_data$income[match(all_villager_pairs$j_ID, villagers_data$ID)]
  all_villager_pairs$hasPhone_j <- villagers_data$hasPhone[match(all_villager_pairs$j_ID, villagers_data$ID)]
  all_villager_pairs$leader_j <- villagers_data$leader[match(all_villager_pairs$j_ID, villagers_data$ID)]
  all_villager_pairs$HH_Head_j <- villagers_data$HH_Head[match(all_villager_pairs$j_ID, villagers_data$ID)] 
  all_villager_pairs$religion_j <- villagers_data$religion[match(all_villager_pairs$j_ID, villagers_data$ID)] 

  village_dyads[[v]] <- all_villager_pairs
  
}

rm(v, villagers_data, villagers, villagers_nominations, all_villager_pairs,
   melted.lenders, melted.lenders.transpose, melted.friends, melted.kin, melted.distance, melted.goods, melted.problems)


## Sanity Check to see if the Sociomatrix to Dyadic coversion above worked
Map(list, lapply(X = village_dyads, FUN = function(x){table(x$lender_ij)}), 
    lapply(X = village_networks_lending, FUN = function(x){table(x)}))

sum(unlist(lapply(X = village_dyads, FUN = function(x){nrow(x)}))) ## Ordered Dyads




#################################### BUILD GLOBAL DYADIC DATAFRAME ###################################
## Combine village specific data frame
all_village_dyads <- do.call(rbind.data.frame, village_dyads)
rownames(all_village_dyads) <- all_village_dyads$ij_ID


## Prepare dyadic variables for regression models
all_village_dyads$friend_ij <- ifelse(all_village_dyads$friend_ij == 1, "Best Friend", "Not a Best Friend")
all_village_dyads$friend_ij <- as.factor(all_village_dyads$friend_ij)
all_village_dyads$friend_ij <- relevel(all_village_dyads$friend_ij , ref = "Not a Best Friend")

all_village_dyads$family_ij <- ifelse(all_village_dyads$family_ij == 1, "Salient Kin", "Unrelated")
all_village_dyads$family_ij <- as.factor(all_village_dyads$family_ij)
all_village_dyads$family_ij <- relevel(all_village_dyads$family_ij , ref = "Unrelated")

all_village_dyads$lender_ji <- ifelse(all_village_dyads$lender_ji == 1, "Yes", "No")
all_village_dyads$lender_ji <- as.factor(all_village_dyads$lender_ji)
all_village_dyads$lender_ji <- relevel(all_village_dyads$lender_ji , ref = "No")

all_village_dyads$goodsgame_ij <- ifelse(all_village_dyads$goodsgame_ij == 1, "Preferred Money Handler", "Not Preferred Money Handler")
all_village_dyads$goodsgame_ij <- as.factor(all_village_dyads$goodsgame_ij)
all_village_dyads$goodsgame_ij <- relevel(all_village_dyads$goodsgame_ij , ref = "Not Preferred Money Handler")

all_village_dyads$problemsolver_ij <- ifelse(all_village_dyads$problemsolver_ij == 1, "Preferred Problem Solver", "Not Preferred Problem Solver")
all_village_dyads$problemsolver_ij <- as.factor(all_village_dyads$problemsolver_ij)
all_village_dyads$problemsolver_ij <- relevel(all_village_dyads$problemsolver_ij , ref = "Not Preferred Problem Solver")

all_village_dyads$same_gender_ij <- NA
all_village_dyads$same_gender_ij[all_village_dyads$gender_i == all_village_dyads$gender_j] <- "Same Gender"
all_village_dyads$same_gender_ij[all_village_dyads$gender_i != all_village_dyads$gender_j] <- "Different Gender"
all_village_dyads$same_gender_ij <- as.factor(all_village_dyads$same_gender_ij)
all_village_dyads$same_gender_ij <- relevel(all_village_dyads$same_gender_ij, ref = "Different Gender")

all_village_dyads$same_edu_full_ij <- NA
all_village_dyads$same_edu_full_ij[all_village_dyads$edu_full_i == all_village_dyads$edu_full_j] <- "Same Level of Education"
all_village_dyads$same_edu_full_ij[all_village_dyads$edu_full_i != all_village_dyads$edu_full_j] <- "Different Level of Education"
all_village_dyads$same_edu_full_ij <- as.factor(all_village_dyads$same_edu_full_ij)
all_village_dyads$same_edu_full_ij <- relevel(all_village_dyads$same_edu_full_ij, ref = "Different Level of Education")

all_village_dyads$same_religion_ij <- NA 
all_village_dyads$same_religion_ij[all_village_dyads$religion_i == all_village_dyads$religion_j] <- "Same Religion"
all_village_dyads$same_religion_ij[all_village_dyads$religion_i != all_village_dyads$religion_j] <- "Different Religion"
all_village_dyads$same_religion_ij <- as.factor(all_village_dyads$same_religion_ij)
all_village_dyads$same_religion_ij <- relevel(all_village_dyads$same_religion_ij, ref = "Different Religion")


all_village_dyads$log_distance_ij <- log(all_village_dyads$distance_ij)
all_village_dyads$log_distance_ij_Z <- scale(all_village_dyads$log_distance_ij, center = TRUE, scale = TRUE)


all_village_dyads$age_absdiff_ij_sqrt <- sqrt(abs(all_village_dyads$age_i - all_village_dyads$age_j))
all_village_dyads$age_absdiff_ij_sqrt_Z <- scale(all_village_dyads$age_absdiff_ij_sqrt, center = TRUE, scale = TRUE)



## Prepare monadic variables for regression models
## Covariates for ego/actor i
all_village_dyads$gender_i <- as.factor(all_village_dyads$gender_i)
all_village_dyads$gender_i <- relevel(all_village_dyads$gender_i, ref = "Male")

all_village_dyads$hasPhone_i <- as.factor(all_village_dyads$hasPhone_i)
all_village_dyads$hasPhone_i <- relevel(all_village_dyads$hasPhone_i, ref = "No")

all_village_dyads$leader_i <- as.factor(all_village_dyads$leader_i)
all_village_dyads$leader_i <- relevel(all_village_dyads$leader_i, ref = "No")

all_village_dyads$HH_Head_i <- as.factor(all_village_dyads$HH_Head_i)
all_village_dyads$HH_Head_i <- relevel(all_village_dyads$HH_Head_i, ref = "No")

all_village_dyads$religion_i <- as.factor(all_village_dyads$religion_i)
all_village_dyads$religion_i <- relevel(all_village_dyads$religion_i, ref = "Catholic")

all_village_dyads$age_i_Z <- scale(all_village_dyads$age_i, center = TRUE, scale = TRUE)

all_village_dyads$age_i_squared <- all_village_dyads$age_i^2 # https://stats.stackexchange.com/questions/264146/standardizing-quadratic-variables-in-linear-model
all_village_dyads$age_i_squared_Z <- scale(all_village_dyads$age_i_squared, center = TRUE, scale = TRUE)



## Covariates for alter/actor j
all_village_dyads$gender_j <- as.factor(all_village_dyads$gender_j)
all_village_dyads$gender_j <- relevel(all_village_dyads$gender_j, ref = "Male")

all_village_dyads$hasPhone_j <- as.factor(all_village_dyads$hasPhone_j)
all_village_dyads$hasPhone_j <- relevel(all_village_dyads$hasPhone_j, ref = "No")

all_village_dyads$leader_j <- as.factor(all_village_dyads$leader_j)
all_village_dyads$leader_j <- relevel(all_village_dyads$leader_j, ref = "No")

all_village_dyads$HH_Head_j <- as.factor(all_village_dyads$HH_Head_j)
all_village_dyads$HH_Head_j <- relevel(all_village_dyads$HH_Head_j, ref = "No")

all_village_dyads$religion_j <- as.factor(all_village_dyads$religion_j)
all_village_dyads$religion_j <- relevel(all_village_dyads$religion_j, ref = "Catholic")

all_village_dyads$age_j_Z <- scale(all_village_dyads$age_j, center = TRUE, scale = TRUE)

all_village_dyads$age_j_squared <- all_village_dyads$age_j^2 # https://stats.stackexchange.com/questions/264146/standardizing-quadratic-variables-in-linear-model
all_village_dyads$age_j_squared_Z <- scale(all_village_dyads$age_j_squared, center = TRUE, scale = TRUE)



## Prepare village-level variables for regression models
# Variables prefixed with "census_" come from the Republic of Uganda National Population and Housing Census 2014.
# Variables prefixed with "pg_" come from a baseline survey of public goods conducted in 2014.
all_village_dyads$village_population_size <- villages$census_pop[match(all_village_dyads$village, villages$village_ID)] # adult population
all_village_dyads$village_census_employ <- villages$census_employ[match(all_village_dyads$village, villages$village_ID)] # employment rate
all_village_dyads$village_census_noAgri <- villages$census_noAgri[match(all_village_dyads$village, villages$village_ID)] # percent employed in non-agriculture
all_village_dyads$village_census_poverty <- villages$census_poverty[match(all_village_dyads$village, villages$village_ID)] # poverty index

all_village_dyads$village_pg_savingsgroup <- villages$pg_savingsgroup[match(all_village_dyads$village, villages$village_ID)] # number of savings/community savings groups in village
all_village_dyads$village_pg_market_any <- villages$pg_market_any[match(all_village_dyads$village, villages$village_ID)] # 1 if a general market AND/OR a marketplace for crops located within village, 0 otherwise

all_village_dyads$village_distArua <- villages$distArua[match(all_village_dyads$village, villages$village_ID)] # distance to Arua in km


all_village_dyads$village_population_size_log <- log(all_village_dyads$village_population_size)
all_village_dyads$village_population_size_log_Z <- scale(all_village_dyads$village_population_size_log, center = TRUE, scale = TRUE)

all_village_dyads$village_census_employ_Z <- scale(all_village_dyads$village_census_employ, center = TRUE, scale = TRUE)

all_village_dyads$village_census_noAgri_Z <- scale(all_village_dyads$village_census_noAgri, center = TRUE, scale = TRUE)

all_village_dyads$village_census_poverty_Z <- scale(all_village_dyads$village_census_poverty, center = TRUE, scale = TRUE)

all_village_dyads$village_pg_market_any <- ifelse(all_village_dyads$village_pg_market_any == 1, "General or Farmers Market", "No Market")
all_village_dyads$village_pg_market_any <- as.factor(all_village_dyads$village_pg_market_any)
all_village_dyads$village_pg_market_any <- relevel(all_village_dyads$village_pg_market_any , ref = "No Market")

all_village_dyads$village_distArua_log <- log(all_village_dyads$village_distArua)
all_village_dyads$village_distArua_log_Z <- scale(all_village_dyads$village_distArua_log, center = TRUE, scale = TRUE)


## Prepare village-level reference for regression models
## Village 29 is the largest village based on survey respondents. 
## Village 80 is the largest by population size (according to the 2014 census)
all_village_dyads$village <- as.factor(all_village_dyads$village)
all_village_dyads$village <- relevel(all_village_dyads$village, ref = "29") 




#################################### DESCRIPTIVE STATISTICS ####################################
## Monadic Villager Covariates
summary(all_village_dyads$age_i) ## Years of age
summary(all_village_dyads$edu_full_i) ## Education level
summary(all_village_dyads$income_i) ## Perceived relative income

table(all_village_dyads$gender_i) ## Gender
table(all_village_dyads$hasPhone_i) ## Owner of Mobile Phone
table(all_village_dyads$HH_Head_i) ## Household Head
table(all_village_dyads$religion_i) ## Catholic (i.e., the dominant religion)
table(all_village_dyads$leader_i) ## Formal village leader


## Dyadic Covariates
table(all_village_dyads$same_gender_ij) ## Same Gender
table(all_village_dyads$same_edu_full_ij) ## Same Education Level
table(all_village_dyads$same_religion_ij) ## Same Religion

table(all_village_dyads$lender_ij) ## Lender of Ego
table(all_village_dyads$lender_ji) ## Lender of Alter
table(all_village_dyads$goodsgame_ij) ## Preferred Money Handler of Ego
table(all_village_dyads$problemsolver_ij) ## Preferred Problem Solver of Ego

summary(all_village_dyads$distance_ij) ## Geographic Distance Between Ego and Alter
summary(all_village_dyads$age_absdiff_ij_sqrt) ## sqrt(abs(Age of Ego - Age of Alter)) 


## Monadic Village Covariates
table(all_village_dyads$village_pg_market_any) ## Village Market (General or Farmers) 

summary(all_village_dyads$village_population_size) ## Village Population Size
summary(all_village_dyads$village_census_employ) ## Village Employment Rate
summary(all_village_dyads$village_census_noAgri) ## Village % Employed in Non-Agricultural Work
summary(all_village_dyads$village_census_poverty) ## Village Poverty Rate
summary(all_village_dyads$village_pg_savingsgroup) ## Number of Village Savings Groups
summary(all_village_dyads$village_distArua) ## Village Distance to Arua



## Village Response Rates
villages$village_network_size <- unlist(lapply(X = village_networks_lending, FUN = function(x){dim(x)[1]}))[villages$village_ID]
villages$village_percent_2014_population_surveyed <- villages$village_network_size/villages$census_pop
summary(villages$village_percent_2014_population_surveyed
        )



