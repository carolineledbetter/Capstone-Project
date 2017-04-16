### This file generates the final descriptives of the analysis data set. ###
####### All descriptives used in the final paper are created here ##########
####### The workspace for this is saved as finaldescriptives.rdata #########


load(file = '~/Repositories/Data/Capstone/analysis.rda')
source('~/Repositories/Capstone-Project/Code/Table1Unweighted.R')
source('~/Repositories/Capstone-Project/Code/Table1Weighted.R')


#########################################################################################
############################ Table 1 Unweighted Desriptives #############################
#########################################################################################
library(plyr)
#rename covariates so they're pretty
analysis <- rename(analysis, c('alcuse' = 'Alcohol Use', 'smoker' = 'Smoking Status',
                               'ModerateActivity' = 'Moderate Phys Act', 
                               'ridageyr' = 'Age'))

# Setup categorical Food Insecurity
analysis$'Food Insecure' <- factor(analysis$foodinsecure, exclude = NULL)
levels(analysis$`Food Insecure`) <- list('Not Food Insecure' = FALSE, 'Food Insecure' = TRUE, 
                                         'Missing' = NA)


table1 <- Table1(c('Gender', 'Race', 'Education', 'Income', 'Alcohol Use', 'Smoking Status', 
                   'Moderate Phys Act', 'Age'), 'Food Insecure', 
                 analysis[analysis$subset == T,], incl_missing = T)


#########################################################################################
############################ Table 2 Weighted Desriptives  ##############################
#########################################################################################

# Change all spaces to '.' since survey design does not like spaces
names(analysis) <- gsub(' ', '\\.', names(analysis))

# Setup categorical Food Insecurity (exlude missing)
analysis$FoodInsecure <- factor(analysis$foodinsecure)
levels(analysis$FoodInsecure) <- list('Not Food Insecure' = 'FALSE', 
                                      'Food Insecure' = 'TRUE')

library(survey)
design <- svydesign(id=~sdmvpsu, strata=~sdmvstra, weights=~samplewgt, 
                    nest=TRUE,data=analysis)

table2 <- Table1Weighted(c('Gender', 'Race', 'Education', 'Income', 'Alcohol.Use',
                                   'Smoking.Status', 'Moderate.Phys.Act', 'Age'),
                                 'FoodInsecure', design = subset(design, subset2 == T))

# put spaces back in for '.'
rownames(table2) <- gsub('\\.', ' ', rownames(table2))

save(list = ls(pattern = 'table.*'), 
     file = '~/Repositories/Data/Capstone/finaldescriptives.rdata')
