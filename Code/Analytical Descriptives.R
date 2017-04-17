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

# also look at non dichotimized Food Security
analysis$'Food Security' <- factor(analysis$fsdad, exclude = NULL)
levels(analysis$`Food Security`) <- list('Fully Food Secure' = 1, 'Marginal Food Security' = 2, 
                                         'Low Food Security' = 3, 'Very Low Food Security' = 4, 
                                         'Missing' = NA)


table1 <- Table1(c('Gender', 'Race', 'Education', 'Income', 'Alcohol Use', 'Smoking Status', 
                   'Moderate Phys Act', 'Age'), 'Food Insecure', 
                 analysis[analysis$subset == T,], incl_missing = T)

table1_alt <- Table1(c('Gender', 'Race', 'Education', 'Income', 'Alcohol Use', 'Smoking Status', 
                       'Moderate Phys Act', 'Age'), 'Food Security', 
                     analysis[analysis$subset == T,], incl_missing = T)

#########################################################################################
############################ Table 2 Weighted Desriptives  ##############################
#########################################################################################

# Change all spaces to '.' since survey design does not like spaces
names(analysis) <- gsub(' ', '\\.', names(analysis))

# Setup binary Food Insecurity (exlude missing)
analysis$FoodInsecure <- factor(analysis$foodinsecure)
levels(analysis$FoodInsecure) <- list('Not Food Insecure' = 'FALSE', 
                                      'Food Insecure' = 'TRUE')

# Setup categorical Food Security (exlude missing)
analysis$FoodSecurity <- factor(analysis$fsdad)
levels(analysis$FoodSecurity) <- list('Fully Food Secure' = 1, 'Marginal Food Security' = 2, 
                                      'Low Food Security' = 3, 
                                      'Very Low Food Security' = 4)

library(survey)
design <- svydesign(id=~sdmvpsu, strata=~sdmvstra, weights=~samplewgt, 
                    nest=TRUE,data=analysis)

table2 <- Table1Weighted(c('Gender', 'Race', 'Education', 'Income', 'Alcohol.Use',
                                   'Smoking.Status', 'Moderate.Phys.Act', 'Age'),
                                 'FoodInsecure', design = subset(design, subset2 == T))

table2alt <- Table1Weighted(c('Gender', 'Race', 'Education', 'Income', 'Alcohol.Use',
                           'Smoking.Status', 'Moderate.Phys.Act', 'Age'),
                         'FoodSecurity', design = subset(design, subset2 == T))

# put spaces back in for '.'
rownames(table2) <- gsub('\\.', ' ', rownames(table2))

save(list = ls(pattern = 'table.*'), 
     file = '~/Repositories/Data/Capstone/finaldescriptives.rdata')
