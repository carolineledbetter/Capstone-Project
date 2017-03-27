load(file = '~/Repositories/Data/Capstone/analysis.rda')
source('~/Repositories/Capstone-Project/Code/Table1Weighted.R')

analysis$FoodInsecure <- factor(analysis$foodinsecure)
levels(analysis$FoodInsecure) <- list('Insecure' = 'TRUE', 'Not Insecure' = 'FALSE')
analysis$'Metabolic Syndrome' <- factor(analysis$syndrome)
levels(analysis$`Metabolic Syndrome`) <- list('Metabolic Syndrome' = 'TRUE', 'No Metabolic Syndrome' = 'FALSE')

library(survey)
design <- svydesign(id=~sdmvpsu, strata=~sdmvstra, weights=~samplewgt, 
                    nest=TRUE,data=analysis)
remove(analysis)

table1Weighted <- Table1Weighted(c('Gender', 'Race', 'Education', 'Income', 
                                   'alcuse', 'smoker', 'ModerateActivity', 
                                   'ridageyr'), 'FoodInsecure', design = subset(design, subset == T))
save(table1Weighted, file = '~/Repositories/Data/Capstone/table1weighted.rda')
