### This file generates the preliminary descriptives of the analysis data set. ###

load(file = '~/Repositories/Data/Capstone/analysis.rda')
source('~/Repositories/Capstone-Project/Code/Table1Unweighted.R')

library(plyr)
analysis <- rename(analysis, c('alcuse' = 'Alcohol Use', 'smoker' = 'Smoking Status',
                               'ModerateActivity' = 'Moderate Phys Act', 'ridageyr' = 'Age'))
analysis$'Food Insecure' <- factor(analysis$foodinsecure,
                                   levels = list('Not Insecure' = FALSE, 'Insecure' = TRUE, 'NA'),
                                   exclude = NULL)
analysis$`Food Insecure`[is.na(analysis$foodinsecure)] <- 'NA'
levels(analysis$`Food Insecure`) <- c('Not Insecure', 'Insecure', 'Missing')
table1 <- Table1(c('Gender', 'Race', 'Education', 'Income', 'Alcohol Use', 'Smoking Status', 
                   'Moderate Phys Act', 'Age'), 'Food Insecure', analysis[analysis$subset == T,])


missingmetabolic <- sapply(analysis[analysis$subset == T,c("bmxwaist", "bpxsar", "bpxdar", "lbxtr", "hdl", "lbxglu")],
                           function(x) sum(is.na(x))); 
missingtable <- data.frame('Missing' = missingmetabolic)
row.names(missingtable) <- c('Waist Circ', 'SBP', 'DBP', 'Trigly', 'HDL', 'Fasting Glu')

analysis$BMICAT <- cut(analysis$bmxbmi, c(0,18.5,25,30,100), 
                       labels = c('Underweight', 'Normal', 'Overweight', 'Obese'), 
                       right = F)
table3 <- table(analysis$BMICAT[analysis$subset == T], analysis$syndrome[analysis$subset ==T])
Table3 <- data.frame("No Metabolic Syndrome" = table3[,1], "Metabolic Syndrome" = table3[,2])
Table3[5,] <- apply(Table3, 2, sum)
Table3[1:4,1] <- paste(Table3[1:4,1], '(', round(Table3[1:4,1]/Table3[5,1] * 100, 0), ')', sep = '')
Table3[1:4,2] <- paste(Table3[1:4,2], '(', round(Table3[1:4,2]/Table3[5,2] * 100, 0), ')', sep = '')
Table3 <- Table3[-5,]
names(Table3) <- c('No Metabolic Syndrome', 'Metabolic Syndrome')

table4 <- table(analysis$syndrome[analysis$subset == T], analysis$`Food Insecure`[analysis$subset == T])
Table4 <- data.frame(table4[,1], table4[,2])
row.names(Table4) <- c('No Metabolic Syndrome', 'Metabolic Syndrome')
Table4[,3] <- apply(Table4, 1, sum)
Table4[,1] <- paste(Table4[,1], '(', round(Table4[,1]/Table4[,3] * 100, 0), ')', sep = '')
Table4[,2] <- paste(Table4[,2], '(', round(Table4[,2]/Table4[,3] * 100, 0), ')', sep = '')
names(Table4) <- c('Food Secure', 'Food Insecure', 'Total')

table5 <- table(analysis$fsdad[analysis$subset == T])
table5 <- paste(table5, '(', round(table5/sum(table5) * 100, 0), ')', sep = '')
names(table5) <- c('Fully Food Secure', 'Marginally Food Insecure',
                   'Food Insecure w/o Hunger', 'Food Insecure with Hunger')

save.image(file = '~/Repositories/Data/Capstone/descriptives.Rdata')
