library(survey)

load('~/Repositories/Data/Capstone/analysis.rda')

########### Set up factors for logistic regression ###############

analysis$FoodInsecure <- factor(analysis$foodinsecure)
levels(analysis$FoodInsecure) <- list('Not Insecure' = 'FALSE', 'Insecure' = 'TRUE')
analysis$MetabolicSyndrome <- factor(analysis$syndrome)
levels(analysis$MetabolicSyndrome) <- list('No Metabolic Syndrome' = 'FALSE', 'Metabolic Syndrome' = 'TRUE')

###### Add categorical age, exclude under 20 and 60 and over ######
analysis$agecat <- cut(analysis$ridageyr, seq(20,60,10), 
                       labels = seq(25,55,10), 
                       right = F)

###### set up complex survey design object ######
design <- svydesign(id=~sdmvpsu, strata=~sdmvstra, weights=~samplewgt, 
                    nest=TRUE,data=analysis)

##### get unadjusted prevalence for Metabolic Syndrome by +/- Food Insecurity ##########
prevalence <- sapply(levels(analysis$FoodInsecure), function(x) {
  p <- svyciprop(~syndrome, subset(design, subset == T & FoodInsecure == x))
  paste0(sprintf('%.2f', p), '(', sprintf('%.2f',attr(p, 'ci')[1]), '-',
         sprintf('%.2f', attr(p, 'ci')[2]), ')')
  })


########################## logistical regression (odds ratios) ########################
# basic model - outcome and expousre only
summary(basic_model <- svyglm(MetabolicSyndrome ~ FoodInsecure, design, subset = subset == T, family = quasibinomial()))

# full model  - all covariates
summary(full_model <- svyglm(MetabolicSyndrome ~ FoodInsecure + Gender + ridageyr + Race + Education + Income + ModerateActivity + alcuse + smoker, design, subset = subset == T, family = quasibinomial()))

# two different ways to test for significance of variables - they gave similar answers #
anova(full_model)
sapply(list("FoodInsecure", "Gender", "Race", "Education", "Income", 'ModerateActivity', 'alcuse', 'smoker'), function(x) regTermTest(full_model, x, method = 'LRT'))




########################## relative risk regression #####################################
# basic model <- exposure only
summary(rrbasic <- svyglm(MetabolicSyndrome ~ FoodInsecure, design, subset = subset == T, family = quasibinomial(log), start = c(-0.5, rep(0,1))))

# full model all covariates
summary(rrfull <- svyglm(MetabolicSyndrome ~ FoodInsecure + Gender + ridageyr + Race + Education + Income + ModerateActivity + alcuse + smoker, design, subset = subset == T, family = quasibinomial(log), start = c(-0.5, rep(0,19))))

# relative risk and CIs for exposure
# unadj 
RR_basic <- exp(c(rrbasic$coefficients[2], confint(rrbasic)[2,]))

#adj
RR_full <- exp(c(rrfull$coefficients[2], confint(rrfull)[2,]))

################# look at age and period effects ##############################################

# get prevalence of metabolic syndrom by age and HNANES cycle
prevalenceMS <- sapply(c(1999,2001,2003,2005,2007,2009,2011,2013), function(cyc) sapply(seq(25,55,10), function(x) svyciprop(~syndrome, subset(design, yr == cyc & agecat == x))))
colnames(prevalenceMS) <- c(1999,2001,2003,2005,2007,2009,2011,2013)
rownames(prevalenceMS) <- seq(25,55,10)

# reshape data for graph
library(reshape2)
prevMS_melt <- melt(data = prevalenceMS, variable.name = "year", 
                  value.name    = "prevalence")
names(prevMS_melt) <- c("midpoint", "year", "prevalence")
prevMS_melt$NHANES <- factor(prevMS_melt$year, labels = c('1999 - 2000', '2001 - 2002', '2003 - 2004', 
                                                      '2005 - 2006', '2007 - 2008', '2009 - 2010',
                                                      '2011 - 2012', '2013 - 2014'))

# repeat for food insecurity
prevalenceFI <- sapply(c(1999,2001,2003,2005,2007,2009,2011,2013), function(cyc) sapply(seq(25,55,10), function(x) svyciprop(~foodinsecure, subset(design, yr == cyc & agecat == x))))
colnames(prevalenceFI) <- c(1999,2001,2003,2005,2007,2009,2011,2013)
rownames(prevalenceFI) <- seq(25,55,10)

prevFI_melt <- melt(data = prevalenceFI, variable.name = "year", 
                    value.name    = "prevalence")
names(prevFI_melt) <- c("midpoint", "year", "prevalence")
prevFI_melt$NHANES <- factor(prevFI_melt$year, labels = c('1999 - 2000', '2001 - 2002', '2003 - 2004', 
                                                          '2005 - 2006', '2007 - 2008', '2009 - 2010',
                                                          '2011 - 2012', '2013 - 2014'))


######## Make Graphs ########
library(ggplot2)

plotMSbyAGe <- ggplot(data = prevMS_melt, 
                      aes(x = midpoint, y = prevalence, group = year, colour = NHANES)) +
                labs(title = 'Prevalence of Metabolic Syndrome By Age
                     For Each NHANES Cycle', x = 'Midpoint of Age',
                     y = 'Prevalence of Metabolic Syndrome') +
                geom_point(size = 1) + geom_line()

plotMSbyAGe


plotFIbyAGe <- ggplot(data = prevFI_melt,
                      aes(x = midpoint, y = prevalence, group = year, colour = NHANES)) +
                labs(title = 'Prevalence of Food Insecurity By Age
                     For Each NHANES Cycle',
                     y = 'Prevalence of Food Insecurity') + 
                geom_point(size = 1) + geom_line()

plotFIbyAGe

################ look for effect modifcication for Race, AgeCat, and Sex #################

######### SEX ##########

# male only
summary(rrfull_male <- svyglm(MetabolicSyndrome ~ FoodInsecure + ridageyr + Race + Education + Income + ModerateActivity + alcuse + smoker, design, subset = subset == T & Gender == 'Male', family = quasibinomial(log), start = c(-0.5, rep(0,18))))

RR_male <- exp(rrfull_male$coefficients[2]) #1.026

# female only
summary(rrfull_female <- svyglm(MetabolicSyndrome ~ FoodInsecure + ridageyr + Race + Education + Income + ModerateActivity + alcuse + smoker, design, subset = subset == T & Gender == 'Female', family = quasibinomial(log), start = c(-0.5, rep(0,18))))

RR_female <- exp(rrfull_female$coefficients[2]) #1.205

######### RACE ##########
RR_Race <- sapply(levels(analysis$Race), function(x){
  subset <- subset(design, Race == x)
  model <- svyglm(MetabolicSyndrome ~ FoodInsecure + ridageyr + Gender + Education + Income 
                  + ModerateActivity + alcuse + smoker, subset, subset = subset == T, 
                  family = quasibinomial(log), start = c(-0.5, rep(0,15)))
  exp(c(model$coefficients[2], confint(model)[2,]))
  })

RR_Race

######## Age Cat #########
RR_Age <- sapply(levels(analysis$agecat), function(x){
  subset <- subset(design, agecat == x)
  model <- svyglm(MetabolicSyndrome ~ FoodInsecure + Gender + Race + Education + Income 
                  + ModerateActivity + alcuse + smoker, subset, subset = subset == T, 
                  family = quasibinomial(log), start = c(-0.5, rep(0,18)))
  exp(c(model$coefficients[2], confint(model)[2,]))
})

RR_Age

# save objects for report
save(list = c(ls(pattern = 'RR_.*'), ls(pattern = 'plot'), "prevalence"), 
     file = '~/Repositories/Data/Capstone/analysis_report.rdata')

