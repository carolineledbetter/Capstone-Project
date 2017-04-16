#########################################################################################
############ This code contains all code used in the statisitcal analysis for the #######
############ capstone project ###########################################################
#########################################################################################

### Variables need for the final report are saved as analysis_report.rdata
### All analysis is done using the analysis data set created in Code/DataClean.R

library(survey)

load('~/Repositories/Data/Capstone/analysis.rda')

#########################################################################################
##################### Set up factors for logistic regression ############################
#########################################################################################
analysis$FoodInsecure <- factor(analysis$foodinsecure)
levels(analysis$FoodInsecure) <- list('Not Food Insecure' = 'FALSE', 
                                      'Food Insecure' = 'TRUE')

analysis$MetabolicSyndrome <- factor(analysis$syndrome)
levels(analysis$MetabolicSyndrome) <- list('No Metabolic Syndrome' = 'FALSE', 
                                           'Metabolic Syndrome' = 'TRUE')

# create a factor for cycle year so it can be used as a categorical variable
analysis$yr <- factor(analysis$yr)


############### Add categorical age, exclude under 20 and 60 and over ###################
# only used to compare age and period effects so 18-20 and 60-65 don't matter, excluded 
# for simplicity

analysis$agecat <- cut(analysis$ridageyr, seq(20,60,10), 
                       labels = seq(25,55,10), 
                       right = F)

#########################################################################################
###################### set up complex survey design object ##############################
design <- svydesign(id=~sdmvpsu, strata=~sdmvstra, weights=~samplewgt, 
                    nest=TRUE,data=analysis)

#########################################################################################
##### get unadjusted prevalence for Metabolic Syndrome by +/- Food Insecurity ###########
prevalence <- sapply(levels(analysis$FoodInsecure), function(x) {
  p <- svyciprop(~syndrome, subset(design, subset2 == T & FoodInsecure == x))
  paste0(sprintf('%.2f', p), '(', sprintf('%.2f',attr(p, 'ci')[1]), '-',
         sprintf('%.2f', attr(p, 'ci')[2]), ')')
  })


#########################################################################################
################# look at age and period effects ########################################
#########################################################################################

# get prevalence of metabolic syndrom by age and HNANES cycle
prevalenceMS <- sapply(c(1999,2001,2003,2005,2007,2009,2011,2013), function(cyc) 
  sapply(seq(25,55,10), function(x) svyciprop(~syndrome, 
                                              subset(design, yr == cyc & agecat == x))))
colnames(prevalenceMS) <- c(1999,2001,2003,2005,2007,2009,2011,2013)
rownames(prevalenceMS) <- seq(25,55,10)

# reshape data for graph
library(reshape2)
prevMS_melt <- melt(data = prevalenceMS, variable.name = "year", 
                    value.name    = "prevalence")
names(prevMS_melt) <- c("midpoint", "year", "prevalence")
prevMS_melt$NHANES <- factor(prevMS_melt$year, 
                             labels = c('1999 - 2000', '2001 - 2002', '2003 - 2004',
                                        '2005 - 2006', '2007 - 2008', '2009 - 2010',
                                        '2011 - 2012', '2013 - 2014'))

# repeat for food insecurity
prevalenceFI <- sapply(c(1999,2001,2003,2005,2007,2009,2011,2013), 
                       function(cyc) sapply(seq(25,55,10), 
                                            function(x) 
                                              svyciprop(~foodinsecure, 
                                                        subset(design, yr == cyc & 
                                                                 agecat == x))))

colnames(prevalenceFI) <- c(1999,2001,2003,2005,2007,2009,2011,2013)
rownames(prevalenceFI) <- seq(25,55,10)

prevFI_melt <- melt(data = prevalenceFI, variable.name = "year", 
                    value.name    = "prevalence")
names(prevFI_melt) <- c("midpoint", "year", "prevalence")
prevFI_melt$NHANES <- factor(prevFI_melt$year, 
                             labels = c('1999 - 2000', '2001 - 2002', '2003 - 2004',
                                        '2005 - 2006', '2007 - 2008', '2009 - 2010',
                                        '2011 - 2012', '2013 - 2014'))

################################ Make Graphs ############################################
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
                     For Each NHANES Cycle', x = 'Midpoint of Age',
       y = 'Prevalence of Food Insecurity') + 
  geom_point(size = 1) + geom_line()

plotFIbyAGe

#### Food Insecurity has been increasing over time for all age groups, therefore ######
#### NHANES cycle will be adjusted for.                                          ######


#######################################################################################
########################## logistical regression (odds ratios) ########################
# basic model - outcome and expousre only
summary(basic_model <- svyglm(MetabolicSyndrome ~ FoodInsecure, design, 
                              subset = subset2 == T, family = quasibinomial()))

# full model  - all covariates
summary(full_model <- svyglm(MetabolicSyndrome ~ FoodInsecure + Gender + ridageyr + 
                               Race + Education + Income + ModerateActivity + alcuse +
                               smoker + yr, design, subset = subset2 == T, 
                             family = quasibinomial()))


anova(full_model)

#########################################################################################
# prevalence of metabolic syndrome is not rare ~30% so will use relative risk regression

#########################################################################################
########################## relative risk regression #####################################
#########################################################################################
# basic model <- exposure only
summary(rrbasic <- svyglm(MetabolicSyndrome ~ FoodInsecure, design, subset = subset2 == T, 
                          family = quasibinomial(log), start = c(-0.5, rep(0,1))))

# full model all covariates
summary(rrfull <- svyglm(MetabolicSyndrome ~ FoodInsecure + Gender + ridageyr + Race 
                          + Education + Income + ModerateActivity + alcuse + smoker + yr,
                          design, 
               subset = subset2 == T, family = quasibinomial(log), 
               start = c(-0.5, rep(-0,25)), maxit = 100))

# relative risk and CIs for exposure
# unadj 
RR_basic <- exp(c(rrbasic$coefficients[2], confint(rrbasic)[2,]))

#adj
RR_full <- exp(c(rrfull$coefficients[2], confint(rrfull)[2,]))


################ look for effect modifcication for Race, AgeCat, and Sex #################
summary(rrfullmodel <- svyglm(MetabolicSyndrome ~ FoodInsecure*Gender + 
                                FoodInsecure*ridageyr  + FoodInsecure*Race + Income + 
                                ModerateActivity + alcuse + smoker + yr + Education, 
                              design, subset = subset2 == T, family = quasibinomial(log), 
                              start = c(-0.5, rep(0,31)), maxit = 100))

#gender interaction seems significant

summary(rr_nogenderInteraction <- svyglm(MetabolicSyndrome ~ Gender + FoodInsecure*ridageyr 
                                         + FoodInsecure*Race + Education + Income + 
                                           ModerateActivity + alcuse + smoker + yr, design,
                                         subset = subset2 == T, family = quasibinomial(log),
                                         start = c(-0.5, rep(0,30))))

anova(rrfullmodel, rr_nogenderInteraction, force = T)
# p = <0.0001 significant

RR_male <- exp(c(rrfullmodel$coefficients[2], confint(rrfullmodel)[2,]))
RR_male

log_RR_female <- c(rrfullmodel$coefficients[2] + rrfullmodel$coefficients[3] + 
                     rrfullmodel$coefficients[27])
cov <- vcov(rrfullmodel)
SE_female <- sqrt(cov[2,2] + cov[3,3] + cov[27,27] + 
                    2*cov[2,3] + 2*cov[2,27] + 2*cov[3,27])
CI_female <- exp(c(log_RR_female - 1.96*SE_female, log_RR_female + 1.96*SE_female))

RR_female <- c(exp(log_RR_female), CI_female)
names(RR_female)[2:3] <- c('2.5%', '97.5%')
RR_female


# save objects for report
save(list = c(ls(pattern = 'RR_.*'), ls(pattern = 'plot'), "prevalence", 'analysis'), 
     file = '~/Repositories/Data/Capstone/analysis_report.rdata')

