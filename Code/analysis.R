#########################################################################################
############ This code contains all code used in the statisitcal analysis for the #######
############ capstone project ###########################################################
#########################################################################################

### Variables need for the final report are saved as analysis_report.rdata
### All analysis is done using the analysis data set created in Code/DataClean.R

library(survey)

load('~/Repositories/Data/Capstone/analysis.rda')

options(warn = 1)

#########################################################################################
##################### Set up factors for logistic regression ############################
#########################################################################################
analysis$FoodInsecure <- factor(analysis$foodinsecure)
levels(analysis$FoodInsecure) <- list('Not Food Insecure' = 'FALSE', 
                                      'Food Insecure' = 'TRUE')

# categorical food security include all cats
analysis$FoodSecurity <- factor(analysis$fsdad)
levels(analysis$FoodSecurity) <- list('Fully Food Secure' = 1, 'Marginal Food Security' = 2, 
                                         'Low Food Security' = 3, 
                                      'Very Low Food Security' = 4)

#########################################################################################
# set up dummy variables for age period graphs
analysis$FS1[analysis$fsdad != 1] <- 0
analysis$FS1[analysis$fsdad == 1] <- 1
analysis$FS2[analysis$fsdad != 2] <- 0
analysis$FS2[analysis$fsdad == 2] <- 1
analysis$FS3[analysis$fsdad != 3] <- 0
analysis$FS3[analysis$fsdad == 3] <- 1
analysis$FS4[analysis$fsdad != 4] <- 0
analysis$FS4[analysis$fsdad == 4] <- 1
table(analysis$fsdad, analysis$FS1, useNA = 'always')
table(analysis$fsdad, analysis$FS2, useNA = 'always')
table(analysis$fsdad, analysis$FS3, useNA = 'always')
table(analysis$fsdad, analysis$FS4, useNA = 'always')




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


#### get unadjusted prevalence for Metabolic Syndrome by Each Food Security  Cat##########
prevalence_alt <- sapply(levels(analysis$FoodSecurity), function(x) {
  p <- svyciprop(~syndrome, subset(design, subset2 == T & FoodSecurity == x))
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
                    value.name = "prevalence")
names(prevFI_melt) <- c("midpoint", "year", "prevalence")
prevFI_melt$NHANES <- factor(prevFI_melt$year, 
                             labels = c('1999 - 2000', '2001 - 2002', '2003 - 2004',
                                        '2005 - 2006', '2007 - 2008', '2009 - 2010',
                                        '2011 - 2012', '2013 - 2014'))


# repeat for food security categories
pFS1 <- sapply(c(1999,2001,2003,2005,2007,2009,2011,2013), 
               function(cyc) sapply(seq(25,55,10), 
                                    function(x) svyciprop(~FS1, subset(design, yr == cyc &
                                                                         agecat == x))))

pFS2 <- sapply(c(1999,2001,2003,2005,2007,2009,2011,2013), 
               function(cyc) sapply(seq(25,55,10), 
                                    function(x) svyciprop(~FS2, subset(design, yr == cyc &
                                                                         agecat == x))))
pFS3 <- sapply(c(1999,2001,2003,2005,2007,2009,2011,2013), 
               function(cyc) sapply(seq(25,55,10), 
                                    function(x) svyciprop(~FS3, subset(design, yr == cyc &
                                                                         agecat == x))))
pFS4 <- sapply(c(1999,2001,2003,2005,2007,2009,2011,2013), 
               function(cyc) sapply(seq(25,55,10), 
                                    function(x) svyciprop(~FS4, subset(design, yr == cyc &
                                                                         agecat == x))))

for(x in ls(pattern = 'pFS.*')) {
  df <- get(x)
  colnames(df) <- c(1999,2001,2003,2005,2007,2009,2011,2013)
  rownames(df) <- seq(25,55,10)
  assign(paste(x), df, .GlobalEnv)
}

for(x in ls(pattern = 'pFS.*')) {
  df <- get(x)
  melt <- melt(data = df, variable.name = "year", 
               value.name    = "prevalence")
  names(melt) <- c("midpoint", "year", "prevalence")
  melt$NHANES <- factor(melt$year, 
                        labels = c('1999 - 2000', '2001 - 2002', '2003 - 2004',
                                   '2005 - 2006', '2007 - 2008', '2009 - 2010',
                                   '2011 - 2012', '2013 - 2014'))
  assign(paste0(x, '_melt'), melt, .GlobalEnv)
}


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

p_FS2 <- ggplot(data = pFS2_melt, 
               aes(x = midpoint, y = prevalence, group = year, colour = NHANES)) + 
        labs(title = 'Marginal Food Security', x = 'Midpoint of Age',
             y = 'Prevalence') + 
        geom_point(size = 1) + geom_line() + 
        theme(legend.position = 'bottom', legend.text = element_text(size = 12), 
              legend.title = element_text(size =14))

p_FS3 <- ggplot(data = pFS3_melt, 
               aes(x = midpoint, y = prevalence, group = year, colour = NHANES)) + 
  labs(title = 'Low Food Security', x = 'Midpoint of Age',
       y = 'Prevalence') + 
  geom_point(size = 1) + geom_line()

p_FS4 <- ggplot(data = pFS4_melt, 
               aes(x = midpoint, y = prevalence, group = year, colour = NHANES)) + 
  labs(title = 'Very Low Food Security', x = 'Midpoint of Age',
       y = 'Prevalence') + 
  geom_point(size = 1) + geom_line()

#create legend - from 
# http://stackoverflow.com/questions/11883844/inserting-a-table-under-the-legend-in-a-ggplot2-histogram
g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)}
mylegend <- g_legend(p_FS2)


library(grid)
library(gridExtra)
title1=textGrob("Prevalence of Food Security Categories by Age and NHANES Cycle",
                gp=gpar(fontface="bold"))
grid.arrange(arrangeGrob(p_FS2 + theme(legend.position="none"),
                         p_FS3 + theme(legend.position="none"),
                         p_FS4 + theme(legend.position="none"),
                         nrow=3, ncol = 1),
             mylegend, nrow = 2, heights = c(14, 1), top = title1)


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

# full model with food security categories
summary(full_model2 <- svyglm(MetabolicSyndrome ~ FoodSecurity + Gender + ridageyr + 
                               Race + Education + Income + ModerateActivity + alcuse +
                               smoker + yr, design, subset = subset2 == T, 
                             family = quasibinomial()))



anova(full_model)
anova(full_model2)

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


# full model all covariates with Food Security Categories
summary(rrfull2 <- svyglm(MetabolicSyndrome ~ FoodSecurity + Gender + ridageyr + Race 
                         + Education + Income + ModerateActivity + alcuse + smoker + yr,
                         design, 
                         subset = subset2 == T, family = quasibinomial(log), 
                         start = c(-0.5, rep(-0,27)), maxit = 100))


# relative risk and CIs for exposure
# unadj 
RR_basic <- exp(c(rrbasic$coefficients[2], confint(rrbasic)[2,]))

#adj
RR_full <- exp(c(rrfull$coefficients[2], confint(rrfull)[2,]))

# categories
RR_full2 <- matrix(exp(c(rrfull2$coefficients[2:4], confint(rrfull2)[2:4,])), nrow = 3)

################ look for effect modifcication for Race, AgeCat, and Sex #################
summary(rrfullmodel <- svyglm(MetabolicSyndrome ~ FoodInsecure*Gender + 
                                FoodInsecure*ridageyr  + FoodInsecure*Race + Income + 
                                ModerateActivity + alcuse + smoker + yr + Education, 
                              design, subset = subset2 == T, family = quasibinomial(log), 
                              start = c(-0.5, rep(0,31)), maxit = 200))

# with Categorical Food Security
summary(rrfullmodelalt <- svyglm(MetabolicSyndrome ~ FoodSecurity*Gender + 
                                   FoodSecurity*ridageyr + FoodSecurity*Race + Income +
                                   ModerateActivity + alcuse + smoker + 
                                   yr + Education, design, subset = subset2 == T, 
                                 family = quasibinomial(log), 
                                 start = c(-0.5, rep(0,45)), maxit = 100))

#gender interaction seems significant

summary(rr_nogenderInteraction <- svyglm(MetabolicSyndrome ~ Gender + 
                                           FoodInsecure*ridageyr + FoodInsecure*Race +
                                           Education + Income + ModerateActivity + 
                                           alcuse + smoker + yr, design,
                                         subset = subset2 == T, family = quasibinomial(log),
                                         start = c(-0.5, rep(0,30))))

summary(rr_nogenderInteraction2 <- svyglm(MetabolicSyndrome ~ Gender + 
                                            FoodSecurity*ridageyr + FoodSecurity*Race +
                                            Education + Income + ModerateActivity + 
                                            alcuse + smoker + yr, design,
                                         subset = subset2 == T, family = quasibinomial(log),
                                         start = c(-0.5, rep(0,42))))

anova(rrfullmodel, rr_nogenderInteraction, force = T)
# p = <0.0001 significant


anova(rrfullmodelalt, rr_nogenderInteraction2, force = T)
# p = <0.0001 significant

RR_male <- exp(c(rrfullmodel$coefficients[2], confint(rrfullmodel)[2,]))
RR_male

RR_male_cat <- matrix(exp(c(rrfullmodelalt$coefficients[2:4], 
                          confint(rrfullmodelalt)[2:4,])), nrow = 3)

log_RR_female <- c(rrfullmodel$coefficients[2] + rrfullmodel$coefficients[3] + 
                     rrfullmodel$coefficients[27])
cov <- vcov(rrfullmodel)
SE_female <- sqrt(cov[2,2] + cov[3,3] + cov[27,27] + 
                    2*cov[2,3] + 2*cov[2,27] + 2*cov[3,27])
CI_female <- exp(c(log_RR_female - 1.96*SE_female, log_RR_female + 1.96*SE_female))
RR_female <- c(exp(log_RR_female), CI_female)
names(RR_female)[2:3] <- c('2.5%', '97.5%')
RR_female


RR_female_cat <- sapply(0:2, function(i) {
  log_RR_female <- c(rrfullmodelalt$coefficients[2+i] + rrfullmodelalt$coefficients[5+i] + 
                       rrfullmodelalt$coefficients[29+i])
  cov <- vcov(rrfullmodelalt)
  SE_female <- sqrt(cov[2+i,2+i] + cov[5+i,5+i] + cov[29+i,29+i] + 
                      2*cov[2+i, 5+i] + 2*cov[2+i,29+i] + 2*cov[5+i,29+i])
  CI_female <- exp(c(log_RR_female - 1.96*SE_female, log_RR_female + 1.96*SE_female))
  RR_female <- c(exp(log_RR_female), CI_female)
  names(RR_female)[2:3] <- c('2.5%', '97.5%')
  RR_female <- round(RR_female, 2)
  return(paste0(RR_female[1],"(", RR_female[2], '-', RR_female[3], ')'))
})




# save objects for report
save(list = c(ls(pattern = 'RR_.*'), ls(pattern = 'plot'), "prevalence", 'analysis'), 
     file = '~/Repositories/Data/Capstone/analysis_report.rdata')

