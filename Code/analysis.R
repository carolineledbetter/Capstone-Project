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
levels(analysis$FoodInsecure) <- list('Food Secure' = 'FALSE', 
                                      'Food Insecure' = 'TRUE')

# categorical food security include all cats
analysis$FoodSecurity <- factor(analysis$fsdad)
levels(analysis$FoodSecurity) <- list('Full Food Security' = 1, 'Marginal Food Security' = 2, 
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
                                              subset(design, yr == cyc & 
                                                       agecat == x & subset3 == T))))
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
                                                                 agecat == x &
                                                                 subset3 == T))))

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
                                                                         agecat == x &
                                                                         subset3 == T))))

pFS2 <- sapply(c(1999,2001,2003,2005,2007,2009,2011,2013), 
               function(cyc) sapply(seq(25,55,10), 
                                    function(x) svyciprop(~FS2, subset(design, yr == cyc &
                                                                         agecat == x &
                                                                         subset3 == T))))
pFS3 <- sapply(c(1999,2001,2003,2005,2007,2009,2011,2013), 
               function(cyc) sapply(seq(25,55,10), 
                                    function(x) svyciprop(~FS3, subset(design, yr == cyc &
                                                                         agecat == x &
                                                                         subset3 ==T))))
pFS4 <- sapply(c(1999,2001,2003,2005,2007,2009,2011,2013), 
               function(cyc) sapply(seq(25,55,10), 
                                    function(x) svyciprop(~FS4, subset(design, yr == cyc &
                                                                         agecat == x &
                                                                       subset3 == T))))

for(x in ls(pattern = 'pFS.*')) {
  df <- get(x)
  colnames(df) <- c(1999,2001,2003,2005,2007,2009,2011,2013)
  rownames(df) <- seq(25,55,10)
  assign(paste(x), df, .GlobalEnv)
}; remove(x)

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
}; remove(x)


################################ Make Graphs ############################################
library(ggplot2)

plotMSbyAGe <- ggplot(data = prevMS_melt, 
                      aes(x = midpoint, y = prevalence, group = year, colour = NHANES)) +
  labs(title = 'Metabolic Syndrome', x = NULL,
       y = NULL) +
  geom_point(size = 1) + geom_line() + theme(legend.key.size = unit(5, 'mm'))

plotMSbyAGe


plotFIbyAGe <- ggplot(data = prevFI_melt,
                      aes(x = midpoint, y = prevalence, group = year, colour = NHANES)) +
  labs(title = 'Food Insecurity', x = NULL, y = NULL) + 
  geom_point(size = 1) + geom_line()

plotFIbyAGe

#create legend - from 
# http://stackoverflow.com/questions/11883844/inserting-a-table-under-the-legend-in-a-ggplot2-histogram
g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)}
mylegend <- g_legend(plotMSbyAGe)

p_FS2 <- ggplot(data = pFS2_melt, 
               aes(x = midpoint, y = prevalence, group = year, colour = NHANES)) + 
        labs(title = 'Marginal Food Security', x = NULL,
             y = NULL) + 
        geom_point(size = 1) + geom_line() + 
        theme(legend.position = 'bottom', legend.text = element_text(size = 12), 
              legend.title = element_text(size =14))

p_FS3 <- ggplot(data = pFS3_melt, 
               aes(x = midpoint, y = prevalence, group = year, colour = NHANES)) + 
  labs(title = 'Low Food Security', x = NULL,
       y = NULL) + 
  geom_point(size = 1) + geom_line()

p_FS4 <- ggplot(data = pFS4_melt, 
               aes(x = midpoint, y = prevalence, group = year, colour = NHANES)) + 
  labs(title = 'Very Low Food Security', x = 'Midpoint of Age',
       y = NULL) + 
  geom_point(size = 1) + geom_line()

mylegend2 <- g_legend(p_FS2)


library(grid)
library(gridExtra)
title1=textGrob("Prevalence of Food Security Categories by Age and NHANES Cycle",
                gp=gpar(fontface="bold"))
grid.arrange(arrangeGrob(p_FS2 + theme(legend.position="none"),
                         p_FS3 + theme(legend.position="none"),
                         p_FS4 + theme(legend.position="none"),
                         nrow=3, ncol = 1),
             mylegend2, nrow = 2, heights = c(14, 1), top = title1)


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
               start = c(-0.5, rep(-0,26))))



# basic model <- exposure only  with Food Security Categories
summary(rrbasic2 <- svyglm(MetabolicSyndrome ~ FoodSecurity, design, subset = subset2 == T, 
                          family = quasibinomial(log), start = c(-0.5, rep(0,3))))

# full model all covariates with Food Security Categories
summary(rrfull2 <- svyglm(MetabolicSyndrome ~ FoodSecurity + Gender + ridageyr + Race 
                         + Education + Income + ModerateActivity + alcuse + smoker + yr,
                         design, 
                         subset = subset2 == T, family = quasibinomial(log), 
                         start = c(-0.5, rep(-0,28))))


# relative risk and CIs for exposure
# unadj 
RR_basic <- exp(c(rrbasic$coefficients[2], confint(rrbasic)[2,]))

#adj
RR_full <- exp(c(rrfull$coefficients[2], confint(rrfull)[2,]))

# categories
RR_basic2 <- matrix(exp(c(rrbasic2$coefficients[2:4], 
                            confint(rrbasic2)[2:4,])), nrow = 3)

RR_full2 <- matrix(exp(c(rrfull2$coefficients[2:4], confint(rrfull2)[2:4,])), nrow = 3)

################ look for effect modifcication for Race, AgeCat, and Sex #################
summary(rrfullmodel <- svyglm(MetabolicSyndrome ~ FoodInsecure*Gender + 
                                FoodInsecure*ridageyr  + FoodInsecure*Race + Income + 
                                ModerateActivity + alcuse + smoker + yr + Education, 
                              design, subset = subset2 == T, family = quasibinomial(log), 
                              start = c(-0.5, rep(0,32)), maxit = 50))

# with Categorical Food Security
summary(rrfullmodelalt <- svyglm(MetabolicSyndrome ~ FoodSecurity*Gender + 
                                   FoodSecurity*ridageyr + FoodSecurity*Race + Income +
                                   ModerateActivity + alcuse + smoker + 
                                   yr + Education, design, subset = subset2 == T, 
                                 family = quasibinomial(log), 
                                 start = c(-0.5, rep(0,46)), maxit = 100))

#gender interaction seems significant

summary(rr_nogenderInteraction <- svyglm(MetabolicSyndrome ~ Gender + 
                                           FoodInsecure*ridageyr + FoodInsecure*Race +
                                           Education + Income + ModerateActivity + 
                                           alcuse + smoker + yr, design,
                                         subset = subset2 == T, family = quasibinomial(log),
                                         start = c(-0.5, rep(0,31))))

summary(rr_nogenderInteraction2 <- svyglm(MetabolicSyndrome ~ Gender + 
                                            FoodSecurity*ridageyr + FoodSecurity*Race +
                                            Education + Income + ModerateActivity + 
                                            alcuse + smoker + yr, design,
                                         subset = subset2 == T, family = quasibinomial(log),
                                         start = c(-0.5, rep(0,43)), maxit = 50))

genderint <- anova(rrfullmodel, rr_nogenderInteraction, force = T)
# p = <0.0001 significant


# anova(rrfullmodelalt, rr_nogenderInteraction2, force = T)
# error

RR_male <- exp(c(rrfullmodel$coefficients[2], confint(rrfullmodel)[2,]))
RR_male

RR_male_cat <- matrix(exp(c(rrfullmodelalt$coefficients[2:4], 
                          confint(rrfullmodelalt)[2:4,])), nrow = 3)

log_RR_female <- c(rrfullmodel$coefficients[2] + rrfullmodel$coefficients[3] + 
                     rrfullmodel$coefficients[28])
cov <- vcov(rrfullmodel)
SE_female <- sqrt(cov[2,2] + cov[3,3] + cov[28,28] + 
                    2*cov[2,3] + 2*cov[2,28] + 2*cov[3,28])
CI_female <- exp(c(log_RR_female - 1.96*SE_female, log_RR_female + 1.96*SE_female))
RR_female <- c(exp(log_RR_female), CI_female)
names(RR_female)[2:3] <- c('2.5%', '97.5%')
RR_female


RR_female_cat <- sapply(0:2, function(i) {
  log_RR_female <- c(rrfullmodelalt$coefficients[2+i] + rrfullmodelalt$coefficients[5+i] + 
                       rrfullmodelalt$coefficients[30+i])
  cov <- vcov(rrfullmodelalt)
  SE_female <- sqrt(cov[2+i,2+i] + cov[5+i,5+i] + cov[30+i,30+i] + 
                      2*cov[2+i, 5+i] + 2*cov[2+i,30+i] + 2*cov[5+i,30+i])
  CI_female <- exp(c(log_RR_female - 1.96*SE_female, log_RR_female + 1.96*SE_female))
  RR_female <- c(exp(log_RR_female), CI_female)
  names(RR_female)[2:3] <- c('2.5%', '97.5%')
  RR_female <- round(RR_female, 2)
  return(paste0(RR_female[1],"(", RR_female[2], '-', RR_female[3], ')'))
})



#########################################################################################
################ Consider excluding alc use due to high missing #########################
#########################################################################################

#########################################################################################
##### get unadjusted prevalence for Metabolic Syndrome by +/- Food Insecurity ###########

#### on the advice of Dr Dabelea will stratify prevalence and crude RR by Gender
### old code is commented out
# prevalence_noalc <- sapply(levels(analysis$FoodInsecure), function(x) {
#   p <- svyciprop(~syndrome, subset(design, subset3 == T  & FoodInsecure == x))
#   paste0(sprintf('%.2f', p), '(', sprintf('%.2f',attr(p, 'ci')[1]), '-',
#          sprintf('%.2f', attr(p, 'ci')[2]), ')')
# })
# 
# 
# #### get unadjusted prevalence for Metabolic Syndrome by Each Food Security  Cat##########
# prevalence_alt_noalc <- sapply(levels(analysis$FoodSecurity), function(x) {
#   p <- svyciprop(~syndrome, subset(design, subset3 == T & FoodSecurity == x))
#   paste0(sprintf('%.2f', p), '(', sprintf('%.2f',attr(p, 'ci')[1]), '-',
#          sprintf('%.2f', attr(p, 'ci')[2]), ')')
# })

prevalence_noalc_male <- sapply(levels(analysis$FoodInsecure), function(x) {
  p <- svyciprop(~syndrome, subset(design, subset3 == T  & FoodInsecure == x & 
                                      Gender == 'Male'))
  paste0(sprintf('%.2f', p), '(', sprintf('%.2f',attr(p, 'ci')[1]), '-',
        sprintf('%.2f', attr(p, 'ci')[2]), ')')
})

prevalence_noalc_female <- sapply(levels(analysis$FoodInsecure), function(x) {
  p <- svyciprop(~syndrome, subset(design, subset3 == T  & FoodInsecure == x & 
                                     Gender == 'Female'))
  paste0(sprintf('%.2f', p), '(', sprintf('%.2f',attr(p, 'ci')[1]), '-',
         sprintf('%.2f', attr(p, 'ci')[2]), ')')
})

prevalence_noalc_male_100 <- sapply(levels(analysis$FoodInsecure), function(x) {
  p <- svyciprop(~syndrome, subset(design, subset3 == T  & FoodInsecure == x & 
                                     Gender == 'Male'))
  paste0(round(p * 100, 0), '(', round(attr(p, 'ci')[1] * 100, 0), '-',
         round(attr(p, 'ci')[2] * 100, 0), ')')
})

prevalence_noalc_female <- sapply(levels(analysis$FoodInsecure), function(x) {
  p <- svyciprop(~syndrome, subset(design, subset3 == T  & FoodInsecure == x & 
                                     Gender == 'Female'))
  paste0(sprintf('%.2f', p), '(', sprintf('%.2f',attr(p, 'ci')[1]), '-',
         sprintf('%.2f', attr(p, 'ci')[2]), ')')
})

prevalence_noalc_female_100 <- sapply(levels(analysis$FoodInsecure), function(x) {
  p <- svyciprop(~syndrome, subset(design, subset3 == T  & FoodInsecure == x & 
                                     Gender == 'Female'))
  paste0(round(p * 100, 0), '(', round(attr(p, 'ci')[1] * 100, 0), '-',
         round(attr(p, 'ci')[2] * 100, 0), ')')
})

#### get unadjusted prevalence for Metabolic Syndrome by Each Food Security  Cat##########
prevalence_alt_noalc_male <- sapply(levels(analysis$FoodSecurity), function(x) {
  p <- svyciprop(~syndrome, subset(design, subset3 == T & FoodSecurity == x & 
                                     Gender == 'Male'))
  paste0(sprintf('%.2f', p), '(', sprintf('%.2f',attr(p, 'ci')[1]), '-', 
         sprintf('%.2f', attr(p, 'ci')[2]), ')')
})

prevalence_alt_noalc_female <- sapply(levels(analysis$FoodSecurity), function(x) {
  p <- svyciprop(~syndrome, subset(design, subset3 == T & FoodSecurity == x & 
                                     Gender == 'Female'))
  paste0(sprintf('%.2f', p), '(', sprintf('%.2f',attr(p, 'ci')[1]), '-', 
         sprintf('%.2f', attr(p, 'ci')[2]), ')')
})

prevalence_alt_noalc_male_100 <- sapply(levels(analysis$FoodSecurity), function(x) {
  p <- svyciprop(~syndrome, subset(design, subset3 == T & FoodSecurity == x & 
                                     Gender == 'Male'))
  paste0(round(p * 100, 0), '(', round(attr(p, 'ci')[1] * 100, 0), '-', 
         round(attr(p, 'ci')[2] * 100, 0), ')')
})

prevalence_alt_noalc_female_100 <- sapply(levels(analysis$FoodSecurity), function(x) {
  p <- svyciprop(~syndrome, subset(design, subset3 == T & FoodSecurity == x & 
                                     Gender == 'Female'))
  paste0(round(p * 100, 0), '(', round(attr(p, 'ci')[1] * 100, 0), '-', 
         round(attr(p, 'ci')[2] * 100, 0), ')')
})
#########################################################################################
########################## relative risk regression #####################################
#########################################################################################

# summary(rrbasic_noalc <- svyglm(MetabolicSyndrome ~ FoodInsecure, design,
#                                 subset = subset3 == T,
#                                 family = quasibinomial(log),
#                                 start = c(-0.5, rep(0,1))))

summary(rrbasic_noalc <- svyglm(MetabolicSyndrome ~ FoodInsecure*Gender, design,
                                subset = subset3 == T,
                                family = quasibinomial(log),
                                start = c(-0.5, rep(0,3))))


# full model 
summary(rrnoalc <- svyglm(MetabolicSyndrome ~ FoodInsecure + Gender + ridageyr + Race 
                         + Education + Income + ModerateActivity +  smoker + yr,
                         design, 
                         subset = subset3 == T, family = quasibinomial(log), 
                         start = c(-0.5, rep(-0,24))))


# basic model <- exposure only  with Food Security Categories
# summary(rrbasic2_noalc <- svyglm(MetabolicSyndrome ~ FoodSecurity, design, 
#                                  subset = subset3 == T, 
#                                  family = quasibinomial(log), start = c(-0.5, rep(0,3))))
summary(rrbasic2_noalc <- svyglm(MetabolicSyndrome ~ FoodSecurity*Gender, design, 
                                 subset = subset3 == T, 
                                 family = quasibinomial(log), start = c(-0.5, rep(0,7))))

# full model all covariates with Food Security Categories
summary(rrnoalc2 <- svyglm(MetabolicSyndrome ~ FoodSecurity + Gender + ridageyr + Race 
                          + Education + Income + ModerateActivity + smoker + yr,
                          design, 
                          subset = subset3 == T, family = quasibinomial(log), 
                          start = c(-0.5, rep(-0,26))))
#crude
RR_basic_noalc <- matrix(NaN, nrow =2, ncol = 3, dimnames = 
                           list(c('Male', 'Female'), c('Est', '2.5%', '97.5%')))
# unadjusted male
RR_basic_noalc[1,] <- exp(c(rrbasic_noalc$coefficients[2], confint(rrbasic_noalc)[2,]))

# unadjusted female
log_RR_basic_noalc <- c(rrbasic_noalc$coefficients[2] + rrbasic_noalc$coefficients[3] + 
                          rrbasic_noalc$coefficients[4])
cov <- vcov(rrbasic_noalc)
SE_female_basic <- sqrt(cov[2,2] + cov[3,3] + cov[4,4] + 
                          2*cov[2,3] + 2*cov[2,4] + 2*cov[3,4])
CI_female_basic <- exp(c(log_RR_basic_noalc - 1.96*SE_female, 
                         log_RR_basic_noalc + 1.96*SE_female))
RR_basic_noalc[2,] <- c(exp(log_RR_basic_noalc), CI_female_basic)

#adj
RR_noalc <- exp(c(rrnoalc$coefficients[2], confint(rrnoalc)[2,]))
RR_noalc

# categories - crude
RR_basic_noalc2 <- matrix(NaN, nrow =2, ncol = 9, dimnames = 
                            list(c('Male', 'Female'), 
                                 c('Marginal', '2.5%', '97.5%',
                                   'Low', '2.5%', '97.5%',
                                   'Very Low', '2.5%', '97.5%')))
# unadjusted male
RR_basic_noalc2[1,] <- sapply(2:4, function(x) 
  exp(c(rrbasic2_noalc$coefficients[x], confint(rrbasic2_noalc)[x,])))
  
# unadjusted female
RR_basic_noalc2[2,] <- sapply(0:2, function(i) {
  log_RR_female <- c(rrbasic2_noalc$coefficients[2+i] + rrbasic2_noalc$coefficients[5] + 
                       rrbasic2_noalc$coefficients[6+i])
  cov <- vcov(rrbasic2_noalc)
  SE_female <- sqrt(cov[2+i,2+i] + cov[5,5] + cov[6+i,6+i] + 
                      2*cov[2+i, 5] + 2*cov[2+i,6+i] + 2*cov[5,6+i])
  CI_female <- exp(c(log_RR_female - 1.96*SE_female, log_RR_female + 1.96*SE_female))
  RR_female <- c(exp(log_RR_female), CI_female)
  names(RR_female) <- c('ARR', '2.5%', '97.5%')
  return(c(RR_female[1], RR_female[2], RR_female[3]))
})


################ look for effect modifcication for Race, AgeCat, and Sex #################
summary(rr_int_noalc <- svyglm(MetabolicSyndrome ~ FoodInsecure*Gender +
                                 FoodInsecure*ridageyr  + FoodInsecure*Race + Income +
                                 ModerateActivity + smoker + yr + Education,
                               design, subset = subset3 == T, family = quasibinomial(log),
                               start = c(-0.5, rep(0,30)), maxit = 50))


# with Categorical Food Security
summary(rr_int_noalc2 <- svyglm(MetabolicSyndrome ~ FoodSecurity*Gender + 
                                   FoodSecurity*ridageyr + FoodSecurity*Race + Income +
                                   ModerateActivity + smoker + 
                                   yr + Education, design, subset = subset3 == T, 
                                 family = quasibinomial(log), 
                                 start = c(-0.5, rep(0,44)), maxit = 50))

#gender interaction seems significant

summary(rr_nogenderint_noalc <- svyglm(MetabolicSyndrome ~ Gender + 
                                         FoodInsecure*ridageyr + FoodInsecure*Race + 
                                         Education + Income + ModerateActivity  + 
                                         smoker + yr, design, subset = subset3 == T, 
                                       family = quasibinomial(log), 
                                       start = c(-0.5, rep(0,29))))

summary(rr_nogenderint_noalc2 <- svyglm(MetabolicSyndrome ~ Gender + 
                                          FoodSecurity*ridageyr + FoodSecurity*Race + 
                                          Education + Income + ModerateActivity + 
                                          smoker + yr, design, subset = subset3 == T, 
                                        family = quasibinomial(log), 
                                        start = c(-0.5, rep(0,41)), maxit = 50))

anova(rr_int_noalc, rr_nogenderint_noalc, force = T)
# p = <0.0001 significant


# anova(rr_int_noalc2, rr_nogenderint_noalc2, force = T)
# error

RR_male_noalc <- exp(c(rr_int_noalc$coefficients[2], confint(rr_int_noalc)[2,]))
RR_male_noalc

RR_male_cat_noalc <- matrix(exp(c(rr_int_noalc2$coefficients[2:4], 
                                  confint(rr_int_noalc2)[2:4,])), nrow = 3)

log_RR_female_noalc <- c(rr_int_noalc$coefficients[2] + rr_int_noalc$coefficients[3] + 
                           rr_int_noalc$coefficients[26])
cov <- vcov(rr_int_noalc)
SE_female <- sqrt(cov[2,2] + cov[3,3] + cov[26,26] + 
                    2*cov[2,3] + 2*cov[2,26] + 2*cov[3,26])
CI_female_noalc <- exp(c(log_RR_female_noalc - 1.96*SE_female, 
                         log_RR_female_noalc + 1.96*SE_female))
RR_female_noalc <- c(exp(log_RR_female_noalc), CI_female_noalc)
names(RR_female_noalc)[2:3] <- c('2.5%', '97.5%')
RR_female_noalc


RR_female_cat_noalc <- sapply(0:2, function(i) {
  log_RR_female <- c(rr_int_noalc2$coefficients[2+i] + rr_int_noalc2$coefficients[5] + 
                       rr_int_noalc2$coefficients[28+i])
  cov <- vcov(rr_int_noalc2)
  SE_female <- sqrt(cov[2+i,2+i] + cov[5,5] + cov[28+i,28+i] + 
                      2*cov[2+i, 5] + 2*cov[2+i,28+i] + 2*cov[5,28+i])
  CI_female <- exp(c(log_RR_female - 1.96*SE_female, log_RR_female + 1.96*SE_female))
  RR_female <- c(exp(log_RR_female), CI_female)
  names(RR_female) <- c('ARR', '2.5%', '97.5%')
  return(c(RR_female[1], RR_female[2], RR_female[3]))
})

save(list = c(ls(pattern = 'RR_.*'), ls(pattern = 'plot'), ls(pattern = 'p_FS'), 
              ls(pattern = 'myleg.*'), ls(pattern = 'preval.*'), 'genderint', 
              'analysis'), 
     file = '~/Repositories/Data/Capstone/analysis_report.rdata')

