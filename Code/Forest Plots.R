#########################################################################################
##################### This script makes forest plots of the results #####################
#########################################################################################

# This script makes forest plots of the results for use in the presentation.

#load results
load(file = '~/Repositories/Data/Capstone/analysis_report.rdata')


#########################################################################################
##################################### Relative Risk #####################################
#########################################################################################

#########################################################################################
## set names for relative risk forest plots
names <- c(' ', 'Crude Risk Ratio', 
           'Insecure (vs. Secure)', 'Marginal Security (vs. Full)',  
           'Low Security (vs. Full)',  'Very Low Security (vs. Full)', 
           'Adjusted Risk Ratio', 
           'Insecure (vs. Secure)', 'Marginal Security (vs. Full)',  
           'Low Security (vs. Full)',  'Very Low Security (vs. Full)')

#########################################################################################
## Create List for estimates w/ male and female
## Need to blanks at the top for labels and one between crude and Adjusted

RR_est <- list()
RR_est$Male <- c(NaN, NaN, RR_basic_noalc[1,1], RR_basic_noalc2[1,c(1,4,7)], 
                 NaN, RR_male_noalc[1], RR_male_cat_noalc[,1])
names(RR_est$Male) <-  names

RR_est$Female <- c(NaN, NaN, RR_basic_noalc[2,1], RR_basic_noalc2[2,c(1,4,7)], 
                   NaN, RR_female_noalc[1], RR_female_cat_noalc[1,])
names(RR_est$Female) <- names

#########################################################################################
## Setup lower & higher CIs for male and female

## low
RR_low <- list()
RR_low$Male <- c(NaN, NaN, RR_basic_noalc[1,2], RR_basic_noalc2[1,c(2,5,8)], 
                 NaN, RR_male_noalc[2], RR_male_cat_noalc[,2])
names(RR_low$Male) <- names

RR_low$Female <- c(NaN, NaN, RR_basic_noalc[2,2], RR_basic_noalc2[2,c(2,5,8)], 
                   NaN, RR_female_noalc[2], 
                   RR_female_cat_noalc[2,])
names(RR_low$Female) <-  names

## high
RR_high <- list()
RR_high$Male <- c(NaN, NaN, RR_basic_noalc[1,3], RR_basic_noalc2[1,c(3,6,9)], 
                  NaN, RR_male_noalc[3], RR_male_cat_noalc[,3])
names(RR_high$Male) <-  names
RR_high$Female <- c(NaN, NaN, RR_basic_noalc[2,3], RR_basic_noalc2[2,c(3,6,9)], 
                    NaN, RR_female_noalc[3], RR_female_cat_noalc[3,])
names(RR_high$Female) <-names

tabletext<-cbind(names,
  c("Male",   
    ifelse(is.na(RR_est$Male[-1]), " ", 
           paste0(format(RR_est$Male[-1], digits = 2, nsmall = 2)))),
  c("Female",   
    ifelse(is.na(RR_est$Male[-1]), " ", 
           paste0(format(RR_est$Male[-1], digits = 2, nsmall = 2)))))


forestplot(tabletext, 
           graph.pos = 2,
           legend = c("Male", "Female"),
           fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
           boxsize = .25, # We set the box size to better visualize the type
           line.margin = .1, # We need to add this to avoid crowding
           mean = cbind(RR_est$Male, RR_est$Female),
           lower = cbind(RR_low$Male, RR_low$Female),
           upper = cbind(RR_high$Male, RR_high$Female),
           clip =c(0.5, 2.5),
           col=fpColors(box=c("cadetblue4", "darkblue"), 
                        lines =c("gray24", "darkblue")), 
           is.summary=c(TRUE,TRUE,rep(FALSE,4),TRUE,rep(FALSE,4)),
           grid = TRUE,
           xlog = T,
           xticks = c(0.5, 1, 1.5, 2, 2.5),
           xlab="Risk Ratio",
           lwd.ci = 2,
           graphwidth = unit(200, 'mm'), 
           txt_gp = fpTxtGp(label = list(gpar(fontfamily = "Arial", cex = 3),
                                         gpar(fontfamily = "Arial", col = 'cadetblue4', cex = 3),
                                         gpar(fontfamily = "Arial", col = "darkblue", cex = 3)),
                            ticks = gpar(fontfamily = "", cex=2.25),
                            xlab  = gpar(fontfamily = "Arial", cex = 3)))


#########################################################################################
################################# Unadjusted Prevalence #################################
#########################################################################################

#########################################################################################
## set names for unadjusted prevalence forest plots
names <- c('Unadjusted Prevalence', "of Metabolic Syndrome",
           'Food Secure', 'Food Insecure', 'Full Food Security', 
           'Marginal Food Security',  
           'Low Food Security',  'Very Low Food Security')

#########################################################################################
## Create List for estimates w/ male and female
## Need to blanks at the top for labels

RR_est <- list()
sub("\\(.*", '', prevalence_noalc_male)
RR_est$Male <- c(NaN, NaN, 
                 sub("\\(.*", '', prevalence_noalc_male), 
                 sub("\\(.*", '', prevalence_alt_noalc_male))
names(RR_est$Male) <-  names

RR_est$Female<- c(NaN, NaN, 
                  sub("\\(.*", '', prevalence_noalc_female), 
                  sub("\\(.*", '', prevalence_alt_noalc_female))
names(RR_est$Female) <- names

#########################################################################################
## Setup lower & higher CIs for male and female

## low
RR_low <- list()

RR_low$Male <- c(NaN, NaN, 
                 gsub(".*\\(\\s*|-.*", "", prevalence_noalc_male),
                 gsub(".*\\(\\s*|-.*", "", prevalence_alt_noalc_male))
names(RR_low$Male) <- names

RR_low$Female <- c(NaN, NaN, 
                   gsub(".*\\(\\s*|-.*", "", prevalence_noalc_female),
                   gsub(".*\\(\\s*|-.*", "", prevalence_alt_noalc_female))
names(RR_low$Female) <-  names

## high
RR_high <- list()
RR_high$Male <- c(NaN, NaN, 
                  gsub(".*-\\s*|\\).*", "", prevalence_noalc_male),
                  gsub(".*-\\s*|\\).*", "", prevalence_alt_noalc_male))
names(RR_high$Male) <-  names
RR_high$Female <- c(NaN, NaN, 
                    gsub(".*-\\s*|\\).*", "", prevalence_noalc_female),
                    gsub(".*-\\s*|\\).*", "", prevalence_alt_noalc_female))
names(RR_high$Female) <-names

tabletext<-cbind(names,
                 c("Male", " ",
                   paste0(format(RR_est$Male[3:8], digits = 2, nsmall = 2))),
                 c("Female", " ", 
                   paste0(format(RR_est$Male[3:8], digits = 2, nsmall = 2))))

forestplot(tabletext, 
           graph.pos = 2,
           legend = c("Male", "Female"),
           fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
           boxsize = .25, # We set the box size to better visualize the type
           line.margin = .1, # We need to add this to avoid crowding
           mean = cbind(RR_est$Male, RR_est$Female),
           lower = cbind(RR_low$Male, RR_low$Female),
           upper = cbind(RR_high$Male, RR_high$Female), 
           clip =c(0.2, 0.5),
           col=fpColors(box=c("cadetblue4", "darkblue"), 
                        lines =c("gray24", "darkblue")), 
           is.summary=c(TRUE,TRUE,rep(FALSE,6)),
           grid = F,
           zero = 0.35,
           xticks = c(0.2, 0.3, 0.4, 0.5),
           xlab="Prevalence",
           lwd.ci = 2,
           graphwidth = unit(200, 'mm'), 
           txt_gp = fpTxtGp(label = list(gpar(fontfamily = "Arial", cex = 3),
                                         gpar(fontfamily = "Arial", col = 'cadetblue4', cex = 3),
                                         gpar(fontfamily = "Arial", col = "darkblue", cex = 3)),
                            ticks = gpar(fontfamily = "", cex=2.25),
                            xlab  = gpar(fontfamily = "Arial", cex = 3)))

