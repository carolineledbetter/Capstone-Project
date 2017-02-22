### This file cleans and merges the original data sets and creates the final data set ###
### used for analysis. ###

# import workspace from DataImport containing all orig data. 
load(file = '~/Repositories/Data/Capstone/original_all.Rdata')

# create variable for year cycles
allcyc <- c(1999,2001,2003,2005,2007,2009,2011,2013)

# for Food Security Questionnaire only need food security variable (ADFDSEC for 1999-2002
# and FSDAD for 2003-2014) and seqn (identifier)

FSQ1999 <- FSQ1999[,c('seqn', 'adfdsec')]
FSQ2001 <- FSQ2001[,c('seqn', 'adfdsec')]

for(i in allcyc[3:8]){
  ds <- get(paste('FSQ', i, sep = ''))
  assign(paste('FSQ', i, sep = ''), ds[,c('seqn', 'fsdad')])
}; remove(i,ds)

# for fasting glucose need 4 year weight for 1999-2002, 2 year weight for all others, 
# lbxglu(fasting glucose), and seqn, rename weighting variable so it's the same in all 
# datasets

FGI1999 <- FGI1999[,c('seqn', 'wtsaf4yr', 'lbxglu')]
names(FGI1999)[2] <- 'svywgt'
FGI2001 <- FGI2001[,c('seqn', 'wtsaf4yr', 'lbxglu')]
names(FGI2001)[2] <- 'svywgt'

for(i in allcyc[3:8]){
  ds <- get(paste('FGI', i, sep = ''))
  ds <- ds[,c('seqn', 'wtsaf2yr', 'lbxglu')]
  names(ds)[2] <- 'svywgt'
  assign(paste('FGI', i, sep = ''), ds)
}; remove(i,ds)

# for body measures data need BMIWAIST (waist circumference), BMXBMI (BMI) and seqn
for(i in allcyc){
  ds <- get(paste('BMD', i, sep = ''))
  assign(paste('BMD', i, sep = ''), ds[,c('seqn', 'bmiwaist', 'bmxbmi')])
}; remove(i,ds)

# for blood pressure

# for triglycerides need LBXTR (trigylcerides) and seqn

for(i in allcyc){
  ds <- get(paste('TRG', i, sep = ''))
  assign(paste('TRG', i, sep = ''), ds[,c('seqn', 'lbxtr')])
}; remove(i,ds)

# for HDL need HDL (lbdhdl for 1999-2002 and lbxhdd for 2003-2004 and lbdhdd for 2005-2014) and seqn,
# rename HDL so it is the same in all datasets

HDL1999 <- HDL1999[,c('seqn', 'lbdhdl')]
names(HDL1999)[2] <- 'hdl'
HDL2001 <- HDL2001[,c('seqn', 'lbdhdl')]
names(HDL2001)[2] <- 'hdl'
HDL2003 <- HDL2003[,c('seqn', 'lbxhdd')]
names(HDL2003)[2] <- 'hdl'

for(i in allcyc[4:8]){
  ds <- get(paste('HDL', i, sep = ''))
  ds <- ds[,c('seqn', 'lbdhdd')]
  names(ds)[2] <- 'hdl'
  assign(paste('HDL', i, sep = ''), ds)
}; remove(i,ds)


# alcohol use (for detailed criteria see data analyis plan)
invisible(lapply(ls(pattern='ALC.*'),
                     function(x) {
                       dat=get(x)
                       dat$alcuse = factor(NaN, levels = c('Never','Moderate', 
                                                           'Heavy'))
                       dat$alcuse[dat$alq110 == 2] <- 'Never' #alq110 = no
                       dat$alcuse[dat$alq130 <=2 ] <- 'Moderate'
                       dat$alcuse[dat$alq130 > 2] <- 'Heavy'
                       dat$alcuse[dat$alq140q > 0] <- 'Heavy'
                       assign(paste(x), dat[c('seqn', 'alcuse')], envir = .GlobalEnv)
                     }))



