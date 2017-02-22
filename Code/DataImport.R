############################################################################################
#####             Import Data from https://wwwn.cdc.gov/Nchs/Nhanes/                   #####
############################################################################################

# This file imports all of the data sets from the CDC used for analysis. The final workspace
# is saved as ~/Repositories/Data/Capstone/original_all.Rdata


library(Hmisc) #for importing SAS xport files

# character variables are converted to R factors

#### Download Food Security Questionairre Data for Each Year ####

FSQ1999 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/FSQ.XPT")
FSQ2001 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/FSQ_B.XPT")
FSQ2003 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/FSQ_C.XPT")
FSQ2005 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/FSQ_D.XPT")
FSQ2007 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/FSQ_E.XPT")
FSQ2009 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/FSQ_F.XPT")
FSQ2011 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/FSQ_G.XPT")
FSQ2013 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/FSQ_H.XPT")



#### Download Fasting Glucose Data for each year ####

FGI1999 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/LAB10AM.XPT")
FGI2001 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/L10AM_B.XPT")
FGI2003 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/L10AM_C.XPT")
FGI2005 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/GLU_D.XPT")
FGI2007 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/GLU_E.XPT")
FGI2009 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/GLU_F.XPT")
FGI2011 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/GLU_G.XPT")
FGI2013 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/GLU_H.XPT")



#### Download Body Measures Data for each year ####

BMD1999 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/BMX.XPT")
BMD2001 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/BMX_B.XPT")
BMD2003 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/BMX_C.XPT")
BMD2005 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/BMX_D.XPT")
BMD2007 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/BMX_E.XPT")
BMD2009 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/BMX_F.XPT")
BMD2011 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/BMX_G.XPT")
BMD2013 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/BMX_H.XPT")



#### Download Blood Pressure Data for each year ####

BPD1999 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/BPX.XPT")
BPD2001 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/BPX_B.XPT")
BPD2003 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/BPX_C.XPT")
BPD2005 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/BPX_D.XPT")
BPD2007 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/BPX_E.XPT")
BPD2009 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/BPX_F.XPT")
BPD2011 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/BPX_G.XPT")
BPD2013 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/BPX_H.XPT")



#### Download Triglyceride Data for each year ####

TRG1999 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/LAB13AM.XPT")
TRG2001 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/L13AM_B.XPT")
TRG2003 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/L13AM_C.XPT")
TRG2005 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/TRIGLY_D.XPT")
TRG2007 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/TRIGLY_E.XPT")
TRG2009 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/TRIGLY_F.XPT")
TRG2011 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/TRIGLY_G.XPT")
TRG2013 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/TRIGLY_H.XPT")



#### Download HDL Cholesterol Data for each year ####

HDL1999 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/LAB13.XPT")
HDL2001 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/L13_B.XPT")
HDL2003 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/L13_C.XPT")
HDL2005 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/HDL_D.XPT")
HDL2007 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/HDL_E.XPT")
HDL2009 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/HDL_F.XPT")
HDL2011 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/HDL_G.XPT")
HDL2013 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/HDL_H.XPT")



#### Download Demographics and Sample Weights Data for each year ####

DEM1999 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DEMO.XPT")
DEM2001 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/DEMO_B.XPT")
DEM2003 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DEMO_C.XPT")
DEM2005 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.XPT")
DEM2007 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/DEMO_E.XPT")
DEM2009 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/DEMO_F.XPT")
DEM2011 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.XPT")
DEM2013 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DEMO_H.XPT")



#### Download Alchohol Use Data for each year ####

ALC1999 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/ALQ.XPT")
ALC2001 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/ALQ_B.XPT")
ALC2003 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/ALQ_C.XPT")
ALC2005 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/ALQ_D.XPT")
ALC2007 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/ALQ_E.XPT")
ALC2009 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/ALQ_F.XPT")
ALC2011 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/ALQ_G.XPT")
ALC2013 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/ALQ_H.XPT")



#### Download Smoking Questionnaire Data for each year ####

SMQ1999 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/SMQ.XPT")
SMQ2001 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/SMQ_B.XPT")
SMQ2003 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/SMQ_C.XPT")
SMQ2005 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/SMQ_D.XPT")
SMQ2007 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/SMQ_E.XPT")
SMQ2009 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/SMQ_F.XPT")
SMQ2011 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/SMQ_G.XPT")
SMQ2013 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/SMQ_H.XPT")

#### Download Physical Acitivity Data for each year ####

PAQ1999 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/PAQ.XPT")
PAQ2001 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/PAQ_B.XPT")
PAQ2003 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/PAQ_C.XPT")
PAQ2005 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/PAQ_D.XPT")
PAQ2007 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/PAQ_E.XPT")
PAQ2009 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/PAQ_F.XPT")
PAQ2011 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/PAQ_G.XPT")
PAQ2013 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/PAQ_H.XPT")



#### Download Blood Pressure Questionairre Data for each year ####
# (for High Blood Pressure/High Cholesterol Medication) #

BPQ1999 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/BPQ.XPT")
BPQ2001 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/BPQ_B.XPT")
BPQ2003 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/BPQ_C.XPT")
BPQ2005 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/BPQ_D.XPT")
BPQ2007 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/BPQ_E.XPT")
BPQ2009 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/BPQ_F.XPT")
BPQ2011 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/BPQ_G.XPT")
BPQ2013 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/BPQ_H.XPT")



#### Download Diabetes Questionairre Data for each year ####
# (for High Glucose Medication) #

DIQ1999 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DIQ.XPT")
DIQ2001 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/DIQ_B.XPT")
DIQ2003 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DIQ_C.XPT")
DIQ2005 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DIQ_D.XPT")
DIQ2007 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/DIQ_E.XPT")
DIQ2009 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/DIQ_F.XPT")
DIQ2011 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DIQ_G.XPT")
DIQ2013 <- sasxport.get("http://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DIQ_H.XPT")





#### Save Workspace ####
save.image(file = '~/Repositories/Data/Capstone/original_all.Rdata')
