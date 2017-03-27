This folder contains all the code.  

Details about the files in this folder:

File | Description
---|---------------------------------------------------------------------
DataImport.R | imports all data from the CDC website  
DataClean.R | cleans and merges the orig data to create the analyis data set.  
Descriptives.R | contains a preliminary look at missing data and the unweighted descriptives.  
Analytical Descriptives.R | contains the descriptives of the analsis data set inculding the weighted table 1.  
Table1Unweighted.R | contains a function that creates unweighted descriptives for table 1 including missing values and excluding p values.  
Table1Weighted | contains a function that uses the survey design object to create the weighted descriptives for Table 1 inculding p values.  
Analysis.R | contains all of the code used in the analysis.
