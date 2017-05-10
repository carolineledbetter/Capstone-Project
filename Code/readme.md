This folder contains all the code.  

Details about the files in this folder:

File | Description
---|---------------------------------------------------------------------
DataImport.R | imports all data from the CDC website  
DataClean.R | cleans and merges the orig data to create the analysis data set.  
Descriptives.R | contains a preliminary look at missing data and the unweighted descriptives.  
Analytical Descriptives.R | contains final descriptives of the analysis data set used in the final report including table 1 (unweighted) and table 2 (weighted).
Table1Unweighted.R | contains a function that creates unweighted descriptives for table 1 including missing values and excluding p values with an option to include missing categoricals.  
Table1Weighted | contains a function that uses the survey design object to create the weighted descriptives for Table 1 including p values.  
Analysis.R | contains all of the code used in the analysis.
Demographic Graphs.R | Represents table 2 in graphic form. 
Forest Plots.R | contains the code to generate forest plots of the results for use in the presentation.
