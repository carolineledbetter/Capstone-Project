#########################################################################################
######## This script makes graphs of the demographic characterstics of the sample #######
#########################################################################################

# This script uses the percentages in table2alt from Analytical Descriptives.R 
# stored in finaldescepitives.rdata to graphically represent the demographics of the 
# sample for use in the presentation.

# The final png is stored as Reports/demos.png
# The final plot objs are stored as demo_plots.rdata


load(file = '~/Repositories/Data/Capstone/finaldescriptives.rdata')
#need final analysis data set for factor levels
load(file = '~/Repositories/Data/Capstone/analysis_report.rdata')


# retrieve percents from table2alt
demos <- gsub(".*\\((.*)\\).*", "\\1", table2alt)
# drop p-value column
demos <- demos[,-5]

#########################################################################################
# make data frame for each demographic category from demos
# X1 = food sec category need to repeat for each level of demo
# X2 = levels of demos, need to repeat for each food sec cat block
# X3 = percents, for binary need to make 100- 2nd category for first
# (only 2nd category is displayed in table)

getBinPcts <- function(variable, var2 = NULL) { 
  # some variables have different levels in the table than in the analysis dataset
  var2 <- ifelse(is.null(var2), variable, var2)
  # get position of rows
  n <- which(row.names(demos) == var2) + 1
  df <- data.frame(X1 = rep(levels(analysis$FoodSecurity), 
                            length(levels(analysis[,c(variable)]))), 
                   X2 = as.vector(t(replicate(length(levels(analysis$FoodSecurity)),
                                              levels(analysis[,c(variable)])))), 
                   X3 = c(c(100 - as.numeric(demos[n,])), 
                          as.numeric(demos[n,])))
  #rename levels so they fit on screen
  levels(df$X1) <- c('Full', 'Marginal', 'Low', "Very Low")
  #rename X2 so it matches demographic cat, makes legend pretty
  names(df)[2] <- var2
  return(df)
}

getNonBinPcts <- function(variable, var2 = NULL) { 
  var2 <- ifelse(is.null(var2), variable, var2)
  n <- which(row.names(demos) == var2) + 1
  n_last <- n + length(levels(analysis[,c(variable)])) - 1
  
  df <- data.frame(X1 = as.vector(t(replicate(length(levels(analysis[,c(variable)])), 
                                              levels(analysis$FoodSecurity)))), 
                   X2 = rep(levels(analysis[,c(variable)]), 
                            length(levels(analysis$FoodSecurity))), 
                   X3 = as.numeric(demos[n:n_last,]))
  levels(df$X1) <- c('Full', 'Marginal', 'Low', "Very Low")
  df$X2 <- factor(df$X2, levels =rev(levels(analysis[,c(variable)])))
  names(df)[2] <- var2
  return(df)
}

Gender <- getBinPcts('Gender')
PhysAct <- getBinPcts('ModerateActivity', 'Moderate Phys Act')
Race <- getNonBinPcts('Race')
Education <- getNonBinPcts('Education')
Income <- getNonBinPcts('Income')
levels(Income$Income)[levels(Income$Income) == 
                        levels(Income$Income)[1]] <- '\u2265 $75,000'

Smoker <- getNonBinPcts('smoker', 'Smoking Status')

#########################################################################################
# make stacked bar plots for each demographic category, supress x, y axis lab
makeplot <- function(var){
  var2 <- names(var)[2]
  plot <- ggplot(data = var, aes(x = X1, y = X3, fill = get(var2))) + 
          geom_bar(stat = 'identity', position = 'stack') + 
          labs(fill = var2, x = NULL, y = NULL) 
  return(plot)
}

for(x in list("Race", "Education", "Income", "Gender", "PhysAct", "Smoker")){
  assign(paste0("p_", x), makeplot(get(x)))
}; remove(x)

#########################################################################################
# put them all in 3x2 plot

library(grid)
library(gridExtra)
title1=textGrob("Characterstics of Particiants by Food Security Category",
                gp=gpar(fontface="bold"))

png(filename = '~/Repositories/Capstone-Project/Reports/Images/demos.png', width = 1280,
    height = 800, bg = 'white', typ = 'quartz')
plot(grid.arrange(arrangeGrob(p_Race, p_Education, p_Income, 
                              p_Smoker, p_PhysAct, p_Gender, 
                         nrow=3, ncol = 2), 
             bottom = "Food Security Category", 
             left = 'Percent', top = title1))
dev.off()
save(list = ls(pattern = 'p_.*'), file =  '~/Repositories/Data/Capstone/demo_plots.Rdata')
