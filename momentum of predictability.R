#Definition of the MoP and how to analyze it can be found in Wang's article,
#Or in the pdf file in my GitHub repository , where the methodology of the 
#Directional Accuracy Test by Pesaran and Timmermann is detailed as well.

#Momentum of predictability (MoP) is the property that if a variable displayed 
#Superior predictive ability relative to the historical average benchmark
#Forecast in the recent past, then it is likely to do so in the present.
#This part of the code uses squared predictive errors of univariate regression
#Models to check if there is MoP for certain fundamental variables, as shown
#By Wang(2018), or analyse whether there is significant MoP for technical
#Variables as well. The exact methodology is given in Yudong Wang's article
#Momentum of return predictability
rm(list = ls())

#Replication will use Wang's data to analyze momentum of predictability
#for FUNDAMENTAL VARIABLES. The results in Table 2 of the article
#are exactly replicated by this script, if replication == TRUE
replication <- FALSE

#If replication is set to FALSE, we will use data with technical
#Variables. If average_strategies is FALSE, then the 14 technical
#Indicators in Neely(2014) are used. If TRUE, then 3 averaged out
#rules (1 for the avg of 354 moving average rules, 1 for the avg
#of 36 momentum rules, 1 for the avg of 354 volume based rules) are
#used, plus a principal components extracted from the PCA for these
#744 variables

average_strategies <- FALSE
output_to_csv <- FALSE

source("mop_forecast_errors.R")

#Comparing past and current forecasting performance against Historical Avg

d1 <- dim(df_sqerr)[1] #Number of predictions
d2 <- dim(df_sqerr)[2]
lookbacks <- c(1, 3, 6, 9, 12)

#Splitting the dataframe, each row of current_errors contains the prediction
#Error for the given period, and each row of pst_errors contains the moving
#Average of the prediction error for the past s, 3, 6, 9 or 12 periods
current_errors <- df_sqerr[2 : d1, 1 : v] #v is inherited, number of predictive variables
past_errors <- df_sqerr[1 : (d1 - 1), 2 : d2]

#-----Pesaran-Timmermann test for serial independence-----

#Initializing table for the outputs
table_mops <- data.frame(matrix(0, length(lookbacks), v - 2)) 
row.names(table_mops) <- lookbacks
colnames(table_mops) <- colnames(current_errors)[3 : v]

#Setting column names so that a for cycle can recognize each column
if (1 %in% lookbacks){
  colnames(past_errors)[1 : (v - 1)] <- paste0(colnames(past_errors)[1 : (v - 1)], "_1")
}

#For each technical indicator (MA_1_9, MA_1_12 ..., VOL_3_12), and for each
#lookback period k, we now determine the strength of the relationship
#forecasting performance in the past k month and in the current period
#table_mops will contain the p-values

for (i in 1:length(lookbacks)){
  k <- lookbacks[i]
  for (vc in colnames(table_mops)){
    #performing a directional accuracy test, the methodology is written in
    #the pdf file on github.com/szmnkrisz/ForecastOnTechIndicators
    #fcst and actl are the two vectors to be compared, fcst contains a 1, if
    #in the recent past, historical average outperformed the model, and a -1
    #If it did not; actl is the same for the current period
    fcst <- 2 * (past_errors[paste0(vc, "_", k)] > past_errors[paste0("HA_", k)])[k : (d1 - 1)] - 1
    
    actl <- 2 * (current_errors[vc] > current_errors["HA"])[k : (d1 - 1)] - 1
    
    table_mops[i, vc] <- DACTest(fcst, actl, test = "PT", conf.level = 0.95)$p.value
  }
}

if (output_to_csv){
  folder <- "Tables"
  ifelse(!dir.exists(file.path(folder)), dir.create(file.path(folder)), FALSE)
  setwd(paste0(path, "/", folder))
  write.csv(table_mops, "table2.csv")
  setwd(path)
}