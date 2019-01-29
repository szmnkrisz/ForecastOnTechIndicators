#This part of the code gets the necessary packages and
#establishes the dataframe according to whether one wants to
#use the original data with fundamental variables, or the data
#used to investigate the question in hand for technical indicators

path <- "~/EmpiricalFinanceResearch/TechnicalIndicatorsPredictiveAbility"

library(openxlsx)
library(TTR)
library(rugarch)

if (replication){
  source("data_for_replication.R")
} else if(average_strategies){
  source("data_average_strategies.R")
} else{
  source("data_new_research.R")
}