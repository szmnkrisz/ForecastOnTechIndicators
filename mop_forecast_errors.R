#This script should not run on its own. It is called by momentum_of_predictability.R
#This part calculates the out of sample forecasting performance of
#the variables in the df given by getting_data.R. First, an initial
#model building period (m) is given (recommended to set to 240, if one 
#wants to replicate the results of Wang's article, 180 otherwise), then 
#the prediction for the next period is determined, then the model is built
#again using the newest observation. The forecast errors are saved
#Into a dataframe for later analysis.

#2 parameters, replication anaverage_strategies are determined in momentum_of_predictability.R

#Replication will use Wang's data to analyze momentum of predictability
#for FUNDAMENTAL VARIABLES.

#If replication is set to FALSE, we will use data with technical
#Variables. If average_strategies is FALSE, then the 14 technical
#Indicators in Neely(2014) are used. If TRUE, then 3 averaged out
#rules (1 for the avg of 354 moving average rules, 1 for the avg
#of 36 momentum rules, 1 for the avg of 354 volume based rules) are
#used, plus a principal components extracted from the PCA for these
#744 variables

source("getting_data.R")

#The equity risk premium
eqp <- data_15[, "EQP"]

d15 <- dim(data_15)[1] #number of observations
v <- dim(data_15)[2] #number of columns
var_cols <- colnames(data_15)[3 : v] #the predictive variables, the first to are date and eqp
m <- 180 #length of model-building period
lookbacks <- c(3, 6, 9, 12) #MoP lookbacks other than 1

#The historical average forecast and its error
#The average up to  t-1 is predicted for the period t
ha_forecast <- cumsum(eqp) / seq_along(eqp)
ha_sqerror <- (eqp[(m + 1) : d15] -  ha_forecast[m : (d15 - 1)]) ^ 2 

#This function takes a column name of data_15 as input, then build an
#Initial regression model for the equity risk premium regressed on the
#Selected variable on the build-up period (set by m, 180 or 240 months)
#It predicts a risk premium for the next period and stores it, then gets
#The actual values of that period and build another, newer model using
#That and the original data to predict for the next period, etc.
#It returns the predictions for all the periods after the init build-up.

Fc <- function(var_col){
  x <- data_15[, var_col]
  r_preds <- c()
  for (i in m : (d15 - 1)){
    model <- lm(eqp[2 : i] ~ x[1 : (i - 1)])
    r_pred <- sum(as.numeric(model$coefficients) * c(1, x[i]))
    r_preds <- c(r_preds, r_pred)
  }
  r_preds
}

#Using Fc, FcError creates a vector of the same length containing the
#Prediction errors. Later on, it will be compared to the errors of 
#The historical average forecast

FcError <- function(var_col){
  r_preds <- Fc(var_col)
  x <- data_15[, var_col]
  all_se <- (eqp[(m + 1) : d15] - r_preds) ^ 2
  all_se
}

#Putting the errors into df_sqerr
df_sqerr <- data.frame("Date" = data_15[(m + 1) : d15,"Date"])
df_sqerr["HA"] <- ha_sqerror #The historical avg is needed as well
for (vc in var_cols){
  df_sqerr[vc] <- (FcError(vc))
}

#Computing the moving average of the errors, to be able to
#Later compare predictive ability in the past and present
for (k in lookbacks){
  for (vc in c("HA", var_cols)){
    df_sqerr[paste0(vc, "_", k)] <- SMA(df_sqerr[, vc], k)
  }
}
