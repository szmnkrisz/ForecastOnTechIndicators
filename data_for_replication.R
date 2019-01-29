#This script is not for use on its own. It is called by getting_data.R
#This part recreates the data used by Wang2018 to show that the predictive
#Ability of certain fundamental variables is persistent. The article employed
#12 technical variables, out of which 11 is used in this analysis.

input_file <- "/Inputs/PredictorData2017.xlsx" #From Amit Goyal's homepage

raw_data <- read.xlsx(paste0(path, input_file))

#Initializing the dataframe, data_15 (data until 2015 Dec)

from_t <- as.Date("1927-01-01")
to_t <- as.Date("2015-12-01")

date <- seq.Date(from <- from_t, to_t, by = "month")
#The analysis will be done with the df named data_15 
data_15 <- data.frame(Date = date) 

#-------Determining The Risk Premium----------

s <- which(raw_data[,1] == "192701") #row of starting period
e <- which(raw_data[,1] == "201512") #row of end period

#Value-weighted equity returns incl. dividends, in accordance with Neely

EQ <- raw_data[s :  e, c("yyyymm", "CRSP_SPvw")]
EQ[, 2] <- as.numeric(EQ[,2])

#Lag of risk-free rate
RF <- raw_data[(s - 1) :  (e - 1), c("yyyymm", "Rfree")]

data_15["EQP"] <- log(1 + EQ[, 2]) - log(1 + RF[, 2])

#--------12 Fundamental Variables

data_15["DP"] <- log(raw_data[s : e,"D12"] / raw_data[s : e,"Index"]) #Dividend-Price Ratio
data_15["DY"] <- log(raw_data[s : e,"D12"] / raw_data[(s - 1) : (e - 1), "Index"]) #Dividend Yield
data_15["EP"] <- log(raw_data[s : e,"E12"] / raw_data[s : e,"Index"]) #Earning-Price Ratio
data_15["BM"] <- as.numeric(raw_data[s : e, "b/m"]) #Book-To-Market
data_15["NTIS"] <- - as.numeric(raw_data[s : e, "ntis"]) #Net Equity Expansion
data_15["TBL"] <- - 100 * as.numeric(raw_data[s : e, "tbl"]) #Treasury Bill Rate
data_15["LTY"] <- - 100 * as.numeric(raw_data[s : e, "lty"]) #Long-Term Yield
data_15["LTR"] <- - 100 * as.numeric(raw_data[s : e,"ltr"]) #Long-Term Return (Government Bonds)
data_15["DFY"] <- - 100 * (as.numeric(raw_data[s : e,"BAA"]) - as.numeric(raw_data[s : e,"AAA"])) #Default Yield Spread
data_15["DFR"] <- - 100 * (as.numeric(raw_data[s : e,"corpr"]) - as.numeric(raw_data[s : e,"ltr"])) #Default Return Spread
data_15["INFL"] <- - 100 * as.numeric(raw_data[(e - 1) : (s - 1),"infl"]) #Inflation (t-1 used to account for decay in CPI releases)

#only for data-checking purposes, whether they are the same as Wang's Table 1
for (i in 2:13) print(paste(colnames(data_15)[i],"mean =", 
          round(mean(data_15[,i]), 3), "stdev =", round(sd(data_15[,i]), 3)))
#Data avgs and std devs are equal to Wang2018's, only difference is the scaling