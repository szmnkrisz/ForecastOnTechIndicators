#This script is not for use on its own. It is called by getting_data.R
#This part creates the dataframe with the equity risk premium and 14
#Technical indicators. The df is later used to analyzed momentum of
#Predictability for technical indicators

input_file <- "/Inputs/PredictorData2017.xlsx"
input_file2 <- "/Inputs/spx.csv"

raw_data <- read.xlsx(paste0(path, input_file))
spx <- read.csv(paste0(path, input_file2))[, c(1, 6, 7)]

from_t <- as.Date("1950-01-01")
to_t <- as.Date("2017-12-01")

date <- seq.Date(from <- from_t, to_t, by = "month")
data_new <- data.frame(Date = date)

#-------Determining The Risk Premium----------

s <- which(raw_data[,1] == "195001")
e <- which(raw_data[,1] == "201712")

#Value-weighted equity returns incl. dividends, in accordance with Neely

EQ <- raw_data[s :  e, c("yyyymm", "CRSP_SPvw")]
EQ[, 2] <- as.numeric(EQ[,2])

#Lag of risk-free rate
RF <- raw_data[(s - 1) :  (e - 1), c("yyyymm", "Rfree")]

data_new["EQP"] <- log(1 + EQ[, 2]) - log(1 + RF[, 2])

#-------14 Technical Indicators-------

#6 Moving Average rules

months_short <- c(1, 2, 3)
months_long <- c(9, 12)
index <- ts(raw_data[s : e,c("yyyymm", "Index")]) #date and the price level
moving_avgs <- data.frame("date" = index[, 1])

#Determining moving averages of the price
for (i in c(months_short, months_long)){
  moving_avgs[, paste0("ma_",i)] = SMA(index[, 2], i)
}

#Determining which MA is greater, the short or the long
for (i in months_short){
  for (j in months_long){
    # 1, if the Short MA >= Long MA, 0 otherwise
    data_new[paste0("ma_",i,"_",j)] <- as.numeric(
      moving_avgs[, paste0("ma_", i)] >= moving_avgs[,  paste0("ma_", j)])
  }
}

#2 MOMENTUM rules

index_ts <- ts(index[, 2])
mom_months <- c(9, 12)
dnew <- dim(data_new)[1]

#Determining MOM signals, 1 if the current price is greater than the
#price i months ago, 0 otherwise
for (i in mom_months){
  data_new[paste0("MOM_",i)] <- c(rep(NA, i), as.numeric(index_ts >= lag(index_ts, -i)))[1 : dnew]
}
data_new[12, "MOM_12"] <- 1 #needed for PCA, and also true

#6 On Balance Volume rules (methdology as in Neely 2014)

index_ts <- ts(spx[, "Adj.Close"]) 
vol_ts <- ts(spx[, "Volume"])

#vol_d is the monthly volume multiplied by 1 if the price just went up,
#by -1 if it just went down
vol_d <- c(0, (2 * (index_ts >= lag(index_ts, -1)) - 1) * vol_ts)
obv <- cumsum(vol_d) #on-basis volume for every period

#The signal is 1, if the short moving average of the OBV is greater
#than the long moving average, 0 otherwise

for (i in months_short){
  for (j in months_long){
    # 1, if the short obv moving average is not less than the long obv moving average
    data_new[paste0("obv_",i,"_",j)] <- as.numeric(SMA(obv, i) >= SMA(obv, j))[1 : dnew]
  }
}

data_new <- data_new[max(mom_months) : dnew,]

#Tech Indicators are highly correlated, so PCA is used and the first principal
#component is extracted to get the majority of information

pca <- prcomp(data_new[3 : dim(data_new)[2]])
data_new["TECH_PC"] <- pca$x[,1] #The first PC explains over 70% of the variance

data_15 <- data_new
rm(data_new)