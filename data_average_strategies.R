#This script is not for use on its own. It is called by getting_data.R
#---Data for Table3 of new research-----
#The df created here is used to analyze the momentum of predictability
#for strategies based on the average of various technical indicators in one group
#In particular, the average of 354 moving avg strategies, the avg of 36
#momentum based rules, and the avg of 354 volume based strategies is computed

input_file <- "/Inputs/PredictorData2017.xlsx"
input_file2 <- "/Inputs/spx.csv"
raw_data <- read.xlsx(paste0(path, input_file))

from_t <- as.Date("1950-01-01")
to_t <- as.Date("2017-12-01")
spx <- read.csv(paste0(path, input_file2))[, c(1, 6, 7)]

date <- seq.Date(from <- from_t, to_t, by = "month")
data_new <- data.frame(Date = date)

s <- which(raw_data[,1] == "195001")
e <- which(raw_data[,1] == "201712")

#Value-weighted equity returns incl. dividends, in accordance with Neely
EQ <- raw_data[s :  e, c("yyyymm", "CRSP_SPvw")]
EQ[, 2] <- as.numeric(EQ[,2])

#Lag of risk-free rate
RF <- raw_data[(s - 1) :  (e - 1), c("yyyymm", "Rfree")]

data_new["EQP"] <- log(1 + EQ[, 2]) - log(1 + RF[, 2])

#-------14 Technical Indicators-------

# 354 Moving Average rules

months_short <- 1:12
months_long <- 2:36
index <- ts(raw_data[s : e,c("yyyymm", "Index")])
moving_avgs <- data.frame("date" = index[, 1])

#Determining moving averages of the price
for (i in c(months_short, months_long)){
  moving_avgs[, paste0("ma_",i)] = SMA(index[, 2], i)
}

#Determining which MA is greater, the short or the long
for (i in months_short){
  for (j in (i + 1) : max(months_long)){
    # 1, if the Short MA >= Long MA, 0 otherwise
    data_new[paste0("ma_",i,"_",j)] <- as.numeric(moving_avgs[, paste0("ma_", i)] >= moving_avgs[,  paste0("ma_", j)])
  }
}

# 36 MOMENTUM rules
index_ts <- ts(index[, 2])
mom_months <- 1:36
dnew <- dim(data_new)[1]
for (i in mom_months){
  data_new[paste0("MOM_",i)] <- c(rep(NA, i), as.numeric(index_ts >= lag(index_ts, -i)))[1 : dnew]
}

# 354 On Balance Volume rules

index_ts <- ts(spx[, "Adj.Close"]) 
vol_ts <- ts(spx[, "Volume"])
vol_d <- c(0, (2 * (index_ts >= lag(index_ts, -1)) - 1) * vol_ts)
obv <- cumsum(vol_d) #on-basis volume for every period

for (i in months_short){
  for (j in (i + 1) : max(months_long)){
    # 1, if the short obv moving average is not less than the long obv moving average
    data_new[paste0("obv_",i,"_",j)] <- as.numeric(SMA(obv, i) >= SMA(obv, j))[1 : dnew]
  }
}

data_new <- data_new[(max(mom_months) + 1) : dnew,]

#Tech Indicators are highly correlated, so PCA is used and the first principal
#component is extracted to get the majority of information
pca <- prcomp(data_new[3 : dim(data_new)[2]])

#Determining the number of MA and VOL based rules
n_ma_rules <- sum(expand.grid(months_short, months_long)[,1] <
                    expand.grid(months_short, months_long)[,2])

df_avg<- data.frame("Date" = date[(max(months_long) + 1) : dnew])
df_avg["EQP"] <- data_new["EQP"]

#------Getting the average of MA, MOM and VOL strategies------

df_avg["MA_AVG"] <- rowMeans(data_new[, 3 : (2 + n_ma_rules)])
df_avg["MOM_AVG"] <- rowMeans(data_new[, (3 + n_ma_rules) : (2 + n_ma_rules + length(mom_months))])
df_avg["VOL_AVG"] <- rowMeans(data_new[, (3 + n_ma_rules + length(mom_months)) : dim(data_new)[2]])
df_avg["PC_TECH_ALL"] <- pca$x[,1]

data_15 <- df_avg
rm(df_avg)