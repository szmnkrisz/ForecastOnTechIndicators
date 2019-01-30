# TechnicalIndicatorsPredictiveAbility

Technical indicators exhibit some predictive ability when it comes to forecasting stock returns.

The persistence of the ability of 12 fundamental variables to predict the equity risk premium is analysed by the paper Momentum of return predictability (Wang(2018)). It shows that it is true for some of the models that if they outperformed the historical average forecast for the excess return are likely to do so in the current period as well.

This property is called the momentum of predictability (MoP), and I examined the presence or non-presence of it for technical indicators instead of fundamental variables. The pdf file in the repo, and the abovementioned article contains everything related to the methodology.

## The Scripts

### momentum_of_predictability.R
The main script that test the momentum of predictability of technical indicators. The code runs to 3 different outputs based on our choice here. The first is replication, if replication is set to TRUE, we use the same data that Wang(2018) did, with fundamental variables, so the code arrives to exactly the same output as Table 2 of that article. 

If it is set to FALSE, the next choice is average_strategies. If it is FALSE, we test the momentum of predictability for a smaller number (14) of technical indicators. If it is TRUE, we test MoP for 3 average variables derived from a higher number of technical indicators.

The script sources mop_forecast_errors.R that creates the required dataframe with predictions and their performance. The scripts performs a directional accuracy test on the predictions.

### mop_forecast_errors.R

Sources getting_data.R, and creates a dataframe containing out-of-sample forecast errors.

### getting_data.R

Based on the values of replication and average_strategies, it sources one of data_for_replication.R (replication <- TRUE), data_new_research.R (replication <- FALSE and average_strategies <- FALSE) or data_average_strategies.R (replication <- FALSE and average_strategies <- TRUE). Also loads the required packages.

### data__new_research.R, data_average_strategies.R, data_for_replication.R

Longer scripts only for creating the necessary variables for performing data analysis. Used files are PredictorData2017.xlsx from Amit Goyal's homepage, and SNP500 data from yahoo.com
