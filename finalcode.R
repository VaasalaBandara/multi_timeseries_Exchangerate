########################## Installing furthur required packages ##########################################
update.packages(ask = FALSE, checkBuilt = TRUE)#updating available packages prior to running code
install.packages("vars")
install.packages("mFilter")
install.packages("tseries")
install.packages("TSstudio")
install.packages("forecast")
install.packages("ragg")
install.packages("tidyverse")
install.packages("ggplot2")
################################ importing the csv file ##############################################
df <- read.csv("/home/vaasala/Desktop/extra_coding/multiproject/import_export_exchangerate.csv")
head(df)#visualizing the first 6 rows of the dataframe

########################## Data preprocessing #################################################3

df_trans<-t(df)#obtaining the transpose of the original dataframe
print(df_trans)#printing the transposed dataframe
df_trans <- df_trans[-(1:4), ]#removing first four rows from the transposed dataframe
print(df_trans)#printing the transposed dataframe
data <- df_trans[, -c(1,6,11,16,18)]#removing unwanted columns from the dataframe
print(data)#printing the current dataframe

#removing unwanted variables
summary(data)#displays the five number summary
#the data is observed to be in the character class
data<-subset(data,select = -c(11,14))#removing the column 11 and 14 of the dataframe due to many zero values
print(data)#printing the dataframe

#converting characters to numeric
data<-apply(data,2,as.numeric)#converting all columns of the dataframe from chatacters to numeric values
summary(data)#dsiplays the five number summary 
data<-data[,1:13]#isolating the first 13 columns of the dataframe by removing other columns
summary(data)#observing give number summary

#removing variables with missing values
data<-subset(data,select = -c(1,3,5))#removing rows 1,3,5 dues to presence of 'NA' values
print(data)#printing dataframe
summary(data)#analysing five number summary


############################################# Visualizing each time series #############################

par(mfrow=c(2,1))#chart of 2 rows and 1 column
#consumer good imports time series
ts1<-ts(data[,1],start = c(2015,1),frequency=12)
print(ts1)
plot.ts(ts1,xlab="year",ylab="millions(USD)")
#investment goods imports time series
ts2<-ts(data[,2],start = c(2015,1),frequency=12)
print(ts2)
plot.ts(ts2,xlab="year",ylab="millions(USD)")

par(mfrow=c(2,1))#chart of 2 rows and 1 column
#agricultural exports time series
ts3<-ts(data[,3],start = c(2015,1),frequency=12)
print(ts3)
plot.ts(ts3,xlab="year",ylab="millions(USD)")
#industrial exports time series
ts4<-ts(data[,4],start = c(2015,1),frequency=12)
print(ts4)
plot.ts(ts4,xlab="year",ylab="millions(USD)")

par(mfrow=c(2,1))#chart of 2 rows and 1 column
#mineral exports time series
ts5<-ts(data[,5],start = c(2015,1),frequency=12)
print(ts5)
plot.ts(ts5,xlab="year",ylab="millions(USD)")
#Rice Imports time series
ts6<-ts(data[,6],start = c(2015,1),frequency=12)
print(ts6)
plot.ts(ts6,xlab="year",ylab="USD/MT")

par(mfrow=c(2,1))#chart of 2 rows and 1 column
#Sugar Imports time series
ts7<-ts(data[,7],start = c(2015,1),frequency=12)
print(ts7)
plot.ts(ts7,xlab="year",ylab="USD/MT")
#Crude Oil Imports time series
ts8<-ts(data[,8],start = c(2015,1),frequency=12)
print(ts8)
plot.ts(ts8,xlab="year",ylab="USD/Barrel")

par(mfrow=c(2,1))#chart of 2 rows and 1 column
#Tourist Earnings time series
ts9<-ts(data[,9],start = c(2015,1),frequency=12)
print(ts9)
plot.ts(ts9,xlab="year",ylab="Millions(USD)")
#monthly average exchange rates time series
ts10<-ts(data[,10],start = c(2015,1),frequency=12)
print(ts10)
plot.ts(ts10,xlab="year",ylab="units")

#plotting all time series simultaneously
library(forecast)#for time series forecasting
autoplot(cbind(ts1,ts2,ts3,ts4,ts5,ts6,ts7,ts8,ts9,ts10),ylab = "Time series of Exports and Imports and Exchange Rate")
##################### Importing further required packages ##########################################
library(vars)#modelling and analyzing vector autoregressive models
library(mFilter)#filters and decomposing time series
library(tseries)#time series analysis and computational finance
library(TSstudio)#descriptive and predictive analysis of time series
library(tidyverse)#collection of packages designed for data science

######################################## philips perron test for stationarity ############################################
#null hypothesis: non stationary 
#alternate hypothesis: stationary
pp.test(ts1)
#p value=0.01<0.05. therefore we reject the null hypothesis and accept the alternate hypothesis to be stationary
pp.test(ts2)
#p value=0.01<0.05. therefore we reject the null hypothesis and accept the alternate hypothesis to be stationary
pp.test(ts3)
#p value=0.01<0.05. therefore we reject the null hypothesis and accept the alternate hypothesis to be stationary
pp.test(ts4)
#p value=0.01<0.05. therefore we reject the null hypothesis and accept the alternate hypothesis to be stationary
pp.test(ts5)
#p value=0.01<0.05. therefore we reject the null hypothesis and accept the alternate hypothesis to be stationary
pp.test(ts6)
#p-value = 0.05421>0.05 suggests that the time series is non stationary
diff_ts6<-diff(ts6)
pp.test(diff_ts6)
plot.ts(diff_ts6)
#pvalue=0.01<0.05 suggest that the ts6 time series is stationary for 1st difference
pp.test(ts7)
#p-value = 0.9023>0.05 suggests non stationarity
diff_ts7<-diff(ts7)
pp.test(diff_ts7)
plot.ts(diff_ts7)
# p-value = 0.01<0.05 implies that it is stationary for 1st difference
pp.test(ts8)
# p-value = 0.01<0.05 therefore we reject the null hypothesis and accept the alternate hypothesis to be stationary
pp.test(ts9)
#p-value = 0.3563>0.05 suggests non stationarity
diff_ts9<-diff(ts9)
pp.test(diff_ts9)
#p-value = 0.01<0.05 suggests stationarity
plot.ts(diff_ts9)
pp.test(ts10)
#p-value = 0.3756>0.05, suggests non stationarity
diff_ts10<-diff(ts10)
pp.test(diff_ts10)
#p-value = 0.01<0.05 implies stationarity

########################################################### model  ############################################################################


#combining tine series into one dataframe
v1 <- cbind(ts1, ts2, ts3, ts4, ts5, diff_ts6, diff_ts7, ts8, diff_ts9, diff_ts10)#combining R objects as columns
print(v1)
#assigning column names to the new object
colnames(v1) <- cbind("consumer good imports","investment goods imports","agricultural exports", "industrial exports", 
                      "mineral exports","Rice Imports ","Sugar Imports ","Crude Oil Imports ","Tourist Earnings ", "monthly average exchange rates")
print(v1)

#Removing rows with 'NA' values
v1<-na.omit(v1)
print(v1)
########################################## Model selection criteria ############################################

#we will select the lag order by using the command VARselect() .The command will automatically generate lag order based on 
#multivariate iterations of the AIC, SBIC, HQIC and the FPE
#AIC=akike information criterion
#SBIC=swartz information criterion or bayesian information criterion
#HQIC=Hannan-Quinn Information Criterion 
#FPE=Final Prediction Error
lagselect <- VARselect(v1, lag.max = 20, type = "const")#the maximum lag is assigned as 20,constant intercept is optimal
lagselect$selection
#most criterion suggest the optimal lag length to be 5. Therefore we select the optimal lag length as 5.

######################################### model fitting #######################################################
Model1 <- VAR(v1, p = 5, type = "const", season = NULL, exog = NULL) 
#lag length of 5. There are no observed seasonal patterns in the time series. Also it is assumed to not consist exogenous variables
summary(Model1)

######################################### model diagnostics ######################################################
#tests for residual autocorrelation

#portmantau test
#null hypothesis: there is no residual autocorrelation
#alternate hypothesis: there is residual autocorrelation
Serial1 <- serial.test(Model1, lags.pt = 5, type = "PT.asymptotic")
print(Serial1)
#since thep-value < 2.2e-16 , we fail to reject the null hypothesis. There is no residual autocorrelation


#tests for normality of residuals
Norm1 <- normality.test(Model1, multivariate.only = TRUE)
print(Norm1)
#Jarque-Bera Test (JB-Test)
#H0: residuals are normally distributed
#H1: residuals are not normally dsitributed
#since p-value: 0.04137<0.05, we reject the null hypothesis and say that the residuals are not normally distributed

#Skewness Test
#H0: residuals are symmetric
#H1: residuals have skewness
#p-value: 0.4611>0.05, there we reject the null hypothesis and assume residuals have skewness

#Kurtosis Test
#H0: normally distributed residuals
#H1: residuals are not normally distributed
#p-value: 0.01312<0.05, therefore there is significant kurtosis in residuals

#The final conclusion is that the residuals are not normally distributed. Which can have issues on confidence intervals.
#But is a issue that can be neglected

#stability test(assessing presence of structural breaks)
Stability1 <- stability(Model1, type = "OLS-CUSUM")
plot(Stability1)
#since none of the points in the graph pass the red critical region, no structural brakes can be seen

####################################### Policy simulations #############################################

#Granger Causality
#null hypothesis: does not granger-cause other variables
#alternate hypothesis:granger -cause other variables

#f-test: long term causality. indicates the ability of one variable to predict the other
#chi-squared: instantaneous causality, or causlity within same time frame

Grangerts1<- causality(Model1, cause = "consumer.good.imports")
Grangerts1
#p-value = 7.438e-14<0.05, there is  granger-cause

Grangerts2<- causality(Model1, cause = "investment.goods.imports")
Grangerts2
#p-value = 1.064e-11<0.05,therefore there is granger-cause

Grangerts3<- causality(Model1, cause = "agricultural.exports")
Grangerts3
#p-value = 8.882e-16<0.05,therefore there is  granger cause

Grangerts4<- causality(Model1, cause = "industrial.exports")
Grangerts4
#p-value = 9.723e-13<0.05, therefore there is granger-cause
Grangerts5<- causality(Model1, cause = "mineral.exports")
Grangerts5
#p-value = 2.533e-08<0.05,therefore there is granger cause
Grangerts6<- causality(Model1, cause = "Rice.Imports.")
Grangerts6
#p-value = 0.001726<0.05,therefore there is granger cause
Grangerts7<- causality(Model1, cause = "Sugar.Imports.")
Grangerts7
#p-value = 5.149e-07<0.05,therefore there is granger cause
Grangerts8<- causality(Model1, cause = "Crude.Oil.Imports.")
Grangerts8
#p-value = 0.000331<0.05,therefore there is granger cause
Grangerts9<- causality(Model1, cause = "Tourist.Earnings.")
Grangerts9
#p-value = 6.661e-16<0.05, therefore there is granger-cause
Grangerts10<- causality(Model1, cause = "monthly.average.exchange.rates")
Grangerts10
#p-value = 1.457e-10<0.05, therefore there is granger cause


#Forecast Error Variance Decomposition

#we can trace the development of shocks in our system to explaining the forecast error variances of all the variables in the system
FEVD1 <- fevd(Model1, n.ahead = 10)
FEVD1
plot(FEVD1)


#Impulse Response Functions
ts1irf <- irf(Model1, response = "monthly.average.exchange.rates", impulse = "consumer.good.imports", n.ahead = 20, boot = TRUE)
plot(ts1irf)
#The shocks are temporary and dampen over time
ts2irf <- irf(Model1, response = "monthly.average.exchange.rates", impulse = "investment.goods.imports", n.ahead = 20, boot = TRUE)
plot(ts2irf)
#The shocks are temporary and will stabilize
ts3irf <- irf(Model1, response = "monthly.average.exchange.rates", impulse = "agricultural.exports", n.ahead = 20, boot = TRUE)
plot(ts3irf)
#The shocks are temporary and dampen over time
ts4irf <- irf(Model1, response = "monthly.average.exchange.rates", impulse = "industrial.exports", n.ahead = 20, boot = TRUE)
plot(ts4irf)
#The shock is not statistically significant
ts5irf <- irf(Model1, response = "monthly.average.exchange.rates", impulse = "mineral.exports", n.ahead = 20, boot = TRUE)
plot(ts5irf)
#shocks are temporary
ts6irf <- irf(Model1, response = "monthly.average.exchange.rates", impulse = "Rice.Imports.", n.ahead = 20, boot = TRUE)
plot(ts6irf)
#Shocks are temporary and dampen over time
ts7irf <- irf(Model1, response = "monthly.average.exchange.rates", impulse = "Sugar.Imports.", n.ahead = 20, boot = TRUE)
plot(ts7irf)
#The shock is not statistically significant
ts8irf <- irf(Model1, response = "monthly.average.exchange.rates", impulse = "Crude.Oil.Imports.", n.ahead = 20, boot = TRUE)
plot(ts8irf)
#The shocks are temprorary and damp over time. Variable shows mean reversion
ts9irf <- irf(Model1, response = "monthly.average.exchange.rates", impulse = "Tourist.Earnings.", n.ahead = 20, boot = TRUE)
plot(ts9irf)
#shocks are not statistically significant
ts10irf <- irf(Model1, response = "monthly.average.exchange.rates", impulse = "monthly.average.exchange.rates", n.ahead = 20, boot = TRUE)
plot(ts10irf)
#shocks are temporary

####################################### forecasting  model ##########################################################

forecast <- predict(Model1, n.ahead = 4, ci = 0.95)#predictions 2 years ahead
plot(forecast)
fanchart(forecast, names = "consumer.good.imports", main = "Fanchart for consumer.good.imports", xlab = "Horizon", ylab = "consumer.good.imports")
fanchart(forecast, names = "investment.goods.imports", main = "Fanchart for investment.goods.imports", xlab = "Horizon", ylab = "investment.goods.imports")
fanchart(forecast, names = "agricultural.exports", main = "Fanchart for agricultural.exports", xlab = "Horizon", ylab = "agricultural.exports")
fanchart(forecast, names = "industrial.exports", main = "Fanchart for industrial.exports", xlab = "Horizon", ylab = "industrial.exports")
fanchart(forecast, names = "mineral.exports", main = "Fanchart for mineral.exports", xlab = "Horizon", ylab = "mineral.exports")
fanchart(forecast, names = "Rice.Imports.", main = "Fanchart for Rice.Imports.", xlab = "Horizon", ylab = "Rice.Imports.")
fanchart(forecast, names = "Sugar.Imports.", main = "Fanchart for Sugar.Imports.", xlab = "Horizon", ylab = "Sugar.Imports.")
fanchart(forecast, names = "Crude.Oil.Imports.", main = "Fanchart for Crude.Oil.Imports.", xlab = "Horizon", ylab = "Crude.Oil.Imports.")
fanchart(forecast, names = "Tourist.Earnings.", main = "Fanchart for Tourist.Earnings.", xlab = "Horizon", ylab = "Tourist.Earnings.")
fanchart(forecast, names = "monthly.average.exchange.rates", main = "Fanchart for monthly.average.exchange.rates", xlab = "Horizon", ylab = "monthly.average.exchange.rates")

fanchart(forecast, names = "monthly.average.exchange.rates", main = "Monthly average exchange rate forecast", xlab = "Horizon", ylab = "monthly average exchange rate of Sri Lanka")
# Reverse the first difference to get the integrated time series
integrated_forecast <- cumsum(c(ts10[length(ts10)], forecast$fcst$monthly.average.exchange.rates))
integrated_forecast <- ts(integrated_forecast, start = c(2021,1), frequency = frequency(ts10))
# Plotting the fan chart


real<-c(190.49,194.07,196.98,197.42,199.58,199.81,199.98,200.49,201.98,201.08,201.85,201.39,201.46,201.73,255.81,319.44,358.94)
plot(integrated_forecast, type = "l", lty=2, col = "blue", ylim=c(180, max(real)), lwd = 2, 
     main = "Monthly average exchange rate variation",
     xlab = "year", ylab = "Monthly Average Exchange Rates")
length(real)
real_val <- ts(real, start= c(2021,1), frequency = frequency(ts10))
lines(real_val, col = "red", lty = 1, lwd = 2)
legend("topleft", legend = c("forecast", "actual values"), col = c("blue", "red"), lty = c(2, 1), lwd = 2)


#combined time series
real<-c(190.49,194.07,196.98,197.42,199.58,199.81,199.98,200.49,201.98,201.08,201.85,201.39,201.46,201.73,255.81,319.44,358.94)
length(real)
c1 <- c(ts10,integrated_forecast)
plot(c1, type = "l", lty=2, col = "blue", ylim=c(100, max(real)), xlim = c(2015, 2022 + 3/12), lwd = 2, 
     main = "Monthly average exchange rate variation",
     xlab = "year", ylab = "Monthly Average Exchange Rates")

real_val <- ts(real, start= c(2021,1), frequency = frequency(ts10))
c2 <- c(ts10,real_val)
lines(c2, col = "red", lty = 1, lwd = 2)
legend("topleft", legend = c("forecast", "actual values"), col = c("blue", "red"), lty = c(2, 1), lwd = 2)

# Assuming ts10 and integrated_forecast are time series objects
# Correct example using proper time series concatenation and plotting

# Create a time series for real values starting in January 2021
real_val <- ts(real, start = c(2021, 1), frequency = frequency(ts10))

# Combine the forecasted values (ts10 and integrated_forecast) sequentially
# Ensure both are properly aligned in time
forecast_combined <- ts(c(ts10, integrated_forecast), start = start(ts10), frequency = frequency(ts10))

# Combine the forecast series with real values
# Forecast and real values must be concatenated at the correct point
combined_forecast_real <- ts(c(ts10, real), start = start(ts10), frequency = frequency(ts10))

# Plot forecasted values
plot(forecast_combined, type = "l", lty = 2, col = "red", ylim = c(100, max(real)), 
     xlim = c(2015, 2022 + 3/12), lwd = 2, 
     main = "Monthly Average Exchange Rate Variation",
     xlab = "Year", ylab = "Monthly Average Exchange Rates")

# Add the real values (actual data)
lines(combined_forecast_real, col = "black", lty = 1, lwd = 1.5)

# Add a legend
legend("topleft", legend = c("Forecast", "Actual Values"), col = c("red", "black"), lty = c(2, 1), lwd = 2)

#mean absolute error
mabs <- mean(abs(real_val - integrated_forecast))
print(mabs)

#mean absolute percentage error

mapes <- mean(abs((real_val - integrated_forecast)/real_val))*100
print(mapes)
