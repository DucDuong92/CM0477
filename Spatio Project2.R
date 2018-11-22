# This is the code used in the project of class Statistics for spatio-temporal data
# Author: Duc Duong (871492)
# Intrucstor: Prof. Gaetan
# Ca' Foscari University of Venice

# A comment begin with ## mean that it is the beginning of a big part
#---------------------------------------------

## Draw some time series
library(readxl)
temp <- read_excel("C:/Users/Duong Minh Duc/Desktop/tas_1901_2015.xls")
rain <- read_excel("C:/Users/Duong Minh Duc/Desktop/pr_1901_2015.xls")

temp.ts <- ts(temp$tas, start = 1901, frequency = 12)
rain.ts <- ts(rain$pr, start = 1901, frequency = 12)

plot(temp.ts, xlab= "Year", ylab= "Celsius degree", main="Temperature time series")
plot(rain.ts, xlab= "Year", ylab= "Millimeter", main="Rainfall time series")

#-----------------------------------------------

## Identify possible trends and seasonality
#monthplot
monthplot(temp.ts, main="Monthplot of temperature time series", ylab = "Celsius degree")
monthplot(rain.ts, main="Monthplot of Rainfall time series", ylab = "Millimeter")

#Decompose
temp.decompose <- stl(temp.ts, s.window="periodic")
rain.decompose <- stl(rain.ts, s.window="periodic")

plot(temp.decompose, main = "Decompose the temperature time series", xlab="Year")
plot(rain.decompose, main = "Decompose the rainfall time series", xlab="Year")

#-------------------------------------

##Check the stationary and evaluate by ARIMA model
library(tseries)
library(forecast)
#check the stationary
adf.test(temp.ts)
adf.test(rain.ts)

#Draw ACF and PACF graph
acf(diff(temp$tas, 12), main= "ACF of temperature with difference of lag 12")
pacf(diff(temp$tas, 12), main= "PACF of temperature with difference of lag 12")

acf(diff(rain$pr, 12), main= "ACF of rainfall with difference of lag 12")
pacf(diff(rain$pr, 12), main= "PACF of rainfall with difference of lag 12")

#Fitting ARIMA models
fitARIMAtemp1 <-arima(temp.ts, order = c(1,0,0), seasonal=c(0,1,1))
fitARIMAtemp2 <-arima(temp.ts, order = c(1,0,0), seasonal=c(1,1,1))

fitARIMArain1 <-arima(rain.ts, order = c(1,0,1), seasonal=c(0,1,1))
fitARIMArain2 <-arima(rain.ts, order = c(1,0,2), seasonal=c(0,1,1))

#Calculate the AIC and BIC

AIC(fitARIMAtemp1)
AIC(fitARIMAtemp2)
BIC(fitARIMAtemp1)
BIC(fitARIMAtemp2)

AIC(fitARIMArain1)
AIC(fitARIMArain2)
BIC(fitARIMArain1)
BIC(fitARIMArain2)

#ACF and PACF for residuals of ARIMA model
acf(residuals(fitARIMAtemp1), main= "ACF for residuals of the temperature")
pacf(residuals(fitARIMAtemp1), main= "PACF for residuals of the temperature")

acf(residuals(fitARIMArain1), main= "ACF for residuals of the rainfall")
pacf(residuals(fitARIMArain1),main= "PACF for residuals of the rainfall")


##Forecast 2 years
foretemp <- forecast(fitARIMAtemp1, h=24)
plot(foretemp, xlab= "Year", ylab =  "Celsius degree")

forerain <- forecast(fitARIMArain1, h=24)
plot(forerain, xlab= "Year", ylab = "Millimeter")