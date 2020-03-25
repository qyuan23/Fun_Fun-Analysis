##### The purpose of this code is to use time series algorithm to analyse & predict rain volumn in Boston.

##### The data was downloaded from https://www.ncdc.noaa.gov/cdo-web/search?datasetid=GHCND
##### Boston, MA (USW00014739) was chosen as the city to analyze its precipitation(PRCP) over the years. Precipitation data was collected daily from January 1, 1936 to December 31, 2018. 


library(tidyverse)
library(readxl)
library(corrgram)
library(tseries)
library(forecast)

RainBostonAll <- read_excel("old Boston_PRCP.xlsx")

dim(RainBostonAll)    #returns the dimensions of an object
str(RainBostonAll)    #returns the structure of an object
sum(is.na(RainBostonAll)) #returns how many observations have "na"
RainBostonAll[is.na(RainBostonAll)] <- '0'      #replaces "na" with 0. This is a choice, statistically, but you can't run the regression without it
sum(is.na(RainBostonAll))     # check to see if there is any more NA
View(RainBostonAll)


##### Thinking process: Create date variables, collapse day into monthly averages, then compare plots of daily vs. monthly PRCP

RainBostonAll$DATE <- as.POSIXct(RainBostonAll$DATE, format="%Y-%m-%d")
RainBostonAll$PRCP <- as.numeric(RainBostonAll$PRCP)

# aggregate day into month, and get monthly average PRCP --- better to see the trend & seasonality
MonthlyRain <- aggregate(list(rain = RainBostonAll$PRCP),list(month = cut(RainBostonAll$DATE, "month")), mean)
MonthlyRain

# monthly rain data
MonthlyRain2 <- ts(MonthlyRain$rain, frequency = 12, start = c(1936,1))      # ts: time series

# daily rain data
Rain<-ts(RainBostonAll$PRCP, frequency = 365, start = c(1936,1))     

# create a plot of the time series
plot.ts(Rain)
plot.ts(MonthlyRain2)

# identify the trend/season/random components
RainParts<-decompose(Rain)
RainMonthParts<-decompose(MonthlyRain2)     # Random is the left-data after taking out trend & seasonality. if there is no random variable, the 4th plot should be much like a straight line.
plot(RainParts)
plot(RainMonthParts)


# Modeling: use exponential smoothing method(with daily rain data)

RainModel1<-HoltWinters(Rain)    #HoltWinters() use exponential smoothing
RainModel1
RainModel1$SSE         # sum squared error, as the indicator whether this exponential smoothing model is good, comparing to the data has same sample size & only time series as the variable
plot(RainModel1, col=6, col.predicted=1)    #color3 = actual data, color 9 = predicted
residualsHolt1<-residuals(RainModel1)    #
plot(residualsHolt1)
acf(residualsHolt1)       # to check if there are auto-correlation between residuals. residuals are supposed to be independent/no auto-correlation. 
pacf(residualsHolt1)      # no correlation: no lines above blue lines or <= 1 lag (significant=very tall) above the blue line;  has correlation: >= 2 lags(significant=very tall) above the blue lines. If there is correlation of residuals, indicating that exponential smoothing is not a good model, try different modeling
 
# Modeling: use exponential smoothing(with monthly rain data)

RainModel2<-HoltWinters(MonthlyRain2)
RainModel2
RainModel2$SSE
plot(RainModel2, col=5, col.predicted=1)
residualsHolt2<-residuals(RainModel2)
plot(residualsHolt2)
acf(residualsHolt2)
pacf(residualsHolt2)


##### Predicting the future PRCP

# Forecasting using exponential smooting(with daily rain data)
RainForecast<-forecast(Rain, h=1000)      # h=1000 days in the future
plot(RainForecast)

# Forecasting using exponential smooting(with monthly rain data)

RainForecast2<-forecast(MonthlyRain2, h=3)            # h=3 months in the future
plot(RainForecast2)

# Modeling using an auto.arima model - Full Data 

par(mfrow=c(1,2))  #plot the acf and pacf side by side
acf(Rain)
pacf(Rain)

RainArima <- auto.arima(Rain, seasonal = TRUE)      
RainArima <- auto.arima(Rain, trace = TRUE)             
RainArima
residuals.auto1<-residuals(RainArima)    
plot(residuals.auto1)
acf(ts(RainArima$residuals), main='ACF Residual - Full')
pacf(ts(RainArima$residuals), main='PACF Residual - Full')



# Modeling using an auto.arima model - Monthly Data 

acf(MonthlyRain2)
pacf(MonthlyRain2)

RainArima2<-auto.arima(MonthlyRain2, seasonal = TRUE)       
RainArima2 <- auto.arima(MonthlyRain2, trace = TRUE) 
RainArima2
residuals.auto2<-residuals(RainArima2)    #
plot(residuals.auto2)
acf(ts(RainArima2$residuals), main='ACF Residual - Monthly')
pacf(ts(RainArima2$residuals), main='PACF Residual- Monthly')

RainArima3 <- arima(MonthlyRain2, order = c(1, 0, 0), seasonal = c(4,0,2), include.mean = FALSE, include.drift = FALSE, include.constant =FALSE, method = "ML")    # the c() of seasonal is not from solid proof

prediction=predict(RainArima,n.ahead=1000)    
prediction                                  
plot(forecast(RainArima, h=1000))


prediction=predict(RainArima2,n.ahead=3)
prediction
plot(forecast(RainArima2, h=3))
