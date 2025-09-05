#read data from csv file
library(readr)
CocaColaStock <- read_csv("CocaColaStock.csv")
View(CocaColaStock)
#Required packages
library(tseries)
library(forecast)
library(lmtest)
library(seastests)

attach(CocaColaStock)
dataa <- as.vector(StockPrice)

#checking seasonality
kw(dataa, freq = 12) 
#our data is non seasonal data

#ploting
dataa.to.plot <- ts(dataa,frequency = 12,start = c(1970,1))
ts.plot(dataa, main= "The  Stationary TS", ylab = "Measure")
# Checking Stationarity
acf(dataa, lag.max = 20) 
pacf(dataa, lag.max = 82)

# Transforming to Stationarity

#trying suitable transformations to reach stationarity 
ts_data_sqrt <- sqrt(dataa)
ts_data_log <- log(dataa) #the suitable
ts_data_square <- 2^(dataa)

st_diff = diff(ts_data_log, differences=1)
ts.plot(st_diff,main=  "First difference")
acf(st_diff, lag.max = 30) 
pacf(st_diff, lag.max = 30) 

nd_diff = diff(ts_data_log, differences=2)
ts.plot(nd_diff,main=  "Second difference")
acf(nd_diff, lag.max = 30)
pacf(nd_diff , lag.max = 30)

library("urca")
adf_result <- ur.df(st_diff, lags = 1, type = "trend")
summary(adf_result)
kpss.test(st_diff)

adf_result <- ur.df(nd_diff, lags = 1, type = "trend")
summary(adf_result)
kpss.test(nd_diff)


# Specifying the Initial Model
Model1 = arima(ts_data_log,order = c(0,2,1), method = c("ML"))
Model2 = arima(ts_data_log,order = c(0,2,2), method = c("ML"))
Model3 = arima(ts_data_log,order = c(1,2,1), method = c("ML"))
Model4 = arima(ts_data_log,order = c(2,2,2), method = c("ML"))

summary (Model1)
summary (Model2) 
summary (Model3)
summary (Model4)
# Diagnosis Tests

#1 the significance of the coefficients
coeftest(Model1) 
coeftest(Model2) 
coeftest(Model3)
coeftest(Model4)    
   
    
#2 The randomness of the noise
Residuals1 = residuals(Model1)
Residuals2= residuals(Model2)
Residuals3= residuals(Model3)
Residuals4= residuals(Model4)

acf(Residuals1)
pacf(Residuals1)

acf(Residuals2)
pacf(Residuals2)

acf(Residuals3)
pacf(Residuals3)

acf(Residuals4)
pacf(Residuals4)

Box.test(Residuals1,type="Ljung",lag=10,fitdf=2)
Box.test(Residuals1,type="Box-Pierce",lag=10,fitdf=2)  # fitdf = p+q

Box.test(Residuals2,type="Ljung",lag=10,fitdf=2)
Box.test(Residuals2,type="Box-Pierce",lag=10,fitdf=2)

Box.test(Residuals3,type="Ljung",lag=10,fitdf=2)
Box.test(Residuals3,type="Box-Pierce",lag=10,fitdf=2)

Box.test(Residuals4,type="Ljung",lag=10,fitdf=2)
Box.test(Residuals4,type="Box-Pierce",lag=10,fitdf=2)
#3 the accuracy measures
accuracy(Model1)                       ## the accuracy measures: MAE, RMSE, MAPE

#Forecasting

f=forecast(Model2, h = 20)
plot(f)


