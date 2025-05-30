install.packages("tseries")
library(tseries)
myts <- ts(2005:2021, start=c(2005, 1), end=c(2020, 5), frequency=12)
head(myts)

myts <- c(2005:2021)
# xreg must have same number of rows as myts. xreg is vector/matrix NOT a df
x <- (c(2060:2076, 2045:2061))
x
dim(x) <- c(2, 17)
x
arimax(myts, order = c(0,0,0), seasonal = c(0,0,0), xreg = x )

myts <- c(2005:2021)
myts
x <- matrix(1:16, nrow=1)
x

arima(myts, order = c(0,0,0), seasonal = c(0,0,0), xreg = x )


# Dickey-Fuller Test for stationarity 
adf.test(myts, alternative="stationary", k=0)



# ARIMA without external regressors:
install.packages("forecast")
library(forecast)
setwd('/Users/RafikaMomin/Desktop') 
data <- read.csv("Labels.csv", header = F)
head(data)
data$V1 <- ts(data$V1, start=c(2005, 1), end=c(2020, 5), frequency=12)
head(data)
plot(data)
difflabels <- diff(data$V2) 
fit_diff_ar <- arima(difflabels, order=c(1,0,0))
# fit_diff_ar <- arima(data$V2, order=c(1,0,0))

fit_diff_ar
summary(fit_diff_ar)
plot(difflabels)
# variance for above plot increases so take the log
forecast <- forecast(fit_diff_ar, h=3)
print(forecast)
plot(forecast, include=5)
