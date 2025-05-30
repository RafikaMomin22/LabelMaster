# ARIMAX w/ external regressors:
library("tseries")
install.packages("forecast")
library(forecast)
setwd('/Users/RafikaMomin/Desktop') 
data <- read.csv("Labels.csv", header = F)
head(data)
data$V2 <- ts(data$V2, start=c(2005, 1), end=c(2020, 5), frequency=12)
data$V1 <- ts(data$V1, start=c(2005, 1), end=c(2020, 5), frequency=12)

head(data)
plot(data$V2) # non stationary data (but conduct a dickey-fuller test to test for presence)
plot(diff(data$V2))  #stationary data: mean around 0 and constant variance
# variance for above plot increases so take the log
difflabels <- diff(data$V2) 
fitAR <- arima(data$V2, order=c(1,0,0))   # instead of data$V2, use difflabels if data is NOT stationary
forecastar <- forecast(fitAR, h=2)
print(forecastar)
####

df <- data.frame("age"=c(34,55,87,5,34,28,93,2,98,101), "height"=c(160,155,170,167,189,140,176,142,152,123))
df
time <- c(2005:2014)
df$age <- ts(df$age, start=c(2005), end=c(2014), frequency=1)
df$height <- ts(df$height, start=c(2005), end=c(2014), frequency=1)
diffage <- diff(df$age) 
diffheight <- diff(df$height)

response <- c(165,170,190,120,165,142,110,220,100,119)
fit_diff_arx <- arima(response, order=c(1,0,0), xreg = df)
fit_diff_arx
summary(fit_diff_arx)
plot(response, type="l")

newdf <- data.frame("age"=c(34,55,87,5,34,28,93,2,98,101,46,32), "height"=c(160,155,170,167,189,140,176,142,152,123,165,151))
forecastnum <- forecast(fit_diff_arx, h=2)
newpredict <- data.frame("age"=c(46,32), "height"=c(165,151))
pred <- predict(fit_diff_arx, n.ahead=2, newxreg=newpredict)    # THIS WORKS!!!
pred
print(pred)
plot(newpredict, pred)

print(forecastnum)
plot(forecastnum, include=15)
