# ch6-ex
setwd("D:\\undergraduate\\研\\432统计学\\应用时间序列分析资料\\R解答")
library(openxlsx)
library(tseries)
library(aTSA)
library(forecast)

## ex1
rm(list=ls(all=TRUE))
E6_1 <- read.xlsx(".\\datafile\\E6_1.xlsx", sheet=1)
x <- ts(E6_1$x, start=c(1962,1), frequency=12)
plot(x)
fit1 <- decompose(x)
fit1$trend
plot(fit1$trend)
fit1$seasonal
plot(fit1$seasonal)
fit2 <- HoltWinters(x); fit2
fore <- forecast(fit2, h=24); fore
par(mfrow=c(1,1))
plot(fore)
lines(fore$fitted, col=2)
par(mfrow=c(1,2))
acf(diff(x))
pacf(diff(x))
fit3 <- arima(x, order=c(1,1,0), seasonal=list(order=c(0,1,1), period=12)); fit3
par(mfrow=c(1,1))
plot(x)
lines(fitted(fit3), col=2)
c(mean(na.exclude(fit1$random)^2), mean((x-fit2$fit[,1])^2), mean(fit3$residuals^2))

## ex2
rm(list=ls(all=TRUE))
E6_2 <- read.xlsx(".\\datafile\\E6_2.xlsx", sheet=1)
x <- ts(E6_2$x, start=c(1973,1), frequency=12)
plot(x)
fit1 <- decompose(x)
fit1$trend
plot(fit1$trend)
fit1$seasonal
plot(fit1$seasonal)
fit2 <- HoltWinters(x); fit2
fore <- forecast(fit2, h=24); fore
par(mfrow=c(1,1))
plot(fore)
lines(fore$fitted, col=2)
par(mfrow=c(1,2))
acf(diff(x))
pacf(diff(x))
fit3 <- arima(x, order=c(1,1,0), seasonal=list(order=c(0,1,1), period=12)); fit3
par(mfrow=c(1,1))
plot(x)
lines(fitted(fit3), col=2)
c(mean(na.exclude(fit1$random)^2), mean((x-fit2$fit[,1])^2), mean(fit3$residuals^2))

## ex6
rm(list=ls(all=TRUE))
E6_6 <- read.xlsx(".\\datafile\\E6_6.xlsx", sheet=1)
x <- ts(E6_6$x, start=c(1949,1))
plot(x)
fit1 <- HoltWinters(x, gamma=F); fit1
par(mfrow=c(1,2))
acf(diff(x))
pacf(diff(x))
fit2 <- arima(x, order=c(1,1,0)); fit2
c(mean((x-fit1$fit[,1])^2), mean(fit2$residuals^2))
fore <- forecast(fit2, h=8); fore
par(mfrow=c(1,1))
plot(fore)
lines(fore$fitted, col=2)

## ex7
rm(list=ls(all=TRUE))
E6_7 <- read.xlsx(".\\datafile\\E6_7.xlsx", sheet=1)
x <- ts(E6_7$x, start=c(1948,1), frequency=4)
plot(x)
fit1 <- HoltWinters(x, gamma=F); fit1
adf.test(diff(x))
adf.test(diff(diff(x)))
par(mfrow=c(1,2))
acf(diff(diff(x)))
pacf(diff(diff(x)))
fit2 <- Arima(x, order=c(0,2,16), transform.pars=F, fixed=c(NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,NA))
fit2
c(mean((x-fit1$fit[,1])^2), mean(fit2$residuals^2))
fore <- forecast(fit2, h=20); fore
par(mfrow=c(1,1))
plot(fore)
lines(fore$fitted, col=2)

## ex8
rm(list=ls(all=TRUE))
E6_8 <- read.xlsx(".\\datafile\\E6_8.xlsx", sheet=1)
x <- ts(E6_8$x,start=c(1980,1), frequency=12)
plot(x)
fit1 <- decompose(x)
plot(fit1)
fit2 <- HoltWinters(x); fit2
fore1 <- forecast(fit2, h=60); fore1
par(mfrow=c(1,1))
plot(fore1)
lines(fore1$fitted, col=2)
adf.test(diff(diff(x,12)))
fit2 <- Arima(x, order=c(2,1,0), seasonal=list(order=c(0,1,1), period=12)); fit2
fore2 <- forecast(fit2, h=60); fore2
par(mfrow=c(1,1))
plot(fore2)
lines(fore2$fitted, col=2)

## ex9
rm(list=ls(all=TRUE))
E6_9 <- read.xlsx(".\\datafile\\E6_9.xlsx", sheet=1)
x <- ts(E6_9$x, start=c(1963,1), frequency=12)
plot(x)
fit1 <- HoltWinters(x, seasonal="multi"); fit1
fore1 <- forecast(fit1, h=60); fore1
par(mfrow=c(1,1))
plot(fore1)
lines(fore1$fitted, col=2)
adf.test(diff(diff(x,12)))
fit2 <- Arima(x, order=c(1,1,1), seasonal=list(order=c(0,1,1), period=12)); fit2
fore2 <- forecast(fit2, h=60); fore2
par(mfrow=c(1,1))
plot(fore2)
lines(fore2$fitted, col=2)


