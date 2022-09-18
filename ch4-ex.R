# ch4-ex
setwd("D:\\undergraduate\\研\\432统计学\\应用时间序列分析资料\\R解答")
library(openxlsx)
library(tseries)
library(aTSA)
library(forecast)

## ex1
rm(list=ls(all=TRUE))
E4_1 <- read.xlsx(".\\datafile\\E4_1.xlsx", sheet=1)
x <- ts(E4_1$x)
plot(x)
adf.test(x)
Box.test(x)
acf(x)
pacf(x)
fit <- arima(x, order=c(1,0,0), include.mean = F)
ts.diag(fit)
forecast(fit, h=60)

## ex2
rm(list=ls(all=TRUE))
E4_2 <- read.xlsx(".\\datafile\\E4_2.xlsx", sheet=1)
x <- ts(E4_2$x)
plot(x)
adf.test(x)
Box.test(x)
acf(x)
pacf(x)
fit <- arima(x, order=c(0,0,1))
ts.diag(fit)
forecast(fit, h=60)

## ex3
rm(list=ls(all=TRUE))
E4_3 <- read.xlsx(".\\datafile\\E4_3.xlsx", sheet=1)
x <- ts(E4_3$x)
plot(x)
adf.test(x)
Box.test(x)
acf(x)
pacf(x)
fit <- arima(x, order=c(2,0,0))
ts.diag(fit)
forecast(fit, h=12)

## ex4
rm(list=ls(all=TRUE))
E4_4 <- read.xlsx(".\\datafile\\E4_4.xlsx", sheet=1)
x <- ts(E4_4$x)
plot(x)
adf.test(x)
Box.test(x)
acf(x)
pacf(x)
fit1 <- arima(x, order=c(0,0,1)); fit1
fit2 <- arima(x, order=c(1,0,0)); fit2
fit3 <- arima(x, order=c(1,0,1)); fit3
forecast(fit1, h=5)

## ex6
rm(list=ls(all=TRUE))
E4_6 <- read.xlsx(".\\datafile\\E4_6.xlsx", sheet=1)
x <- ts(E4_6$x)
acf(x)
pacf(x)
fit <- arima(x, order=c(1,0,0))
ts.diag(fit)
forecast(fit, h=5)

## ex7
rm(list=ls(all=TRUE))
E4_7 <- read.xlsx(".\\datafile\\E4_7.xlsx", sheet=1)
x <- ts(E4_7$x)
plot(x)
adf.test(x)
Box.test(x)
acf(x)
pacf(x)
fit <- arima(x, order=c(1,0,1))
ts.diag(fit)
forecast(fit, h=5)

## ex8
rm(list=ls(all=TRUE))
E4_8 <- read.xlsx(".\\datafile\\E4_8.xlsx", sheet=1)
x <- ts(E4_8$x)
acf(x)
pacf(x)
fit <- arima(x, order=c(0,0,1))
ts.diag(fit)
forecast(fit, h=1)

## ex9
rm(list=ls(all=TRUE))
E4_9 <- read.xlsx(".\\datafile\\E4_9.xlsx", sheet=1)
x <- ts(E4_9$x)
plot(x)
adf.test(x)
Box.test(x)
acf(x)
pacf(x)
fit <- arima(x, order=c(4,0,1), transform.pars = F, fixed=c(0,0,NA,NA,NA,NA))
ts.diag(fit)
forecast(fit, h=5)

