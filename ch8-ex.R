# ch8-ex
setwd("D:\\undergraduate\\研\\432统计学\\应用时间序列分析资料\\R解答")
library(openxlsx)
library(tseries)
library(aTSA)
library(forecast)
library(rugarch)

## ex1
rm(list=ls(all=TRUE))
E8_1 <- read.xlsx(".\\datafile\\E8_1.xlsx", sheet=1)
x <- ts(E8_1$x)
y <- ts(E8_1$y)
par(mfrow=c(1,1))
plot(x)
lines(y, col=2)
adf.test(x)
for(i in 1:6) print(Box.test(x, lag=3*i))
adf.test(y)
for(i in 1:6) print(Box.test(y, lag=3*i))

## ex2
rm(list=ls(all=TRUE))
E8_2 <- read.xlsx(".\\datafile\\E8_2.xlsx", sheet=1)
x <- ts(E8_2$x)
y <- ts(E8_2$y)
par(mfrow=c(1,1))
plot(x)
lines(y, col=2)
x1 <- x[-c(1,2)]
y1 <- y[-c(37,38)]
fit <- Arima(y1, xreg=x1, order=c(2,0,0)); fit
tsdiag(fit)
par(mfrow=c(1,2))
acf(diff(x, 10))
pacf(diff(x, 10))
fitx <- arima(x, order=c(2,0,0), seasonal=list(order=c(0,1,1), period=10)); fitx
ts.diag(fitx)
forex <- forecast(fitx, h=14); forex
par(mfrow=c(1,1))
plot(forex)
lines(forex$fitted, col=2)
forey <- forecast(fit, xreg=forex$mean, h=14); forey
par(mfrow=c(1,1))
plot(forey)
lines(forey$fitted, col=2)

## ex3
rm(list=ls(all=TRUE))
E8_3 <- read.xlsx(".\\datafile\\E8_3.xlsx", sheet=1)
x <- ts(E8_3$x, start=1950)
y <- ts(E8_3$y, start=1950)
par(mfrow=c(1,1))
plot(x)
lines(y, col=2)
par(mfrow=c(1,2))
acf(diff(diff(x)))
pacf(diff(diff(x)))
fitx <- arima(x, order=c(0,2,0)); fitx
ts.diag(fitx)
par(mfrow=c(1,2))
acf(diff(diff(y)))
pacf(diff(diff(y)))
fity <- arima(y, order=c(2,2,0), transform.pars=F, fixed=c(0,NA)); fity
ts.diag(fity)
coint.test(y, x)
fit <- Arima(y, xreg=x, order=c(2,0,0)); fit
tsdiag(fit)
ecm(y, x)

## ex4
rm(list=ls(all=TRUE))
E8_4 <- read.xlsx(".\\datafile\\E8_4.xlsx", sheet=1)
x <- ts(E8_4$x, start=1979)
y <- ts(E8_4$y, start=1979)
par(mfrow=c(1,2))
plot(x)
plot(y)
par(mfrow=c(1,2))
acf(diff(diff(x)))
pacf(diff(diff(x)))
fitx <- arima(x, order=c(3,2,0), transform.pars=F, fixed=c(0,0,NA)); fitx
ts.diag(fitx)
forex <- forecast(fitx, h=5); forex
par(mfrow=c(1,2))
acf(diff(diff(y)))
pacf(diff(diff(y)))
fity <- arima(y, order=c(2,2,0), transform.pars=F, fixed=c(0,NA)); fity
ts.diag(fity)
forey <- forecast(fity, h=5); forey
coint.test(y,x)

## ex5
rm(list=ls(all=TRUE))
Seatbelts
drivers <- Seatbelts[,2]
front <- Seatbelts[,3]
rear <- Seatbelts[,4]
kms <- Seatbelts[,5]
petroprice <- Seatbelts[,6]
law <- Seatbelts[,8]
X <- matrix(Seatbelts[, c(5,6,8)], ncol=3)
coint.test(drivers, X)
fit1 <- arima(drivers, xreg=data.frame(petroprice, law), order=c(2,0,0),
            seasonal=list(order=c(1,1,0), period=12) )
fit1
ts.diag(fit1)
coint.test(front, X)
fit2 <- arima(front, xreg=data.frame(petroprice, law), order=c(2,0,0),
            seasonal=list(order=c(1,1,0), period=12) )
fit2
ts.diag(fit2)
coint.test(rear, X)
fit3 <- arima(rear, xreg=data.frame(kms, petroprice), order=c(1,0,0),
            seasonal=list(order=c(1,1,0), period=12) )
fit3
ts.diag(fit3)
Y <- matrix(Seatbelts[, c(3,4)], ncol=2)
coint.test(drivers, Y)

## ex6
rm(list=ls(all=TRUE))
E8_6 <- read.xlsx(".\\datafile\\E8_6.xlsx", sheet=1)
x1 <- ts(E8_6$maize_price, start=1967)
x2 <- ts(E8_6$maize_yield, start=1967)
x3 <- ts(E8_6$pig_price, start=1967)
x4 <- ts(E8_6$pig_yield, start=1967)
x5 <- ts(E8_6$salary, start=1967)
plot(x1)
adf.test(x1)
adf.test(diff(x1))
plot(x2)
adf.test(x2)
adf.test(diff(x2))
plot(x3)
adf.test(x3)
adf.test(diff(x3))
plot(x4)
adf.test(x4)
adf.test(diff(x5))
plot(x5)
adf.test(x5)
adf.test(diff(x5))
X <- data.frame(x1, x2, x3, x4)
X <- as.matrix(X)
coint.test(x5, X, d=1)
fit <- arima(x5, xreg=data.frame(x1,x3,x4), order=c(3,0,0), transform.pars=T,
           fixed=c(NA,0,NA,NA,NA,NA,NA))
fit
ts.diag(fit)
ecm(x5, as.matrix(data.frame(x1, x3, x4)))

## ex7
rm(list=ls(all=TRUE))
E8_7 <- read.xlsx(".\\datafile\\E8_7.xlsx", sheet=1)
logM1 <- ts(E8_7$logM1, start=c(1954,1), frequency=4)
logGDP <- ts(E8_7$logGNP, start=c(1954,1), frequency=4)
sr <- ts(E8_7$sr, start=c(1954,1), frequency=4)
lr <- ts(E8_7$lr, start=c(1954,1), frequency=4)
plot(logM1)
adf.test(logM1)
adf.test(diff(logM1))
par(mfrow=c(1,2))
acf(diff(logM1))
pacf(diff(logM1))
fit1 <- arima(logM1, order=c(1,1,0)); fit1
ts.diag(fit1)
plot(logGDP)
adf.test(logGDP)
adf.test(diff(logGDP))
par(mfrow=c(1,2))
acf(diff(logGDP))
pacf(diff(logGDP))
fit2 <- arima(logGDP, order=c(2,1,1)); fit2
ts.diag(fit2)
plot(sr)
adf.test(sr)
adf.test(diff(sr))
par(mfrow=c(1,2))
acf(diff(sr))
pacf(diff(sr))
fit3 <- arima(sr, order=c(7,1,1), transform.pars=F, fixed=c(0,NA,0,0,0,0,NA,NA)); fit3
ts.diag(fit3)
plot(lr)
adf.test(lr)
adf.test(diff(lr)
par(mfrow=c(1,2))
acf(diff(lr))
pacf(diff(lr))
fit4 <- arima(lr, order=c(14,1,0), transform.pars=F, fixed=c(NA,0,0,0,0,0,0,0,0,0,0,0,0,NA)); fit4
ts.diag(fit4)
lagGNP <- lag(logGDP)
y <- logGDP[1:134]
X <- data.frame(logM1[1:134], sr[1:134], lr[1:134], lagGNP[2:135])
X <- as.matrix(X)
coint.test(y, X)
fit <- arima(y, xreg=data.frame(sr[1:134], lr[1:134], lagGNP[2:135])); fit
ts.diag(fit)
ecm(y, as.matrix(data.frame(sr[1:134], lr[1:134], lagGNP[2:135])))

