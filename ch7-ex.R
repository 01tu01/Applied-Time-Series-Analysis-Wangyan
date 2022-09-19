# ch7-ex
setwd("D:\\undergraduate\\研\\432统计学\\应用时间序列分析资料\\R解答")
library(openxlsx)
library(tseries)
library(aTSA)
library(forecast)
library(rugarch)

## ex1
rm(list=ls(all=TRUE))
E7_1 <- read.xlsx(".\\datafile\\E7_1.xlsx", sheet=1)
x <- ts(E7_1$x)
plot(x)
adf.test(x)
par(mfrow=c(1,2))
plot(diff(x))
plot(diff(x)^2)
adf.test(diff(x))
spec <- ugarchspec(mean.model=list(armaOrder=c(0,0), include.mean=F),
variance.model=list(garchOrder=c(1,0), model="sGARCH"),
distribution.model="norm")
fit <- ugarchfit(spec, data=diff(x), method="ML"); fit
a = (fit@fit$sigma[length(fit@fit$sigma)])^2
ht = fit@fit$coef[1]+fit@fit$coef[2]*a
sumh <- cumsum(ht)
xt <- x[length(x)]
sigma_fore <- sqrt(sumh)
l95 = xt-1.96*sigma_fore
u95 = xt+1.96*sigma_fore
data.frame(xt, sigma_fore, l95, u95)

## ex2
rm(list=ls(all=TRUE))
E7_2 <- read.xlsx(".\\datafile\\E7_2.xlsx", sheet=1)
x <- ts(E7_2$x, start=c(1969,1), frequency=12)
plot(x)
adf.test(x)
par(mfrow=c(1,2))
plot(diff(x))
plot(diff(x)^2)
adf.test(diff(x))
arch.test(arima(x, order=c(0,1,0)))
spec <- ugarchspec(mean.model=list(armaOrder=c(0,0), include.mean=F),
variance.model=list(garchOrder=c(2,1), model="sGARCH"),
distribution.model="norm")
fit <- ugarchfit(spec, data=diff(x), method="ML"); fit

## ex3
rm(list=ls(all=TRUE))
E7_3 <- read.xlsx(".\\datafile\\E7_3.xlsx", sheet=1)
x <- ts(E7_3$x, start=c(1974,1), frequency=12)
par(mfrow=c(1,3))
plot(x)
plot(diff(x))
plot(diff(x)^2)
adf.test(diff(x))
for(i in 1:6) print(Box.test(diff(x), i))
arch.test(arima(x, order=c(0,1,0)))
sd(diff(x))
sd(diff(diff(x)))
spec <- ugarchspec(mean.model=list(armaOrder=c(0,0), include.mean=F),
variance.model=list(garchOrder=c(2,0), model="sGARCH"),
distribution.model="norm")
fit <- ugarchfit(spec, data=diff(x), method="ML"); fit
ht=0
a=(fit@fit$sigma[length(fit@fit$sigma)])^2
for (i in 1:12){
	ht[i]=fit@fit$coef[1]+(fit@fit$coef[2]+fit@fit$coef[3])*a
	a=ht[i]
}
sumh <- cumsum(ht)
xt <- x[length(x)]
sigma_fore <- sqrt(sumh)
l95 = xt-1.96*sigma_fore
u95 = xt+1.96*sigma_fore
data.frame(xt, sigma_fore, l95, u95)

## ex4
rm(list=ls(all=TRUE))
E7_4 <- read.xlsx(".\\datafile\\E7_4.xlsx", sheet=1)
x <- ts(E7_4$x, start=c(1971,7), frequency=4)
par(mfrow=c(1,3))
plot(x)
plot(diff(x))
plot(diff(x)^2)
adf.test(diff(x))
for(i in 1:6) print(Box.test(diff(x), i))
par(mfrow=c(1,2))
acf(diff(x))
pacf(diff(x))
fit1 <- arima(x, order=c(3,1,0), transform.pars=F, fixed=c(0,0,NA)); fit1
fore <- forecast(fit1, h=4); fore
arch.test(arima(x, order=c(0,1,0)))
spec <- ugarchspec(mean.model=list(armaOrder=c(0,0), include.mean=F),
variance.model=list(garchOrder=c(2,0), model="sGARCH"),
distribution.model="norm", fixed.pars=c(0,0,NA))
fit2 <- ugarchfit(spec, data=diff(x), method="ML"); fit2
ht=0
a=(fit2@fit$sigma[length(fit2@fit$sigma)])^2
for (i in 1:4){
	ht[i]=fit2@fit$coef[1]+(fit2@fit$coef[2]+fit2@fit$coef[3])*a
	a=ht[i]
}
sumh <- cumsum(ht)
xt <- x[length(x)]
sigma_fore <- sqrt(sumh)
l95 = xt-1.96*sigma_fore
u95 = xt+1.96*sigma_fore
data.frame(xt, sigma_fore, l95, u95)

## ex5
rm(list=ls(all=TRUE))
E7_5 <- read.xlsx(".\\datafile\\E7_5.xlsx", sheet=1)
x <- ts(E7_5$x, start=c(1969,7), frequency=12)
par(mfrow=c(1,3))
plot(x)
plot(diff(x))
plot(diff(x)^2)
adf.test(diff(x))
for(i in 1:6) print(Box.test(diff(x), i))
par(mfrow=c(1,2))
acf(diff(x))
pacf(diff(x))
arch.test(arima(x, order=c(0,1,0)))
fit <- arima(x, order=c(1,1,0)); fit
fore <- forecast(fit, h=12); fore

## ex6
rm(list=ls(all=TRUE))
E7_6 <- read.xlsx(".\\datafile\\E7_6.xlsx", sheet=1)
x <- ts(E7_6$x, start=c(1985,1), frequency=12)
par(mfrow=c(1,3))
plot(x)
plot(diff(x))
plot(diff(x)^2)
par(mfrow=c(1,2))
acf(diff(x))
pacf(diff(x))
fit1 <- arima(x, order=c(0,1,0)); fit1
arch.test(arima(x, order=c(0,1,0)))
spec <- ugarchspec(mean.model=list(armaOrder=c(0,0), include.mean=F),
variance.model=list(garchOrder=c(1,1), model="sGARCH"),
distribution.model="norm")
fit2 <- ugarchfit(spec, data=diff(x), method="ML"); fit2
ht=0
a=(fit2@fit$sigma[length(fit2@fit$sigma)])^2
for (i in 1:12){
	ht[i]=fit2@fit$coef[1]+(fit2@fit$coef[2]+fit2@fit$coef[3])*a
	a=ht[i]
}
sumh <- cumsum(ht)
xt <- x[length(x)]
sigma_fore <- sqrt(sumh)
l95 = xt-1.96*sigma_fore
u95 = xt+1.96*sigma_fore
data.frame(xt, sigma_fore, l95, u95)

