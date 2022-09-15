# ch2-ex
setwd("D:\\undergraduate\\研\\432统计学\\应用时间序列分析资料\\R解答")
library(openxlsx)
library(tseries)

## ex1
rm(list=ls(all=TRUE))
x <- 1:20
x <- ts(x)
acf(x)$acf

## ex2
rm(list=ls(all=TRUE))
E2_2 <- read.xlsx(".\\datafile\\E2_2.xlsx", sheet=1)
co2 <- ts(E2_2$co2, start=c(1975,1), frequency = 12)
plot(co2, type="o")
acf(co2, lag.max=24)$acf

## ex3
rm(list=ls(all=TRUE))
E2_3 <- read.xlsx(".\\datafile\\E2_3.xlsx", sheet=1)
rain <- ts(E2_3$rain, start=c(1945,1), frequency = 12)
plot(rain, type="o")
acf(rain, lag.max=24)$acf
for(i in 1:6) print(Box.test(rain, lag=3*i))

## ex4
rm(list=ls(all=TRUE))
Q_test <- function(n,r0){
  k <- length(r0)
  Q = 0
  P = 0
  for(i in 1:k) {
    Q[i] <- n*sum(r0[1:i]^2)
    P[i] <- 1-pchisq(Q[i], df=i)
  }
  return(data.frame(Q, P))
}
r0 <- c(0.02,0.05,0.1,-0.02,0.05,0.01,0.12,-0.06,0.08,-0.05,0.02,-0.05)
Q_test(100, r0)

## ex5
rm(list=ls(all=TRUE))
E2_5 <- read.xlsx(".\\datafile\\E2_5.xlsx", sheet=1)
x <- ts(E2_5$x, start=c(2000,1), frequency=12)
par(mfrow=c(1,2))
plot(x, type="o")
acf(x)
for(i in 1:2) print(Box.test(x, lag=3*i))

## ex6
rm(list=ls(all=TRUE))
E2_6 <- read.xlsx(".\\datafile\\E2_6.xlsx", sheet=1)
x <- ts(E2_6$x, start=c(1969, 1), frequency=12)
plot(x)
acf(x)
adf.test(x)
for(i in 1:2) print(Box.test(x, lag=3*i))
y <- diff(x)
adf.test(y)
for(i in 1:2) print(Box.test(y, lag=3*i))

## ex7
rm(list=ls(all=TRUE))
E2_7 <- read.xlsx(".\\datafile\\E2_7.xlsx", sheet=1)
x <- ts(E2_7$mortality, start=c(1915,1))
par(mfrow=c(1,2))
plot(x)
acf(x, lag.max=24)
adf.test(x)
for(i in 1:2) print(Box.test(x, lag=3*i))
adf.test(diff(x))
for(i in 1:2) print(Box.test(diff(x), lag=3*i))

## ex8
rm(list=ls(all=TRUE))
E2_8 <- read.xlsx(".\\datafile\\E2_8.xlsx", sheet=1)
x <- ts(E2_8$wl, start=c(1860,1))
par(mfrow=c(1,2))
plot(x)
acf(x, lag.max=24)
adf.test(x)
for(i in 1:2) print(Box.test(x, lag=3*i))

