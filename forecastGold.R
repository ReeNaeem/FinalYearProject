library(readr)
library(tsbox)
library(forecast)


gold <- read.csv("Gold.csv",  header = TRUE, sep = ",")
View(gold)
head(gold)
class(gold)
class(gold) = ts
## above i converted df into time series successfully ab line on gld1 doesn't appear to work for each country
## have to split each country for time series

## China
gld <- subset(gold, ï..Country == "China")
china2 <- subset(gld[c(2,3)])
china2

china2 <- ts_ts(ts_long(china2))

plot(china2)
abline(reg=lm(china2~time(china2)))


cycle(china2)
plot(aggregate(china2, nfrequency = .25 ,FUN=mean))

boxplot(china2~cycle(china2))


adf.test(diff(log(china2)), alternative="stationary", k=0)
acf(log(china2))
acf(diff(log(china2)))
pacf(diff(log(china2)))

auto.arima(china2)


fit13 <- arima(log(china2), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 3))
pred <- predict(fit13, n.ahead = 2*1)
ts.plot(china2,2.718^pred$pred, log = "y", lty = c(1,3))
#####################################################################

## USA
gld1 <- subset(gold, ï..Country == "USA")
usa2 <- subset(gld1[c(2,3)])


usa2 <- ts_ts(ts_long(usa2)) #converting into time series

plot(usa2)
abline(reg=lm(usa2~time(usa2)))


cycle(usa2)
plot(aggregate(usa2, nfrequency = .25 ,FUN=mean))

boxplot(usa2~cycle(usa2))


adf.test(diff(log(usa2)), alternative="stationary", k=0)
acf(log(usa2))
acf(diff(log(usa2)))
pacf(diff(log(usa2)))

auto.arima(usa2)


fit14 <- arima(log(usa2), c(0, 1, 1),seasonal = list(order = c(0, 0, 0), period = 1))
pred <- predict(fit14, n.ahead = 2*1)
ts.plot(usa2,2.718^pred$pred, log = "y", lty = c(1,3))
#####################################################################

## Russia
gld2 <- subset(gold, ï..Country == "Russia")
russia2 <- subset(gld2[c(2,3)])
russia2 <- subset(russia2, Year > 1992)

View(russia2)
russia2

russia2 <- ts_ts(ts_long(russia2)) #converting into time series

plot(russia2)
abline(reg=lm(russia2~time(russia2)))


cycle(russia2)

plot(aggregate(russia2, nfrequency = .25 ,FUN=mean))

boxplot(russia2~cycle(russia2))


adf.test(diff(log(russia2)), alternative="stationary", k=0)
acf(log(russia2))
acf(diff(log(russia2)))
pacf(diff(log(russia2)))


auto.arima(russia2)


fit15 <- arima(log(russia2), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 4))
pred <- predict(fit15, n.ahead = 2*1)

ts.plot(russia2,2.718^pred$pred, log = "y", lty = c(1,3))

######################################################################

## Germany
gld3 <- subset(gold, ï..Country == "Germany")
germ2 <- subset(gld3[c(2,3)])

View(germ2)
germ2

germ2 <- ts_ts(ts_long(germ2)) #converting into time series

plot(germ2)
abline(reg=lm(germ2~time(germ2)))


cycle(germ2)

plot(aggregate(germ2, nfrequency = .25 ,FUN=mean))

boxplot(germ2~cycle(germ2))


adf.test(diff(log(germ2)), alternative="stationary", k=0)
acf(log(germ2))
acf(diff(log(germ2)))
pacf(diff(log(germ2)))

auto.arima(germ2)


fit16 <- arima(log(germ2), c(0, 1, 1),seasonal = list(order = c(0, 0, 0), period = 1))
pred <- predict(fit16, n.ahead = 2*1)
ts.plot(germ2,2.718^pred$pred, log = "y", lty = c(1,3))

######################################################################

## France
gld4 <- subset(gold, ï..Country == "France")
france2 <- subset(gld4[c(2,3)])

View(france2)
france2

france2 <- ts_ts(ts_long(france2)) #converting into time series

plot(france2)
abline(reg=lm(france2~time(france2)))


cycle(france2)

plot(aggregate(france2, nfrequency = .25 ,FUN=mean))

boxplot(france2~cycle(france2))


adf.test(diff(log(france2)), alternative="stationary", k=0)
acf(log(france2))
acf(diff(log(france2)))
pacf(diff(log(france2)))

auto.arima(france2)


fit17 <- arima(log(france2), c(0, 1, 1),seasonal = list(order = c(0, 0, 0), period = 1))
pred <- predict(fit17, n.ahead = 2*1)
ts.plot(france2,2.718^pred$pred, log = "y", lty = c(1,3))

######################################################################

## Great Britain
gld5 <- subset(gold, ï..Country == "Great Britain")
uk2 <- subset(gld5[c(2,3)])

View(uk2)
uk2

uk2 <- ts_ts(ts_long(uk2)) #converting into time series

plot(uk2)
abline(reg=lm(uk2~time(uk2)))


cycle(uk2)

plot(aggregate(uk2, nfrequency = .25 ,FUN=mean))

boxplot(uk2~cycle(uk2))


adf.test(diff(log(uk2)), alternative="stationary", k=0)
acf(log(uk2))
acf(diff(log(uk2)))
pacf(diff(log(uk2)))

auto.arima(uk2)


fit18 <- arima(log(uk2), c(0, 1, 1),seasonal = list(order = c(0, 0, 0), period = 1))
pred <- predict(fit18, n.ahead = 2*1)
ts.plot(uk2,2.718^pred$pred, log = "y", lty = c(1,3))

######################################################################

## Japan
gld5 <- subset(gold, ï..Country == "Japan")
japan2 <- subset(gld5[c(2,3)])

japan2

japan2 <- ts_ts(ts_long(japan2)) #converting into time series

plot(japan2)
abline(reg=lm(japan2~time(japan2)))


cycle(japan2)

plot(aggregate(japan2, nfrequency = .25 ,FUN=mean))

boxplot(japan2~cycle(japan2))


adf.test(diff(log(japan2)), alternative="stationary", k=0)
acf(log(japan2))
acf(diff(log(japan2)))
pacf(diff(log(japan2)))

auto.arima(japan2)


fit19 <- arima(log(japan2), c(0, 1, 1),seasonal = list(order = c(0, 0, 0), period = 1))
pred <- predict(fit19, n.ahead = 2*1)
ts.plot(japan2,2.718^pred$pred, log = "y", lty = c(1,3))
