library(readr)
library(tsbox)
library(forecast)


bronze <- read.csv("Bronze.csv",  header = TRUE, sep = ",")
View(bro1)
class(bronze)
class(bronze) = ts
## above i converted df into time series successfully ab line on bro doesn't appear to work for each country
## have to split each country for time series

## China
bro <- subset(bronze, Country == "China")
china <- subset(bro[c(2,3)])

china <- ts_ts(ts_long(china))

plot(china)
abline(reg=lm(china~time(china)))


cycle(china)
plot(aggregate(china, nfrequency = .25 ,FUN=mean))

boxplot(china~cycle(china))


adf.test(diff(log(china)), alternative="stationary", k=0)
acf(log(china))
acf(diff(log(china)))
pacf(diff(log(china)))

auto.arima(china)


fit <- arima(log(china), c(0, 1, 1),seasonal = list(order = c(0, 0, 0), period = 4))
pred <- predict(fit, n.ahead = 2*1)
ts.plot(china,2.718^pred$pred, log = "y", lty = c(1,3))
#####################################################################

## USA
bro1 <- subset(bronze, Country == "USA")
usa <- subset(bro1[c(2,3)])


usa <- ts_ts(ts_long(usa)) #converting into time series

plot(usa)
abline(reg=lm(usa~time(usa)))


cycle(usa)
plot(aggregate(usa, nfrequency = .25 ,FUN=mean))

boxplot(usa~cycle(usa))


adf.test(diff(log(usa)), alternative="stationary", k=0)
acf(log(usa))
acf(diff(log(usa)))
pacf(diff(log(usa)))

auto.arima(usa)


fit1 <- arima(log(usa), c(0, 1, 1),seasonal = list(order = c(0, 0, 0), period = 1))
pred <- predict(fit1, n.ahead = 2*1)
ts.plot(usa,2.718^pred$pred, log = "y", lty = c(1,3))
#####################################################################

## Russia
bro2 <- subset(bronze, Country == "Russia")
russia <- subset(bro2[c(2,3)])
russia <- subset(russia, Year > 1992)

View(russia)
russia

russia <- ts_ts(ts_long(russia)) #converting into time series

plot(russia)
abline(reg=lm(russia~time(russia)))


cycle(russia)

plot(aggregate(russia, nfrequency = .25 ,FUN=mean))

boxplot(russia~cycle(russia))


adf.test(diff(log(russia)), alternative="stationary", k=0)
acf(log(russia))
acf(diff(log(russia)))
pacf(diff(log(russia)))

auto.arima(russia)


fit2 <- arima(log(russia), c(0, 1, 1),seasonal = list(order = c(0, 0, 0), period = 2))
pred <- predict(fit2, n.ahead = 2*1)
ts.plot(russia,2.718^pred$pred, log = "y", lty = c(1,3))

######################################################################

## Germany
bro3 <- subset(bronze, Country == "Germany")
germ <- subset(bro3[c(2,3)])

View(germ)
germ

germ <- ts_ts(ts_long(germ)) #converting into time series

plot(germ)
abline(reg=lm(germ~time(germ)))


cycle(germ)

plot(aggregate(germ, nfrequency = .25 ,FUN=mean))

boxplot(germ~cycle(germ))


adf.test(diff(log(germ)), alternative="stationary", k=0)
acf(log(germ))
acf(diff(log(germ)))
pacf(diff(log(germ)))

auto.arima(germ)


fit3 <- arima(log(germ), c(0, 1, 1),seasonal = list(order = c(0, 0, 0), period = 1))
pred <- predict(fit3, n.ahead = 2*1)
ts.plot(germ,2.718^pred$pred, log = "y", lty = c(1,3))

######################################################################

## France
bro4 <- subset(bronze, Country == "France")
france <- subset(bro4[c(2,3)])

View(france)
france

france <- ts_ts(ts_long(france)) #converting into time series

plot(france)
abline(reg=lm(france~time(france)))


cycle(france)

plot(aggregate(france, nfrequency = .25 ,FUN=mean))

boxplot(france~cycle(france))


adf.test(diff(log(france)), alternative="stationary", k=0)
acf(log(france))
acf(diff(log(france)))
pacf(diff(log(france)))

auto.arima(france)


fit3 <- arima(log(france), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 1))
pred <- predict(fit3, n.ahead = 2*1)
ts.plot(france,2.718^pred$pred, log = "y", lty = c(1,3))

######################################################################

## Great Britain
bro5 <- subset(bronze, Country == "Great Britain")
uk <- subset(bro5[c(2,3)])

View(uk)
uk

uk <- ts_ts(ts_long(uk)) #converting into time series

plot(uk)
abline(reg=lm(uk~time(uk)))


cycle(uk)

plot(aggregate(uk, nfrequency = .25 ,FUN=mean))

boxplot(uk~cycle(uk))


adf.test(diff(log(uk)), alternative="stationary", k=0)
acf(log(uk))
acf(diff(log(uk)))
pacf(diff(log(uk)))

auto.arima(uk)


fit4 <- arima(log(uk), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 3))
pred <- predict(fit4, n.ahead = 2*1)
ts.plot(uk,2.718^pred$pred, log = "y", lty = c(1,3))

######################################################################

## Japan
bro5 <- subset(bronze, Country == "Japan")
japan <- subset(bro5[c(2,3)])

View(japan)
japan

japan <- ts_ts(ts_long(japan)) #converting into time series

plot(japan)
abline(reg=lm(japan~time(japan)))


cycle(japan)

plot(aggregate(japan, nfrequency = .25 ,FUN=mean))

boxplot(japan~cycle(japan))


adf.test(diff(log(japan)), alternative="stationary", k=0)
acf(log(japan))
acf(diff(log(japan)))
pacf(diff(log(japan)))

auto.arima(japan)


fit5 <- arima(log(japan), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 1))
pred <- predict(fit5, n.ahead = 2*1)
ts.plot(japan,2.718^pred$pred, log = "y", lty = c(1,3))
        