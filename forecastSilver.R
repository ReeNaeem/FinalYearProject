library(readr)
library(tsbox)
library(forecast)


silver <- read.csv("Silver.csv",  header = TRUE, sep = ",")
View(silver)
head(silver)
class(silver)
class(silver) = ts
## above i converted df into time series successfully ab line on slv1 doesn't appear to work for each country
## have to split each country for time series

## China
slv <- subset(silver, ï..Country == "China")
china1 <- subset(slv[c(2,3)])
china1

china1 <- ts_ts(ts_long(china1))

plot(china1)
abline(reg=lm(china1~time(china1)))


cycle(china1)
plot(aggregate(china1, nfrequency = .25 ,FUN=mean))

boxplot(china1~cycle(china1))


adf.test(diff(log(china1)), alternative="stationary", k=0)
acf(log(china1))
acf(diff(log(china1)))
pacf(diff(log(china1)))

auto.arima(china1)


fit6 <- arima(log(china1), c(0, 1, 1),seasonal = list(order = c(0, 0, 0), period = 1))
pred <- predict(fit6, n.ahead = 2*1)
ts.plot(china1,2.718^pred$pred, log = "y", lty = c(1,3))
#####################################################################

## USA
slv1 <- subset(silver, ï..Country == "USA")
usa1 <- subset(slv1[c(2,3)])


usa1 <- ts_ts(ts_long(usa1)) #converting into time series

plot(usa1)
abline(reg=lm(usa1~time(usa1)))


cycle(usa1)
plot(aggregate(usa1, nfrequency = .25 ,FUN=mean))

boxplot(usa1~cycle(usa1))


adf.test(diff(log(usa1)), alternative="stationary", k=0)
acf(log(usa1))
acf(diff(log(usa1)))
pacf(diff(log(usa1)))

auto.arima(usa1)


fit7 <- arima(log(usa1), c(0, 1, 1),seasonal = list(order = c(0, 0, 0), period = 1))
pred <- predict(fit7, n.ahead = 2*1)
ts.plot(usa1,2.718^pred$pred, log = "y", lty = c(1,3))
#####################################################################

## Russia
slv2 <- subset(silver, ï..Country == "Russia")
russia1 <- subset(slv2[c(2,3)])
russia1 <- subset(russia1, Year > 1992)

View(russia1)
russia1

russia1 <- ts_ts(ts_long(russia1)) #converting into time series

plot(russia1)
abline(reg=lm(russia~time(russia1)))


cycle(russia1)

plot(aggregate(russia1, nfrequency = .25 ,FUN=mean))

boxplot(russia1~cycle(russia1))


adf.test(diff(log(russia1)), alternative="stationary", k=0)
acf(log(russia1))
acf(diff(log(russia1)))
pacf(diff(log(russia1)))


auto.arima(russia1)


fit8 <- arima(log(russia1), c(0, 1, 1),seasonal = list(order = c(0, 0, 0), period = 2))
pred <- predict(fit8, n.ahead = 2*1)

ts.plot(russia1,2.718^pred$pred, log = "y", lty = c(1,3))

######################################################################

## Germany
slv3 <- subset(silver, ï..Country == "Germany")
germ1 <- subset(slv3[c(2,3)])

View(germ1)
germ1

germ1 <- ts_ts(ts_long(germ1)) #converting into time series

plot(germ1)
abline(reg=lm(germ1~time(germ1)))


cycle(germ1)

plot(aggregate(germ1, nfrequency = .25 ,FUN=mean))

boxplot(germ1~cycle(germ1))


adf.test(diff(log(germ1)), alternative="stationary", k=0)
acf(log(germ1))
acf(diff(log(germ1)))
pacf(diff(log(germ1)))

auto.arima(germ1)


fit9 <- arima(log(germ1), c(0, 1, 1),seasonal = list(order = c(0, 0, 0), period = 1))
pred <- predict(fit9, n.ahead = 2*1)
ts.plot(germ1,2.718^pred$pred, log = "y", lty = c(1,3))

######################################################################

## France
slv4 <- subset(silver, ï..Country == "France")
france1 <- subset(slv4[c(2,3)])

View(france1)
france1

france1 <- ts_ts(ts_long(france1)) #converting into time series

plot(france1)
abline(reg=lm(france1~time(france1)))


cycle(france1)

plot(aggregate(france1, nfrequency = .25 ,FUN=mean))

boxplot(france1~cycle(france1))


adf.test(diff(log(france1)), alternative="stationary", k=0)
acf(log(france1))
acf(diff(log(france1)))
pacf(diff(log(france1)))

auto.arima(france1)


fit10 <- arima(log(france1), c(0, 1, 1),seasonal = list(order = c(0, 0, 0), period = 1))
pred <- predict(fit10, n.ahead = 2*1)
ts.plot(france1,2.718^pred$pred, log = "y", lty = c(1,3))

######################################################################

## Great Britain
slv5 <- subset(silver, ï..Country == "Great Britain")
uk1 <- subset(slv5[c(2,3)])

View(uk1)
uk1

uk1 <- ts_ts(ts_long(uk1)) #converting into time series

plot(uk1)
abline(reg=lm(uk1~time(uk1)))


cycle(uk1)

plot(aggregate(uk1, nfrequency = .25 ,FUN=mean))

boxplot(uk1~cycle(uk1))


adf.test(diff(log(uk1)), alternative="stationary", k=0)
acf(log(uk1))
acf(diff(log(uk1)))
pacf(diff(log(uk1)))

auto.arima(uk1)


fit11 <- arima(log(uk1), c(0, 1, 1),seasonal = list(order = c(0, 0, 0), period = 1))
pred <- predict(fit11, n.ahead = 2*1)
ts.plot(uk1,2.718^pred$pred, log = "y", lty = c(1,3))

######################################################################

## Japan
slv5 <- subset(silver, ï..Country == "Japan")
japan1 <- subset(slv5[c(2,3)])

japan1

japan1 <- ts_ts(ts_long(japan1)) #converting into time series

plot(japan1)
abline(reg=lm(japan1~time(japan1)))


cycle(japan1)

plot(aggregate(japan1, nfrequency = .25 ,FUN=mean))

boxplot(japan1~cycle(japan1))


adf.test(diff(log(japan1)), alternative="stationary", k=0)
acf(log(japan1))
acf(diff(log(japan1)))
pacf(diff(log(japan1)))

auto.arima(japan1)


fit12 <- arima(log(japan1), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 1))
pred <- predict(fit12, n.ahead = 2*1)
ts.plot(japan1,2.718^pred$pred, log = "y", lty = c(1,3))
