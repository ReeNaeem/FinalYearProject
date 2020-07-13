athlete_events <- read.csv("athlete_events.csv",  header = TRUE, sep = ",")
View(athlete_events)
summary(athlete_events)
str(athlete_events)

library(moments)
library(tidyverse)
summer<- subset(athlete_events, Season=="Summer")
View(summer)
summer1 <- subset(summer[c("Team", "NOC", "Year", "Medal")])
View(summer1)
summary(summer1)
write.csv(summer1, file ="CountryMedals.csv")
Country <- read.csv("CountryMedals.csv",  header = TRUE, sep = ",")
View(Country)
str(Country)
 summary(Country)
 
 Country$Medals1 <- as.numeric(Country$Medals1)
 Country$Medals1[Country$Medals1 %in% 1] <- 0
 Country$Medals1[Country$Medals1 %in% 2] <- 1
 Country$Medals1[Country$Medals1 %in% 4] <- 2


## with this dataset i wil be looking at which developing country has improved its performance
## i can later merge GDP data to see if economic status has an effect on performance 
library(ggplot2)
hist(Country$Medals1)
boxplot(df$Medals1, df$NOC)

qplot(NOC, Medals1, data=df, color="red",  geom=c("boxplot", "jitter"))

df1 <- subset(Country, NOC == "NGR")
qplot(Year, Medals1, data=df1, color="red",  geom=c("boxplot", "jitter"))

df2 <- subset(Country, NOC == "JAM")
qplot(Year, Medals1, data=df2, color="red",  geom=c("boxplot", "jitter"))

df3 <- subset(Country, NOC == "ZIM")
qplot(Year, Medals1, data=df3, color="red",  geom=c("boxplot", "jitter"))

df4 <- subset(Country, NOC == "UGA")
qplot(Year, Medals1, data=df4, color="red",  geom=c("boxplot", "jitter"))

df5 <- subset(Country, NOC == "PAK")
qplot(Year, Medals1, data=df5, color="red",  geom=c("boxplot", "jitter"))

df6 <- subset(Country, NOC == "BRA")
qplot(Year, Medals1, data=df6, color="red",  geom=c("boxplot", "jitter"))

df7 <- subset(Country, NOC == "PHI")
qplot(Year, Medals1, data=df7, color="red",  geom=c("boxplot", "jitter"))

df8 <- subset(Country, NOC == "IRI")
qplot(Year, Medals1, data=df8, color="red",  geom=c("boxplot", "jitter"))

df9 <- subset(Country, NOC == "CHN")
qplot(Year, Medals1, data=df9, color="red",  geom=c("boxplot", "jitter"))

df10 <- subset(Country, NOC == "IRQ")
qplot(Year, Medals1, data=df10, color="red",  geom=c("boxplot", "jitter"))

df11 <- subset(Country, NOC == "ARG")
qplot(Year, Medals1, data=df11, color="red",  geom=c("boxplot", "jitter"))


df <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11)
view(df)


ggplot(data=df, aes(x=NOC, y=Year)) +
  geom_line(color="red")+
  geom_point()

p<-ggplot(df1, aes(x=Year, y=Medals1, group=Medal)) +
  geom_line(aes(color=Medal))+
  geom_point(aes(color=Medal))
p

plot(df$NOC, df$Year, pch = 16, col = c('cornflowerblue', 'springgreen')[df$Medals1])

