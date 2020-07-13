setwd("/Users/Rehan Naeem/Documents/College/Fourth Year/FINAL YEAR PROJECT!!!")


athlete_events <- read.csv("athlete_events.csv",  header = TRUE, sep = ",")
View(athlete_events)
summary(athlete_events)
structure(athlete_events)

library(moments)
#males
g.male<-subset(athlete_events,Sex=="M")
View(g.male)
summary(g.male)

summer.m <- subset(g.male, Season=="Summer")
View(summer.m)

summer90s.m <- subset(summer.m, Year > 1990)
View(summer90s.m)
head(summer90s.m$Sport, n=66)
levels(summer90s.m$Sport)
summary(summer90s.m)

#imputing means to replace NAs
age.m <- summer90s.m[c("Age")]
age.m[is.na(age.m)] <- 25
summary(age.m)

height.m <- summer90s.m[c("Height")]
height.m[is.na(height.m)] <- 181
summary(height.m)

weight.m <- summer90s.m[c("Weight")]
weight.m[is.na(weight.m)] <- 76
summary(weight.m)

#medal.m <

update.m <- cbind(age.m, height.m, weight.m)
males <- cbind(summer90s.m[c("ID","Name", "Sex") ], update.m, summer90s.m[c("Team" ,  "NOC"  ,  "Games"  ,"Year" ,  "Season", "City" ,  "Sport" , "Event" , "Medal" )])
View(males)

#############################################################

###############################################################
#females
g.female<-subset(athlete_events,Sex=="F")
View(g.female)
summer.f <- subset(g.female, Season=="Summer")
View(summer.f)

summer90s.f <- subset(summer.f, Year > 1990)
View(summer90s.f)
head(summer90s.f$Sport, n=66)
levels(summer90s.f$Sport)
summary(summer90s.f)
names(summer90s.f)

#imputing means to replace NAs
age.f <- summer90s.f[c("Age")]
age.f[is.na(age.f)] <- 24
summary(age.f)

height.f <- summer90s.f[c("Height")]
height.f[is.na(height.f)] <- 170
summary(height.f)

weight.f <- summer90s.f[c("Weight")]
weight.f[is.na(weight.f)] <- 60
summary(weight.f)

update.f <- cbind(age.f, height.f, weight.f)
females <- cbind(summer90s.f[c("ID","Name", "Sex") ], update.f, summer90s.f[c("Team" ,  "NOC"  ,  "Games"  ,"Year" ,  "Season", "City" ,  "Sport" , "Event" , "Medal" )])
View(females)
#############################################################
#merge back into main dataset
Olympics <- rbind(males, females)
View(Olympics)

medals <- Olympics[c("Medal")]
medals[is.na(medals)] <- 0
View(medals)
structure(medals)

write.csv(Olympics, file = "Olympics.csv")

#test data
test1 <- read.csv("Olympics.csv",  header = TRUE, sep = ",")
bronze <- subset(test1, Medal == 1)
silver <- subset(test1, Medal == 2)
gold <- subset(test1, Medal == 3)
View(silver)
testGames <- rbind(gold, silver, bronze)

test2 <- read.csv("summer.csv",  header = TRUE, sep = ",")
summary(test2)
USA2 <- subset(test2, Country == "USA")
USA3 <- subset(USA2, Year == 2012)
summary(USA3)

#USA
USA <- subset(testGames, Team =="United States")
USA16 <- subset(USA, Year == 2016)
USA12 <- subset(USA, Year == 2012)
summary(USA12)
USA08 <- subset(USA, Year == 2008)
write.csv(USA16, file = "USA16.csv")
write.csv(USA12, file = "USA12.csv")
write.csv(USA08, file = "USA08.csv")


View(USA)
#China
China <- subset(testGames, Team =="China")
CH16 <- subset(China, Year == 2016)
CH12 <- subset(China, Year == 2012)
CH08 <- subset(China, Year == 2008)
View(CH08)
#UK
UK <- subset(testGames, Team =="Great Britain")
UK16 <- subset(UK, Year == 2016)
UK12 <- subset(UK, Year == 2012)
UK08 <- subset(UK, Year == 2008)
View(UK16)
#Canada
Canada <- subset(testGames, Team =="Canada")
Canada16 <- subset(Canada, Year == 2016)
Canada12 <- subset(Canada, Year == 2012)
Canada08 <- subset(Canada, Year == 2008)
View(Canada16)
#Germany
Germ <- subset(testGames, Team =="United States")
Germ16 <- subset(Germ, Year == 2016)
Germ12 <- subset(Germ, Year == 2012)
Germ08 <- subset(Germ, Year == 2008)
View(Germ08)
#Jamaica
Jamaica <- subset(testGames, Team =="Jamaica")
Jamaica16 <- subset(Jamaica, Year == 2016)
Jamaica12 <- subset(Jamaica, Year == 2012)
Jamaica08 <- subset(Jamaica, Year == 2008)
View(Jamaica08)
#Nigeria
Nigeria <- subset(testGames, Team =="Nigeria")
Nigeria16 <- subset(Nigeria, Year == 2016)
Nigeria12 <- subset(Nigeria, Year == 2012)
Nigeria08 <- subset(Nigeria, Year == 2008)
View(Nigeria08)
#Afghanistan
Kenya <- subset(testGames, Team =="Kenya")
Kenya16 <- subset(Kenya, Year == 2016)
Kenya12 <- subset(Kenya, Year == 2012)
Kenya08 <- subset(Kenya, Year == 2008)
View(Kenya08)
#Haiti
Haiti <- subset(testGames, Team =="Haiti")
Haiti16 <- subset(Haiti, Year == 2016)
Haiti12 <- subset(Haiti, Year == 2012)
Haiti08 <- subset(Haiti, Year == 2008)
View(Haiti)
#Argentina
Argentina <- subset(testGames, Team =="Argentina")
Argentina16 <- subset(Argentina, Year == 2016)
Argentina12 <- subset(Argentina, Year == 2012)
Argentina08 <- subset(Argentina, Year == 2008)
View(Argentina08)



# check for NAs

 library(mice)
md.pattern(females)
md.pattern(males)

library(VIM)
aggr_plot <- aggr(females, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(females), 
                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

aggr_plot <- aggr(males, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(males), 
                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern males"))


####################
# GDP DATA CLEANING
GdpOG <- read.csv("DEMO_DS_29112019163028002.csv", header = TRUE, sep = ",")
GDP1 <- subset(GdpOG, Indicator == "GDP growth (annual %)")
GDP2 <- subset(GdpOG, Indicator == "GDP per capita (current US$)")
View(GDP3)
GDP3 <- rbind(GDP1, GDP2)

GDP2008 <- subset(GDP3, TIME == 2008)
GDP2012 <- subset(GDP3, TIME == 2012)
GDP2016 <- subset(GDP3, TIME == 2016)

GDPt1 <- rbind(GDP2016,GDP2012, GDP2008)
View(GDPt1)
write.csv(GDPt1, file = "GDP_test.csv")

#country selction 


####################
games[is.na(athlete_events$Height)] <- 0
olympics[is.na(athlete_events$Height)] <- 0





# Plots

boxplot(olympics)


# histo
hist(olympics$Height) #cm
hist(olympics$Age)
hist(olympics$Weight) #kg
hist(olympics$Year)



##############################################################
# I was gonna make a subset of all the sports and then get the mean values 
# Then impute them for the missing values. Then combine it back to form 1 dataset
# I intended  doing this seperatly for both males and females but i dont think its required as 
# i wont be usnig the figures for height, age and weight to predict the winners
# However this could come in usefull when talking about the improvement developing countries have had
aero.m <- subset(summer90s.m, Sport==	"Aeronautics")
View(aero.m)
skii.m <- subset(summer90s.m, Sport == "Alpine Skiing")
View(skii.m)
alpin.m <- subset(summer90s.m, Sport == "Alpinism")
archery.m <- subset(summer90s.m, Sport == "Archery")
art.m <- subset(summer90s.m, Sport == "Art Competitions")
athletics.m <- subset(summer90s.m, Sport == "Athletics")
badminton.m <- subset(summer90s.m, Sport == "Badminton")
baseball.m <- subset(summer90s.m, Sport == "Baseball")
basketball.m <- subset(summer90s.m, Sport == "Basketball")
basque.m <- subset(summer90s.m, Sport == "Basque Pelota")
beachv.m <- subset(summer90s.m, Sport == "Beach Volleyball")



