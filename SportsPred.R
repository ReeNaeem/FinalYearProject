Olympics <- read.csv("Olympics.csv",  header = TRUE, sep = ",")
str(Olympics)

Olympics$Sex <- as.character((Olympics$Sex))
Olympics$Sex[Olympics$Sex %in% "F"] <- "1"
Olympics$Sex[Olympics$Sex %in% "M"] <- "2"
Olympics$Sex <- as.numeric((Olympics$Sex))

library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(fitdistrplus)
library(nnet)


# making new subsets to see if a individual athletes atributes can help predict what medal will be won for specific sports

Athletics <- subset(Olympics, Sport == "Athletics")
str(Athletic1)
View(Athletics)

Athletics$Sex <- as.character((Athletics$Sex))
Athletics$Sex[Athletics$Sex %in% "F"] <- "1"
Athletics$Sex[Athletics$Sex %in% "M"] <- "2"
Athletics$Sex <- as.numeric((Athletics$Sex))
Athletics$Medal <- as.factor((Athletics$Medal))

Athletics$Sport <- as.character((Athletics$Sport))
Athletics$Sport[Athletics$Sport %in% "Athletics"] <- "1"
Athletics$Sport <- as.factor((Athletics$Sport))

Athletic1 <- subset(Athletics[c("ID", "Sex","Age","Height","Weight","Year","Sport","Medal")])
cor(Athletic1, method = c("pearson", "kendall", "spearman"))
Athletic1 <- subset(Athletics[c("ID", "Sex","Age","Height","Weight","Year","Season","NOC","Sport","Medal")])
Athletic1$Medal<- as.numeric(Athletic1$Medal)
Athletic1$Medal[Athletic1$Medal %in% 1] <- 0
Athletic1$Medal[Athletic1$Medal %in% 2] <- 1
Athletic1$Medal[Athletic1$Medal %in% 4] <- 2
Athletic1$Medal<- as.factor(Athletic1$Medal)


#################################################################################
# cretaing data split for athletics with all countries
set.seed(1234)
trainIndex <- createDataPartition(Athletic1$Medal, p = .8, list = FALSE)
train <- Athletic1[ trainIndex,]
test <- Athletic1[-trainIndex,]
View(train)
table(test$Medal)
str(test)

## Randomm Forest
set.seed(1234)
Athrf <- randomForest(Medal ~  Sex + Age + Height + Weight + Year, data = train)
print(Athrf)

predAth <- predict(Athrf, test)
table(predAth)
caret::confusionMatrix(test$Medal,predAth)


## multinomial logisitc
set.seed(1234)
Model1 <- lm( Medal ~ Sex + Age + Height + Weight + Year, data=train)

predAth<- predict(Model1,  test)
table(predAth)
View(predAth)

set.seed(1234)
Model2 <- multinom( Medal ~ Sex + Age + Height + Weight + Year, data=train)

predAth1<- predict(Model2,  test)
table(predAth1)
View(predAth1)
summary(Model2)

## Ordina; logistic
set.seed(1234)
ordAth <- polr(Medal ~ Sex + Age + Height + Weight + Year, data = train, Hess = TRUE)

predOrd <- predict(ordAth, test)
table(predOrd)
###########################

Athl1 <-  subset(Athletic1, NOC == "CHN")
Athl2 <-  subset(Athletic1, NOC == "FRA")
Athl3 <-  subset(Athletic1, NOC == "GBR")
Athl4 <-  subset(Athletic1, NOC == "USA")
Athl5 <-  subset(Athletic1, NOC == "RUS")
Athl6 <-  subset(Athletic1, NOC == "IRL")
Athl7 <-  subset(Athletic1, NOC == "JPN")
Athl8 <-  subset(Athletic1, NOC == "JAM")

AthlCo <- rbind(Athl1,Athl2,Athl3,Athl4,Athl5,Athl6,Athl7,Athl8)
View(AthlCo)
table(AthlCo$Medal)
write.csv(AthlCo, file = "AthlCo.csv")
##########################################################################################################
## BasketBall
Basketball <- subset(Olympics, Sport == "Basketball")
str(Basketball1)
View(Basketball1)

Basketball$Sport <- as.character((Basketball$Sport))
Basketball$Sport[Basketball$Sport %in% "Basketball"] <- "1"
Basketball$Sport <- as.factor((Basketball$Sport))

Basketball1 <- subset(Basketball[c("ID", "Sex","Age","Height","Weight","Year","Medal")])
cor(Basketball1, method = c("pearson", "kendall", "spearman"))
Basketball1 <- subset(Basketball[c("ID", "Sex","Age","Height","Weight","Year","Season","NOC","Sport","Medal")])

Basketball1$Medal<- as.factor(Basketball1$Medal)

#################################################################################
# cretaing data split for athletics with all countries
set.seed(1234)
trainBbal <- createDataPartition(Basketball1$Medal, p = .8, list = FALSE)
train2 <- Basketball1[ trainBbal,]
test2 <- Basketball1[-trainBbal,]
View(train2)
table(test2$Medal)
str(test2)

## Randomm Forest
set.seed(1234)
Ballrf <- randomForest(Medal ~   Sex + Age + Height + Weight + Year, data = train2)
print(Ballrf)

predball <- predict(Ballrf, test2)
table(predball)
caret::confusionMatrix(test2$Medal,predball)


Bball1 <-  subset(Basketball1, NOC == "CHN")
Bball2 <-  subset(Basketball1, NOC == "FRA")
Bball3 <-  subset(Basketball1, NOC == "ESP")
Bball4 <-  subset(Basketball1, NOC == "USA")
Bball5 <-  subset(Basketball1, NOC == "RUS")
Bball6 <-  subset(Basketball1, NOC == "BRA")
Bball7 <-  subset(Basketball1, NOC == "ITA")

BballCo <- rbind(Bball1,Bball2,Bball3,Bball4,Bball5,Bball6,Bball7)
View(BballCo)
table(BballCo$Medal)
write.csv(BballCo, file = "BballCo.csv")


