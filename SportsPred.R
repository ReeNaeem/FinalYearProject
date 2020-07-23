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

# need to select specific countries
Athlwin1 <- subset(Athletic1, Medal == "1")
Athlwin2 <- subset(Athletic1, Medal == "2")
Athlwin3 <- subset(Athletic1, Medal == "3")
Athlwin <- rbind(Athlwin1,Athlwin2,Athlwin3)

#visualize to see what countries to choose excluding losers
ggplot(Athlwin, aes(NOC, fill = Medal)) + geom_bar()+
  labs(title = "Stacked Bar Chart", x = "NOC", y = "Count of Medals")

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

############################################################################

## diving
Diving <- subset(Olympics, Sport == "Diving")
str(Diving1)
View(Diving1)

Diving$Sport <- as.character((Diving$Sport))
Diving$Sport[Diving$Sport %in% "Diving"] <- "1"
Diving$Sport <- as.factor((Diving$Sport))

Diving1 <- subset(Diving[c("ID", "Sex","Age","Height","Weight","Year","Medal")])
cor(Diving1, method = c("pearson", "kendall", "spearman"))
Diving1 <- subset(Diving[c("ID", "Sex","Age","Height","Weight","Year","Season","Sport","Medal")])

Diving1$Medal<- as.factor(Diving1$Medal)
#################################################################################
# cretaing data split for DIVING with all countries
## this model proved successful without having to filter specific countries
set.seed(1234)
trainD <- createDataPartition(Diving1$Medal, p = .8, list = FALSE)
train3 <- Diving1[ trainD,]
test3 <- Diving1[-trainD,]
View(train3)
table(test3$Medal)
str(test3)

## Randomm Forest
set.seed(1234)
Diverf <- randomForest(Medal ~   , data = train3)
print(Diverf)

preddive <- predict(Diverf, test3)
table(preddive)
caret::confusionMatrix(test3$Medal,preddive)

# no.2
set.seed(1234)
Diverf1 <- randomForest(Medal ~   Sex + Age + Height + Weight + Year, data = train3)
print(Diverf)

preddive1 <- predict(Diverf1, test3)
table(preddive1)
caret::confusionMatrix(test3$Medal,preddive1)

#######################################################################
## Boxing
Boxing <- subset(Olympics, Sport == "Boxing")
str(Boxing1)
View(Boxing1)

Boxing$Sport <- as.character((Boxing$Sport))
Boxing$Sport[Boxing$Sport %in% "Boxing"] <- "1"
Boxing$Sport <- as.factor((Boxing$Sport))

Boxing1 <- subset(Boxing[c("ID", "Sex","Age","Height","Weight","Year","Medal")])
cor(Boxing1, method = c("pearson", "kendall", "spearman"))
Boxing1 <- subset(Boxing[c("ID", "Sex","Age","Height","Weight","Year","Season","NOC","Sport","Medal")])

Boxing1$Medal<- as.factor(Boxing1$Medal)
################################################################
# cretaing data split for Boxing with all countries
set.seed(1234)
trainBox <- createDataPartition(Boxing1$Medal, p = .8, list = FALSE)
train4 <- Boxing1[ trainBox,]
test4 <- Boxing1[-trainBox,]
View(train4)
table(test4$Medal)
str(test4)

## Randomm Forest
set.seed(1234)
Boxrf <- randomForest(Medal ~  . , data = train4)
print(Boxrf)

predbox <- predict(Boxrf, test4)
table(predbox)
caret::confusionMatrix(test4$Medal,predbox)

# no.2
set.seed(1234)
Boxrf1 <- randomForest(Medal ~   Sex + Age + Height + Weight + Year, data = train4)
print(Boxrf)

predbox1 <- predict(Boxrf1, test4)
table(predbox1)
caret::confusionMatrix(test4$Medal,predbox1)

# need to select specific countries
plot(Boxing1$NOC,Boxing1$Medal)

Box1 <-  subset(Boxing1, NOC == "CHN")
Box2 <-  subset(Boxing1, NOC == "FRA")
Box3 <-  subset(Boxing1, NOC == "ESP")
Box4 <-  subset(Boxing1, NOC == "USA")
Box5 <-  subset(Boxing1, NOC == "RUS")
Box6 <-  subset(Boxing1, NOC == "BRA")
Box7 <-  subset(Boxing1, NOC == "ITA")
Box8 <-  subset(Boxing1, NOC == "IRL")
Box9 <-  subset(Boxing1, NOC == "GBR")
Box10 <-  subset(Boxing1, NOC == "KAZ")
Box11 <-  subset(Boxing1, NOC == "JPA")
Box12 <-  subset(Boxing1, NOC == "CAN")
Box13 <-  subset(Boxing1, NOC == "UZB")



BoxCo <- rbind(Box1,Box2,Box3,Box4,Box5,Box6,Box7,Box8,Box9,Box10,Box11,Box12,Box13)
View(BoxCo)
table(BoxCo$Medal)
write.csv(BoxCo, file = "BoxCo.csv")

##############################################
#
#######################################################################
## Gymnastics
Gym <- subset(Olympics, Sport == "Gymnastics")
str(Gym)
View(Gym1)

Gym$Sport <- as.character((Gym$Sport))
Gym$Sport[Gym$Sport %in% "Gymnastics"] <- "1"
Gym$Sport <- as.factor((Gym$Sport))

Gym1 <- subset(Gym[c("ID", "Sex","Age","Height","Weight","Year","Medal")])
cor(Gym1, method = c("pearson", "kendall", "spearman"))
Gym1 <- subset(Gym[c("ID", "Sex","Age","Height","Weight","Year","Season","NOC","Sport","Medal")])

Gym1$Medal<- as.factor(Gym1$Medal)

################################################################
# cretaing data split for Gymnastics with all countries
set.seed(1234)
traingym <- createDataPartition(Gym1$Medal, p = .8, list = FALSE)
train5 <- Gym1[ traingym,]
test5 <- Gym1[-traingym,]
View(train5)
table(test5$Medal)
str(test5)

## Randomm Forest
set.seed(1234)
gymrf <- randomForest(Medal ~  Sex + Age + Height + Weight + Year , data = train5)
print(gymrf)

predgym <- predict(gymrf, test5)
table(predgym)
caret::confusionMatrix(test5$Medal,predgym)

# need to select specific countries
gymwin1 <- subset(Gym1, Medal == "1")
gymwin2 <- subset(Gym1, Medal == "2")
gymwin3 <- subset(Gym1, Medal == "3")
gymwin <- rbind(gymwin1,gymwin2,gymwin3)

#visualize to see what countries to choose excluding losers
ggplot(gymwin, aes(NOC, fill = Medal)) + geom_bar()+
  labs(title = "Stacked Bar Chart", x = "NOC", y = "Count of Medals")

G1 <-  subset(Gym1, NOC == "CHN")
G2 <-  subset(Gym1, NOC == "FRA")
G3 <-  subset(Gym1, NOC == "EUN")
G4 <-  subset(Gym1, NOC == "USA")
G5 <-  subset(Gym1, NOC == "RUS")
G6 <-  subset(Gym1, NOC == "BRA")
G7 <-  subset(Gym1, NOC == "KOR")
G8 <-  subset(Gym1, NOC == "GER")
G9 <-  subset(Gym1, NOC == "GBR")
G10 <-  subset(Gym1, NOC == "ROU")
G11 <-  subset(Gym1, NOC == "UKR")



GCo <- rbind(G1,G2,G3,G4,G5,G6,G7,G8,G9,G10,G11)
View(GCo)
table(GCo$Medal)
write.csv(GCo, file = "GCo.csv")

#######################################################################
## Swimming
Swim <- subset(Olympics, Sport == "Swimming")
str(Swim1)
View(Swim1)

Swim$Sport <- as.character((Swim$Sport))
Swim$Sport[Swim$Sport %in% "Swimming"] <- "1"
Swim$Sport <- as.factor((Swim$Sport))

Swim1 <- subset(Swim[c("ID", "Sex","Age","Height","Weight","Year","Medal")])
cor(Swim1, method = c("pearson", "kendall", "spearman"))
Swim1 <- subset(Swim[c("ID", "Sex","Age","Height","Weight","Year","Season","NOC","Sport","Medal")])

Swim1$Medal<- as.factor(Swim1$Medal)

################################################################
# cretaing data split for Swimming with all countries
set.seed(1234)
trainS1 <- createDataPartition(Swim1$Medal, p = .8, list = FALSE)
train6 <- Swim1[ trainS1,]
test6 <- Swim1[-trainS1,]
View(train6)
table(test6$Medal)
str(test6)

## Randomm Forest
set.seed(1234)
Swimrf <- randomForest(Medal ~  Sex + Age + Height + Weight + Year , data = train6)
print(Swimrf)

predSwim <- predict(Swimrf, test6)
table(predSwim)
caret::confusionMatrix(test6$Medal,predSwim)

## Randomm Forest
set.seed(1234)
Swimrf1 <- randomForest(Medal ~  . , data = train6)
print(Swimrf)

predSwim1 <- predict(Swimrf1, test6)
table(predSwim1)
caret::confusionMatrix(test6$Medal,predSwim1)


# need to select specific countries
swimwin1 <- subset(Swim1, Medal == "1")
swimwin2 <- subset(Swim1, Medal == "2")
swimwin3 <- subset(Swim1, Medal == "3")
swimwin <- rbind(swimwin1,swimwin2,swimwin3)

#visualize to see what countries to choose excluding losers
ggplot(swimwin, aes(NOC, fill = Medal)) + geom_bar()+
  labs(title = "Stacked Bar Chart", x = "NOC", y = "Count of Medals")

S1 <-  subset(Swim1, NOC == "CHN")
S2 <-  subset(Swim1, NOC == "FRA")
S3 <-  subset(Swim1, NOC == "EUN")
S4 <-  subset(Swim1, NOC == "USA")
S5 <-  subset(Swim1, NOC == "RUS")
S6 <-  subset(Swim1, NOC == "GBR")
S7 <-  subset(Swim1, NOC == "GER")
S8 <-  subset(Swim1, NOC == "NED")
S9 <-  subset(Swim1, NOC == "CAN")
S10 <-  subset(Swim1, NOC == "AUS")
S11 <-  subset(Swim1, NOC == "JPN")
S12 <-  subset(Swim1, NOC == "HUN")




SCo <- rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12)
View(SCo)
table(SCo$Medal)
write.csv(SCo, file = "SCo.csv")

#######################################################################
## Football
Foot <- subset(Olympics, Sport == "Football")
str(Foot1)
View(Foot1)

Foot$Sport <- as.character((Foot$Sport))
Foot$Sport[Foot$Sport %in% "Football"] <- "1"
Foot$Sport <- as.factor((Foot$Sport))

Foot1 <- subset(Foot[c("ID", "Sex","Age","Height","Weight","Year","Medal")])
cor(Foot1, method = c("pearson", "kendall", "spearman"))
Foot$Medal<- as.factor(Foot$Medal)

################################################################
# cretaing data split for footming with all countries
set.seed(1234)
trainF1 <- createDataPartition(Foot$Medal, p = .8, list = FALSE)
train7 <- Foot[ trainF1,]
test7 <- Foot[-trainF1,]
View(train7)
table(test7$Medal)
str(test7)

## Randomm Forest
set.seed(1234)
footrf <- randomForest(Medal ~  Sex + Age + Height + Weight + Year , data = train7)
print(footrf)

predfoot <- predict(footrf, test7)
table(predfoot)
caret::confusionMatrix(test7$Medal,predfoot)

## Randomm Forest
set.seed(1234)
footrf1 <- randomForest(Medal ~  . , data = train7)

# need to select specific countries
footwin1 <- subset(Foot, Medal == "1")
footwin2 <- subset(Foot, Medal == "2")
footwin3 <- subset(Foot, Medal == "3")
footwin <- rbind(footwin1,footwin2,footwin3)

#visualize to see what countries to choose excluding losers
ggplot(footwin, aes(NOC, fill = Medal)) + geom_bar()+
  labs(title = "Stacked Bar Chart", x = "NOC", y = "Count of Medals")

F1 <-  subset(Foot, NOC == "ARG")
F2 <-  subset(Foot, NOC == "BRA")
F3 <-  subset(Foot, NOC == "CAN")
F4 <-  subset(Foot, NOC == "CHI")
F5 <-  subset(Foot, NOC == "CHN")
F6 <-  subset(Foot, NOC == "CMR")
F7 <-  subset(Foot, NOC == "ESP")
F8 <-  subset(Foot, NOC == "GER")
F9 <-  subset(Foot, NOC == "GHA")
F10 <-  subset(Foot, NOC == "ITA")
F11 <-  subset(Foot, NOC == "JPN")
F12 <-  subset(Foot, NOC == "KOR")
F13 <-  subset(Foot, NOC == "MEX")
F14 <-  subset(Foot, NOC == "NGR")
F15 <-  subset(Foot, NOC == "NOR")
F16 <-  subset(Foot, NOC == "PAR")
F17 <-  subset(Foot, NOC == "POL")
F18 <-  subset(Foot, NOC == "SWE")
F19 <-  subset(Foot, NOC == "USA")


FCo <- rbind(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19)
View(FCo)
table(FCo$Medal)
write.csv(FCo, file = "FCo.csv")

#######################################################################
## Weightlifting
weight <- subset(Olympics, Sport == "Weightlifting")
str(weight)
View(weight1)

weight$Sport <- as.character((weight$Sport))
weight$Sport[weight$Sport %in% "Weightlifting"] <- "1"
weight$Sport <- as.factor((weight$Sport))

weight1 <- subset(weight[c("ID", "Sex","Age","Height","Weight","Year","Medal")])
cor(weight1, method = c("pearson", "kendall", "spearman"))
weight$Medal<- as.factor(weight$Medal)
weight2 <- subset(weight[c("ID","Sex", "Age","Height","Weight","Year","NOC","Medal")])



################################################################
# cretaing data split for weightming with all countries
set.seed(1234)
trainW1 <- createDataPartition(weight2$Medal, p = .8, list = FALSE)
train8 <- weight2[ trainW1,]
test8 <- weight2[-trainW1,]
View(train8)
table(test8$Medal)
str(test8)

## Randomm Forest
set.seed(1234)
weightrf <- randomForest(Medal ~  Sex + Age + Height + Weight + Year , data = train8)
print(weightrf)

predweight <- predict(weightrf, test8)
table(predweight)
caret::confusionMatrix(test8$Medal,predweight)

## Randomm Forest
set.seed(1234)
weightrf1 <- randomForest(Medal ~  . , data = train8)

# need to select specific countries
weightwin1 <- subset(weight, Medal == "1")
weightwin2 <- subset(weight, Medal == "2")
weightwin3 <- subset(weight, Medal == "3")
weightwin <- rbind(weightwin1,weightwin2,weightwin3)

#visualize to see what countries to choose excluding losers
ggplot(weightwin, aes(NOC, fill = Medal)) + geom_bar()+
  labs(title = "Stacked Bar Chart", x = "NOC", y = "Count of Medals")

W1 <-  subset(weight2, NOC == "BLR")
W2 <-  subset(weight2, NOC == "BUL")
W3 <-  subset(weight2, NOC == "CHN")
W4 <-  subset(weight2, NOC == "GER")
W5 <-  subset(weight2, NOC == "EUN")
W6 <-  subset(weight2, NOC == "GRE")
W7 <-  subset(weight2, NOC == "KAZ")
W8 <-  subset(weight2, NOC == "PRK")
W9 <-  subset(weight2, NOC == "RUS")
W10 <-  subset(weight2, NOC == "THA")
W11 <-  subset(weight2, NOC == "TUR")
W12 <-  subset(weight2, NOC == "UKR")
W13 <-  subset(weight2, NOC == "USA")
W14 <-  subset(weight2, NOC == "IRI")
W15 <-  subset(weight2, NOC == "POL")



WCo <- rbind(W1,W2,W3,W4,W5,W6,W7,W8,W9,W10,W11,W12,W13,W14,W15)
View(WCo)
table(WCo$Medal)
write.csv(WCo, file = "WCo.csv")
