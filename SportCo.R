data1 <- read.csv("AthlCo.csv",  header = TRUE, sep = ",")
str(data1)
df <- subset(data1[c("ID","Sex", "Age","Height","Weight","Year","Medal")])

cor(df, method = c("pearson", "kendall", "spearman"))

data1$Medal<- as.numeric(data1$Medal)
data1$Medal[data1$Medal %in% 1] <- 0
data1$Medal[data1$Medal %in% 2] <- 1
data1$Medal[data1$Medal %in% 3] <- 2
data1$Medal[data1$Medal %in% 4] <- 3

data1$Medal<- as.factor(data1$Medal)


# cretaing data split for athletics with selected countries
set.seed(1234)
trainIndex1 <- createDataPartition(data1$Medal, p = .8, list = FALSE)
trainA <- data1[ trainIndex1,]
testA<- data1[-trainIndex1,]
View(trainA)
table(testA$Medal)
str(testA)

## Random For
set.seed(1234)
rf1 <- randomForest(Medal ~ ., data = trainA)
print(Athrf)

predrf1 <- predict(rf1, testA)
table(predrf1)
caret::confusionMatrix(testA$Medal,predrf1)

#### multinomial logisitc
set.seed(1234)
M1 <- multinom( Medal ~ Sex + Age + Height + Weight + Year, data=trainA)

ACo<- predict(M1,  testA)
table(ACo)
View(ACo)
summary(ACo)

## Ordina; logistic
set.seed(1234)
O1 <- polr(Medal ~ Sex + Age + Height + Weight + Year, data = trainA, Hess = TRUE)

ACo1 <- predict(O1, testA)
table(ACo1)
##############################################
#
## Basketball
data2 <- read.csv("BballCo.csv",  header = TRUE, sep = ",")
View(data2)
table(data2$Medal)
str(data2)

df1 <- subset(data2[c("ID","Sex", "Age","Height","Weight","Year","Medal")])
cor(df1, method = c("pearson", "kendall", "spearman"))
data2$Medal<- as.factor(data2$Medal)

########
# cretaing data split for basketball with selected countries
set.seed(1234)
trainIndex2 <- createDataPartition(data2$Medal, p = .8, list = FALSE)
trainBB <- data2[ trainIndex2,]
testBB<- data2[-trainIndex2,]
View(trainBB)
table(testBB$Medal)
str(testBB)

## Random For
set.seed(1234)
rf2 <- randomForest(Medal ~ ., data = trainBB)
print(Athrf)

predrf2 <- predict(rf2, testBB)
table(predrf2)
caret::confusionMatrix(testBB$Medal,predrf2)
compare <- cbind(testBB, predrf2)
View(compare)

#### multinomial logisitc
set.seed(1234)
M2 <- multinom( Medal ~ Sex + Age + Height + Weight + Year, data=trainBB)

BBCo<- predict(M2,  testBB)
table(BBCo)
View(BBCo)
summary(BBCo)

####################################################################################
## Boxing
data3 <- read.csv("BoxCo.csv",  header = TRUE, sep = ",")
View(data3)
table(data3$Medal)
str(data3)

df2 <- subset(data3[c("ID","Sex", "Age","Height","Weight","Year","Medal")])
cor(df2, method = c("pearson", "kendall", "spearman"))
data3$Medal<- as.factor(data3$Medal)
df2 <- subset(data3[c("ID","Sex", "Age","Height","Weight","Year","NOC","Medal")])

########
# cretaing data split for basketball with selected countries
set.seed(1334)
trainIndex3 <- createDataPartition(data3$Medal, p = .8, list = FALSE)
trainbox <- data3[ trainIndex3,]
testbox<- data3[-trainIndex3,]
View(trainbox)
table(testbox$Medal)
str(testbox)

## Random For
set.seed(1234)
rf3 <- randomForest(Medal ~ ., data = trainbox)
print(Athrf)

predrf3 <- predict(rf3, testbox)
table(predrf3)
caret::confusionMatrix(testbox$Medal,predrf3)
compare1 <- cbind(testbox, predrf3)
View(compare)

set.seed(1234)
rf4 <- randomForest(Medal ~  Sex + Age + Height + Weight + Year, data = trainbox)
print(Athrf)

predrf4 <- predict(rf4, testbox)
table(predrf4)
caret::confusionMatrix(testbox$Medal,predrf4)
compare <- cbind(testbox, predrf4)
View(compare)

####################################################################################
## Gymnastics
data4 <- read.csv("GCo.csv",  header = TRUE, sep = ",")
str(data4)
table(data4$Medal)

df3 <- subset(data4[c("ID","Sex", "Age","Height","Weight","Year","Medal")])
cor(df3, method = c("pearson", "kendall", "spearman"))
data4$Medal<- as.factor(data4$Medal)

########
# cretaing data split for gymnastics with selected countries
set.seed(1234)
trainindex4 <- createDataPartition(data4$Medal, p = .8, list = FALSE)
traingym <- data4[ trainindex4,]
testgym<- data4[-trainindex4,]
View(traingym)
table(testgym$Medal)
str(testgym)

## Random For
set.seed(1234)
rf5 <- randomForest(Medal ~ ., data = traingym)
print(Athrf)

predrf5 <- predict(rf5, testgym)
table(predrf5)
caret::confusionMatrix(testgym$Medal,predrf5)
compare2 <- cbind(testgym, predrf5)
View(compare)

rf6 <- randomForest(Medal ~ Sex + Age + Height + Weight + Year, data = traingym)
print(Athrf)

predrf6 <- predict(rf6, testgym)
table(predrf6)
caret::confusionMatrix(testgym$Medal,predrf6)

####################################################################################
## sWIMMING
data5 <- read.csv("SCo.csv",  header = TRUE, sep = ",")
str(data5)
table(data5$Medal)

df4 <- subset(data5[c("ID","Sex", "Age","Height","Weight","Year","Medal")])
cor(df4, method = c("pearson", "kendall", "spearman"))
data5$Medal<- as.factor(data5$Medal)

########
# cretaing data split for gymnastics with selected countries
set.seed(1234)
trainindex5 <- createDataPartition(data5$Medal, p = .8, list = FALSE)
trainswim <- data5[ trainindex5,]
testswim<- data5[-trainindex5,]
View(trainswim)
table(testswim$Medal)
table(testswim$NOC)

str(testswim)

## Random For
set.seed(1234)
rf7 <- randomForest(Medal ~ ., data = trainswim)
print(rf7)

predrf7 <- predict(rf7, testswim)
table(predrf7)
caret::confusionMatrix(testswim$Medal,predrf7)
compare2 <- cbind(testswim, predrf7)
View(compare)

# rf2
set.seed(1234)
rf8 <- randomForest(Medal ~ Sex + Age + Height + Weight + Year, data = trainswim)
print(rf8)

predrf8 <- predict(rf8, testswim)
table(predrf8)
caret::confusionMatrix(testswim$Medal,predrf8)


####################################################################################
## Football
data6 <- read.csv("FCo.csv",  header = TRUE, sep = ",")
str(data6)
table(data6$Medal)

df5 <- subset(data6[c("ID","Sex", "Age","Height","Weight","Year","Medal")])
cor(df5, method = c("pearson", "kendall", "spearman"))
data6$Medal<- as.factor(data6$Medal)
df6 <- subset(data6[c("ID","Sex", "Age","Height","Weight","Year","NOC","Event","Medal")])


########
# cretaing data split for gymnastics with selected countries
set.seed(1234)
trainindex6 <- createDataPartition(df6$Medal, p = .8, list = FALSE)
trainfoot <- df6[ trainindex6,]
testfoot<- df6[-trainindex6,]
View(trainfoot)
table(testfoot$Medal)
table(testfoot$NOC)

str(testfoot)

## Random For
set.seed(1234)
rf9 <- randomForest(Medal ~ ., data = trainfoot)
print(rf9)

predrf9 <- predict(rf9, testfoot)
table(predrf9)
caret::confusionMatrix(testfoot$Medal,predrf9)
compare2 <- cbind(testfoot, predrf9)
View(compare)

## Random For
set.seed(1234)
rf10 <- randomForest(Medal ~ Sex + Age + Height + Weight + Year, data = trainfoot)
print(rf10)

predrf10 <- predict(rf10, testfoot)
table(predrf10)

####################################################################################
## wEIGHTLIFTING
data7 <- read.csv("WCo.csv",  header = TRUE, sep = ",")
str(data7)
table(data7$Medal)

df7 <- subset(data7[c("ID","Sex", "Age","Height","Weight","Year","Medal")])
cor(df7, method = c("pearson", "kendall", "spearman"))
data7$Medal<- as.factor(data7$Medal)
df8 <- subset(data7[c("ID","Sex", "Age","Height","Weight","Year","NOC","Medal")])

########
# cretaing data split for gymnastics with selected countries
set.seed(1234)
trainindex7 <- createDataPartition(df8$Medal, p = .8, list = FALSE)
trainweight <- df8[ trainindex7,]
testweight<- df8[-trainindex7,]
View(trainweight)
table(testweight$Medal)
table(testweight$NOC)

str(testweight)

## Random For
set.seed(1234)
rf11 <- randomForest(Medal ~ ., data = trainweight)
print(rf11)

predrf11 <- predict(rf11, testweight)
table(predrf11)
caret::confusionMatrix(testweight$Medal,predrf11)
compare2 <- cbind(testweight, predrf11)
View(compare)

## Random For
set.seed(1234)
rf12 <- randomForest(Medal ~  Sex + Age + Height + Weight + Year, data = trainweight)
print(rf12)

predrf12 <- predict(rf12, testweight)
table(predrf12)
