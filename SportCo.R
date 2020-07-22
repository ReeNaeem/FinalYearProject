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