data <- read.csv("Olympics.csv",  header = TRUE, sep = ",")
View(data)
table(data$Medal)


library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(fitdistrplus)

plot(data$Medal)
hist(data$Weight)
table(data$Medal)


ggplot(data, aes(x=Medal)) + geom_histogram()


str(data)

table(data$Sex)
data$Sex <- as.character((data$Sex))
data$Sex[data$Sex %in% "F"] <- "1"
data$Sex[data$Sex %in% "M"] <- "2"
table(data$Sex)


str(newdata)
data$Sex <- as.numeric((data$Sex))
newdata <- subset(data[c("Sex", "Age","Height","Weight","Year","Medal")])
newdata$Medal <- as.numeric(data$Medal)
newdata$Year <- as.numeric(newdata$Year)
View(newdata)
cor(newdata, method = c("pearson", "kendall", "spearman"))
##########################################################

data$Medal <- as.factor(data$Medal)
data$Year <- as.numeric(data$Year)

newdata$Medal <- as.factor(newdata$Medal)
data$Year <- as.numeric(data$Year)

table(newdata$Medal)

##train and test data for prediction 1
set.seed(1234)
trainIndex <- createDataPartition(newdata$Medal, p = .7, list = FALSE)
train <- newdata[ trainIndex,]
test <- newdata[-trainIndex,]
View(train)
table(test$Medal)



#randomForrest
set.seed(1234)
rf <- randomForest(Medal ~. , data = train)
print(rf1)
importance(rf1)
str(train1)
p1 <- predict(rf, test)
p1
View(p1)
table(p1)
caret::confusionMatrix(test$Medal,p1)


### train & test data 2
##
finaldata <- subset(data[c("NOC","Event","Sport","Sex", "Age","Height","Weight","Year","Medal")])
str(finaldata)


### train & test data 2
set.seed(1234)
trainIndex1 <- createDataPartition(finaldata$Medal, p = .7, list = FALSE)
train1 <- finaldata[ trainIndex1,]
test1 <- finaldata[-trainIndex1,]
View(test1)
table(test1$Medal)

##
### This model appears to be the best one 
set.seed(1234)
rf1 <- randomForest(Medal ~ Sex + Age + Height + Weight + Year + Sport  , data = train1)


p2 <- predict(rf1, test1)
p2
View(p2)
table(p2)
caret::confusionMatrix(test1$Medal,p2)

set.seed(1234)
rf2 <- randomForest(Medal ~ Sex + Age + Height + Weight + Year  , data = train1)
print(rf2)
p3 <- predict(rf2, test1)
p3
table(p3)

caret::confusionMatrix(test1$Medal,p3)

############################################
set.seed(1234)
trainIndex2 <- createDataPartition(finaldata$Medal, p = .8, list = FALSE)
train2 <- finaldata[ trainIndex2,]
test2 <- finaldata[-trainIndex2,]
View(test2)
table(test2$Medal)

set.seed(1234)
rf3 <- randomForest(Medal ~ Sex + Age + Height + Weight + Year + Sport  , data = train2)
print(rf3)
importance(rf3)
str(train1)

p4 <- predict(rf3, test2)
p4
View(p4)
table(p4)
caret::confusionMatrix(test2$Medal,p4)

set.seed(1234)
rf4 <- randomForest(Medal ~ Sex + Age + Height + Weight + Year , data = train2)
print(rf4)
importance(rf4)
str(train2)

p5 <- predict(rf4, test2)
p5
View(p5)
table(p5)
caret::confusionMatrix(test2$Medal,p5)

############################################
finald2 <- subset(finaldata)
str(finald2)
finald2$Sex <- as.factor(finald2$Sex)

set.seed(1234)
trainIndex3 <- createDataPartition(finald2$Medal, p = .7, list = FALSE)
train3 <- finald2[ trainIndex3,]
test3 <- finald2[-trainIndex3,]
View(test3)
table(test3$Medal)

set.seed(1234)
rf5 <- randomForest(Medal ~ Sex + Age + Height + Weight + Year  , data = train3)
print(rf5)
importance(rf5)
str(train3)

p6 <- predict(rf5, test3)
p6
View(p6)
table(p6)
caret::confusionMatrix(test3$Medal,p6)

############################################
cm(table(test1[, 3], p2))

write.csv(p3, file = "RandomForestp3.csv")



predict(rf1, finaldata, type="response",
        norm.votes=FALSE, predict.all=FALSE, proximity=TRUE, nodes=TRUE,
        finaldata$Medal)

predict(rf1, finaldata[Medal == 4], predict.all=TRUE)
