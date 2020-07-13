data <- read.csv("Olympics.csv",  header = TRUE, sep = ",")
View(data)
table(data$Medal)


library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)

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

data$Medal <- as.factor(data$Medal)
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
print(rf)
attributes(rf)
importance(rf)

p1 <- predict(rf, test)
p1
View(p1)
table(p1)
caret::confusionMatrix(test$Medal,p1)


### train & test data 2
##
finaldata <- subset(data[c("NOC","Event","Sex", "Age","Height","Weight","Year","Medal")])
str(finaldata)


set.seed(1234)
trainIndex1 <- createDataPartition(finaldata$Medal, p = .7, list = FALSE)
train1 <- finaldata[ trainIndex1,]
test1 <- finaldata[-trainIndex1,]
View(test1)
table(test1$Medal)

set.seed(1234)
rf1 <- randomForest(Medal ~ Age + Height + Weight + Year  , data = train1)
print(rf1)
importance(rf1)
str(train1)

p2 <- predict(rf1, test1)
p2
View(p2)
table(p2)
caret::confusionMatrix(test1$Medal,p2)

cm(table(test1[, 3], p2))

write.csv(p2, file = "RandomForrestpred.csv")



predict(rf1, finaldata, type="response",
        norm.votes=FALSE, predict.all=FALSE, proximity=TRUE, nodes=TRUE,
        finaldata$Medal)

predict(rf1, finaldata[Medal == 4], predict.all=TRUE)
