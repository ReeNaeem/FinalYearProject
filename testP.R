library(ggplot2)
library(aod)
library(dplyr)
library(glmnet)
library(glmnetUtils)
library(dplyr)
library(caret)

Olympics <- read.csv("Olympics.csv",  header = TRUE, sep = ",")
View(Olympics) #in the data set, 0 = no medal, 1 = bronze, 2 = silver, 3 = gold
str(Olympics)

#turning msex into num
table(Olympics$Sex)
Olympics$Sex <- as.character((Olympics$Sex))
Olympics$Sex[Olympics$Sex %in% "F"] <- "1"
Olympics$Sex[Olympics$Sex %in% "M"] <- "2"
table(Olympics$Sex)

# then putting in it new df 
str(newevent)
Olympics$Sex <- as.numeric((Olympics$Sex))
newevent <- subset(data[c("Sex", "Age","Height","Weight","Year","Medal", "Sport")])
newevent$Medal <- as.numeric(data$Medal)
newevent$Year <- as.numeric(newdata$Year)
View(newevent)
cor(newevent, method = c("pearson", "kendall", "spearman"))

finalevent <- subset(data[c("Sex", "Age","Height","Weight","Year","Medal")])
str(finalevent)
Olympics$Year <- as.numeric(Olympics$Year)
Olympics$Medal <- as.factor(Olympics$Medal)

#Shapiro
shapiro.test(Olympics$Height) 


###correlation tests
library(corrplot)
corMatrix <- Olympics[,c("Age","Height","Weight","Year","Medal")]
corMatrix
M <- round(cor(corMatrix), 2)
M


### also split data
#set.seed(123)
#dt = sort(sample(nrow(Olympics), nrow(Olympics)*.7))
#train<-Olympics[dt,]
#test<-Olympics[-dt,]
#View(train)

#split data
#train<-sample_frac(testM, 0.75)
#sid<-as.numeric(rownames(train)) # because rownames() returns character
#test<-Olympics[-sid,]
#View(test)
###

# use this
set.seed(1234)
trainIndex2 <- createDataPartition(Olympics$Medal, p = .7, list = FALSE)
train2 <- Olympics[ trainIndex2,]
test2 <- Olympics[-trainIndex2,]
View(train2)

### model with  multi linear regression with olympic data
Model1 <- lm( Medal ~ Sex + Age + Height + Weight + Year + NOC + Event, data=train2)
summary(Model1)

preds = predict(Model1, test2)
preds
View(preds)
plot()

########################################################################

# split data for newevent df
set.seed(1234)
trainIndex3 <- createDataPartition(newevent$Medal, p = .7, list = FALSE)
train3 <- newevent[ trainIndex3,]
test3 <- newevent[-trainIndex3,]
View(train1)

# model with multi lin regr with newevent data
Model2 <- multinom(Medal ~ Sex + Age + Height + Weight + Year + Sport, data=train1)
summary(Model2)

preds2 = predict(Model2, test1)
preds2
table(preds2)
View(preds2)
plot()


########################################################################
set.seed(1234)
trainIndex4 <- createDataPartition(Olympics$Medal, p = .7, list = FALSE)
train4 <- Olympics[ trainIndex4,]
test4 <- Olympics[-trainIndex4,]
View(tra)

## logistic regression model
mylogit <- glm(Medal ~  Sex + Age + Height + Weight + Year, data=train1)

preds4 = predict(mylogit, test1)
table(preds4)
View(preds4)
plot()

########################################################################
set.seed(1234)
trainIndex5 <- createDataPartition(finalevent$Medal, p = .7, list = FALSE)
train5 <- finalevent[trainIndex5,]
test5 <- finalevent[-trainIndex5,]
View(train)
str(train5)

## logistic regression model
mylogit1 <- glm(Medal ~  Sex + Age + Height + Weight + Year, data=train5, family = binomial)

preds5 = predict(mylogit1, test5)
preds5
View(preds5)
plot()

##########################################################################


## logistic regression model using train 3 split data
mylogit2 <- glm(Medal ~  Sex + Age + Height + Weight + Year, data=train3)

preds6 = predict(mylogit1, test3)
preds6
View(preds6)
plot()





write.csv(preds, file = "preds1.csv")
write.csv(preds2, file = "preds2.csv")

View(preds3)

roc(as.numeric(as.character(QA_test$Average_Severity)), as.numeric(as.character(preds.Q.dtm)))
confusionMatrix(preds.Q.dtm, QA_test$Average_Severity, positive="1")

#############################

testM <- subset(test, Sex=="M")
testC <- subset(testM, Team=="United States")
testC1<- subset(testM, Team=="Great Britain")

View(testC1)
#
#Linear Regresison 
#nope
Model <- lm( Medal ~ Age + Height + Weight + Team, Event, data=testM)
Model
summary(Model)

#continue
Model1 <- lm( Medal ~ Age + Height + Weight + Event, data=testC)
summary(Model1)
summary(Model1)$coefficient
confint(Model1)
sigma(Model1)/mean(testC$Medal)


Model2 <- lm( Medal ~ Age + Height + Weight + Event, data=testC1)
summary(Model2)

#using al countries 
Model3 <- lm( Medal ~ Age + Height + Weight + Event, data=test)
summary(Model3)

View(test1)
View(test1)

## Logistic Regression
testC$Medal <- factor(testC$Medal)
mylogit <- glm(Medal ~ Age + Height + Weight + Event, data=testC1)
summary(mylogit)
confint(mylogit) ## CIs using profiled log-likelihood
confint.default(mylogit) ## CIs using standard errors

mylogit1 <- glm(Event ~ Age + Height + Weight + Medal, data=testC,  family = "binomial")
summary(mylogit1)
predictedY <- predict(logitMod, testC, Medal=="response") 
