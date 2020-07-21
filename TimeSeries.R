Bronze <- read.csv("Bronze.csv",  header = TRUE, sep = ",")
View(Bronze)
str(Bronze)

Bronze$Total <- as.numeric(Bronze$Total)


set.seed(1234)
trainIndex10 <- createDataPartition(Bronze$Total, p = .8, list = FALSE)
train10 <- Bronze[trainIndex10,]
test10 <- Bronze[-trainIndex10,]
View(test10)
str(train10)

Time1 <- lm(Total ~ ., data=train10)
View(Time1)

B2020 <- predict(Time1, test10)
View(B2020)

B <- predict(Time1, Bronze)
View(B)

set.seed(1234)
trainIndex11 <- createDataPartition(Bronze$Total, p = .6, list = FALSE)
train11 <- Bronze[trainIndex11,]
test11 <- Bronze[-trainIndex11,]
View(test11)
str(train11)

Time2 <- lm(Total ~ ., data=train11)
View(Time1)

B12020 <- predict(Time2, test11)
View(B12020)

B1 <- predict(Time2, Bronze)
View(B)

Bronze1 <- cbind(Bronze, B1, B)
View(Bronze1)

#######################
#Silver
Silver <- read.csv("Silver.csv",  header = TRUE, sep = ",")
Silver$Total <- as.numeric(Silver$Total)
View(Silver)


set.seed(1234)
trainIndex12 <- createDataPartition(Silver$Total, p = .8, list = FALSE)
trainS <- Silver[trainIndex12,]
testS <- Silver[-trainIndex12,]
View(testS)
str(trainS)

TimeS <- lm(Total ~ ., data=trainS)
View(Time1)

S2020 <- predict(TimeS, testS)
View(S2020)

S <- predict(TimeS, Silver)
View(S)

set.seed(1234)
trainIndex13 <- createDataPartition(Silver$Total, p = .5, list = FALSE)
trainS1 <- Silver[trainIndex13,]
testS1 <- Silver[-trainIndex13,]
View(testS1)
str(train11)

TimeS1 <- lm(Total ~ ., data=trainS1)
View(Time1)

S12020 <- predict(TimeS1, testS1)
View(B12020)

S1 <- predict(TimeS1, Silver)
View(B)

Silver1 <- cbind(Silver, S, S1)
View(Silver1)

################################
########## GOLD
Gold <- read.csv("Gold.csv",  header = TRUE, sep = ",")
Gold$Total <- as.numeric(Gold$Total)
View(Gold)

set.seed(1234)
trainIndex14 <- createDataPartition(Gold$Total, p = .8, list = FALSE)
traing <- Gold[trainIndex14,]
testg <- Gold[-trainIndex14,]
View(testg)
str(trainS)

TimeG <- lm(Total ~ ., data=traing)
View(Time1)

G2020 <- predict(TimeG, testg)
View(G2020)

G <- predict(TimeG, Gold)
View(G)

set.seed(1234)
trainIndex15 <- createDataPartition(Gold$Total, p = .6, list = FALSE)
traing1 <- Gold[trainIndex15,]
testg1 <- Gold[-trainIndex15,]
View(testg1)
str(traing1)

TimeG1 <- glm(Total ~ ., data=traing1)
View(Time1)

G12020 <- predict(TimeG1, testg1)
View(G12020)

G1 <- predict(TimeG1, Gold)
View(B)

Gold1 <- cbind(Gold, G, G1)
View(Gold1)
str(Gold1)








################################
n <- names(Gold1)
f <- as.formula(paste("Total ~", paste(n[!n %in% c("Country","Year","G", "G1")], collapse = " + ")))
View(f)

nn<-NULL
nn <- neuralnet(f, Gold1 ,linear.output=T, rep = 10)
plot(nn)
