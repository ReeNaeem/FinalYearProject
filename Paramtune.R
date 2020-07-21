p2
table(p2)

listLearners("classif")[c("class","package")]

getParamSet("classif.randomForest")

#create a task
trainTask <- makeClassifTask(data = train1,target = "Medal")
testTask <- makeClassifTask(data = test1, target = "Medal")

trainTask

trainTask <- makeClassifTask(data = train1,target = "Medal", positive = "1")# Error: Cannot set a positive class for a multiclass problem!

#normalize the variables
trainTask <- normalizeFeatures(trainTask,method = "standardize")
testTask <- normalizeFeatures(testTask,method = "standardize")

trainTask <- dropFeatures(task = trainTask,features = c("NOC","Event"))

# qda
qda.learner <- makeLearner("classif.qda", predict.type = "response")

#train model
qmodel <- train(qda.learner, trainTask)

#predict on test data
qpredict <- predict(qmodel, testTask)
View(qpredict)

submit <- data.frame(Year = test$Year, Medal = qpredict$data$response)
View(submit)
table(submit)
testcheck <- cbind(test1,submit)
View(testcheck)
write.csv(testcheck, "submit1.csv",row.names = F)

#create a learner
rf6 <- makeLearner("classif.randomForest", predict.type = "response", par.vals = list(ntree = 200, mtry = 3))
rf6$par.vals <- list(
  importance = TRUE
)

#set tunable parameters
#grid search to find hyperparameters
randf_param <- makeParamSet(
  makeIntegerParam("ntree",lower = 700, upper = 700),
  makeIntegerParam("mtry", lower = 15, upper = 15),
  makeIntegerParam("nodesize", lower = 50, upper = 50)
)

#let's do random search for 50 iterations
rancontrol <- makeTuneControlRandom(maxit = 1L)
rancontrol

#set 3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)


#hypertuning
randomf_tune <- tuneParams(learner = rf6, resampling = set_cv, task = trainTask, par.set = rf_param, control = rancontrol, measures = acc)

randomf.tree <- setHyperPars(rf6, par.vals = randomf_tune$x)

#train a model
newrf <- train(randomf.tree, trainTask)
getLearnerModel(newrf)

#make predictions
rfmodeltune <- predict(newrf, testTask)
rfmodeltune
table(rfmodeltune)                      

peramtunepred <- data.frame(Age = test$Age, Medal = rfmodel$data$response)                      
table(peramtunepred$Medal)
caret::confusionMatrix(testTask$Medal,rfmodeltune)

