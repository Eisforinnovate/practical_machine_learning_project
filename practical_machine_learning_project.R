#load necessary libraries
library(caret)
library(lattice)
library (ggplot2)
library (randomForest)
library (class)

#set working directory locally
setwd("~/Desktop/practical_machine)learning_project")

training <- read.table("~/Desktop/practical_machine)learning_project/pml-training.csv", header = TRUE, sep = ",")
##elimate first 8 columns
trainingOther <- training[, -seq(from = 1, to = 8, by = 1)]
##set seed for subsetting
set.seed(1111)

#test subset, 30% of training data

inTest<-createDataPartition(y=trainingOther$classe, p = 0.3, list= FALSE)
testSub <-trainingOther[-inTest,]

#training subset, 70% of training data
trainingSub <-trainingOther[-inTest,]

#data has 152 variables

thinData <-function(a){
  n<- length(a)
  na.count <-sum(is.nx(a))
  return((n-na/count)/n)
}

variable.thinData <-apply(trainingSub,2,thinData)

trimTrainSub <-trainingSub[,variable.thinData >0.9]

inVarImp <- createDataPartition(y = trimTrainSub$classe, p = 0.1, list = F)
varImpSub <- trimTrainSub[inVarImp, ]
varImpRF <- train(classe ~ ., data = varImpSub, method = "rf")


varImpObj <- varImp(varImpRF)

plot(varImpObj, main = "Variable Importance of Top 52", top = 52)

plot(varImpObj, main = "Variable Importance of Top 20", top = 20)

set.seed(12345)
finalTraingData <- trimTrainSub[-inVarImp, ]
impThresh <- quantile(varImpObj$importance[, 1], 0.75)
impfilter <- varImpObj$importance[, 1] >= impThresh
finalTraingData <- finalTraingData[, impfilter]
rfModel <- train(classe ~ ., data = finalTraingData, method = "rf")

# Apply data trimming to subdata set
trimTestSub <- testSub[, variable.sparseness > 0.9]
finalTestSub <- trimTestSub[, impfilter]
prediction <- predict(rfModel, finalTestSub)
missClass = function(values, prediction) {
  sum(prediction != values)/length(values)
}
errRate = missClass(finalTestSub$classe, prediction)
