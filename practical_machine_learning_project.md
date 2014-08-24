
```r
#load necessary libraries
library(caret)
```

```
## Warning: package 'caret' was built under R version 3.1.1
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
library(lattice)
library (ggplot2)
library (randomForest)
```

```
## Warning: package 'randomForest' was built under R version 3.1.1
```

```
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

```r
library (class)
```

```
## Warning: package 'class' was built under R version 3.1.1
```

```r
library (knitr)
library (e1071)
```

```
## Error: there is no package called 'e1071'
```


```r
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
```


```r
# function for determining sparseness of variables
sparseness <- function(a) {
    n <- length(a)
    na.count <- sum(is.na(a))
    return((n - na.count)/n)
}

# sparness of input variables based on training subset
variable.sparseness <- apply(trainingSub, 2, sparseness)

# trim down the subs by removing sparse variables
trimTrainSub <- trainingSub[, variable.sparseness > 0.9]
```


```r
inVarImp <- createDataPartition(y = trimTrainSub$classe, p = 0.1, list = F)
varImpSub <- trimTrainSub[inVarImp, ]
varImpRF <- train(classe ~ ., data = varImpSub, method = "rf")
```

```
## Error: there is no package called 'e1071'
```

```r
varImpObj <- varImp(varImpRF)
```

```
## Error: object 'varImpRF' not found
```


```r
plot(varImpObj, main = "Variable Importance of Top 52", top = 52)
```

```
## Error: object 'varImpObj' not found
```

```r
plot(varImpObj, main = "Variable Importance of Top 20", top = 20)
```

```
## Error: object 'varImpObj' not found
```

```r
set.seed(12345)
finalTraingData <- trimTrainSub[-inVarImp, ]
impThresh <- quantile(varImpObj$importance[, 1], 0.75)
```

```
## Error: object 'varImpObj' not found
```

```r
impfilter <- varImpObj$importance[, 1] >= impThresh
```

```
## Error: object 'varImpObj' not found
```

```r
finalTraingData <- finalTraingData[, impfilter]
```

```
## Error: object 'impfilter' not found
```

```r
rfModel <- train(classe ~ ., data = finalTraingData, method = "rf")
```

```
## Error: there is no package called 'e1071'
```

```r
# Apply data trimming to subdata set
trimTestSub <- testSub[, variable.sparseness > 0.9]
finalTestSub <- trimTestSub[, impfilter]
```

```
## Error: object 'impfilter' not found
```

```r
prediction <- predict(rfModel, finalTestSub)
```

```
## Error: object 'rfModel' not found
```

```r
missClass = function(values, prediction) {
  sum(prediction != values)/length(values)
}
errRate = missClass(finalTestSub$classe, prediction)
```

```
## Error: object 'prediction' not found
```
