---
title: "Practical Machine Learning-Project"
author: "Daniela Aguilar"
date: "04/05/2021"
output: html_document
---
```{r include=FALSE}
library(caret)
library(forecast)
```
```{r setoptions, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
# The goal
The aim of this project was to build a model to predict from the data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants, the manner in which they did the exercise (they were asked to fo barbell lifts correctly and incorrectly in 5 different ways). As well, with this model 20 different test cases were predicted. 

# Data set up, exploratory analysis, and data cleaning 
Data was split into training and test sets (subset of training set). 
Models were built on training set only and evaluated on the testing data.
```{r}
set.seed(4112)
testing = read.csv("~/Desktop/pml-testing.csv")
training= read.csv("~/Desktop/pml-training.csv")
inTrain = createDataPartition(training$classe, p = .7, list=FALSE)
trainset = training[ inTrain,]
testset = training[-inTrain,]
```
Cross Validation:
```{r}
control <- trainControl(method="cv", number=3, verboseIter=F)
```

Remove non-relevant variables
```{r}
trainset<-trainset[,-(1:5)]
testset<-testset[,-(1:5)]
```
Identify variables that have no variability in them and therefore, won't be good predictors. 
```{r}
nsv<-nearZeroVar(trainset)
trainset<-trainset[,-nsv]
testset<-testset[,-nsv]
```




Remove missing values 
```{r}
isna<- sapply(trainset, function(x) mean(is.na(x))) > 0.95
trainset <- trainset[, isna==FALSE]
testset  <- testset[, isna==FALSE]
```

# Model Building
Three models were analyzed to see which one of these options provided the higher accuracy.

## Algorithm 1 (Random Forests)
```{r}
modfit<-train(classe ~., data=trainset, method="rf", trControl = control, tuneLength = 5)
pred<-predict(modfit, testset)
result<-confusionMatrix(pred, factor(testset$classe))
result
```
---![Screen Shot 2021-05-05 at 3 18 09 PM](https://user-images.githubusercontent.com/78766486/117210603-21a3c980-adb5-11eb-8068-ba538bd7d874.png)

## Algorithm 2 (Generalized boosted model)
```{r}
modfit1 <- train(classe ~., data=trainset, method="gbm", trControl = control, tuneLength = 5, verbose=FALSE)
pred1<-predict(modfit1, testset)
result1<-confusionMatrix(pred1, factor(testset$classe))
result1
```

---![Screen Shot 2021-05-05 at 3 24 58 PM](https://user-images.githubusercontent.com/78766486/117211392-29b03900-adb6-11eb-881b-6578034cf064.png)



## Algorithm 3 (linear discriminative analysis)
```{r}
modfit2 <- train(classe ~., data=trainset, method="lda", trControl = control, tuneLength = 5)
pred2<-predict(modfit2, testset)
result2<-confusionMatrix(pred2, factor(testset$classe))
result2
```
---![Screen Shot 2021-05-05 at 3 25 10 PM](https://user-images.githubusercontent.com/78766486/117211402-2cab2980-adb6-11eb-9268-c74d5d8842bd.png)

We can see that the model with the highest accuracy is the Generalized boosted model.. It's expected out of sample error is 1-accuracy (.008). 

Thus we will use it to predict our new data set (20 diff test cases):

```{r}
prediction<-predict(modfit1, newdata=testing)
prediction
```
---![Screen Shot 2021-05-05 at 3 25 19 PM](https://user-images.githubusercontent.com/78766486/117211462-3e8ccc80-adb6-11eb-8214-e06f5e3d7d99.png)
