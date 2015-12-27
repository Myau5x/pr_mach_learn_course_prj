library(caret)
library(plyr)
library(ipred)
library(e1071)
library(dplyr)

test <- read.csv("pml-testing.csv")
train <- read.csv("pml-training.csv")
#trA<- train %>% filter(classe == "A")
#trB<- train %>% filter(classe == "B")
#trC<- train %>% filter(classe == "C")
#trD<- train %>% filter(classe == "D")
#trE<- train %>% filter(classe == "E")
#t <- predict(modelFit,train)
#a<- confusionMatrix(train$classe,t)

inTrain <- createDataPartition(y=train$classe,
                               p=0.7, list=FALSE)
train2 <- train %>% select( which(colMeans(is.na(.)) < 0.95))
#test2 <- test %>% select( which(colMeans(is.na(train)) < 0.95))
#test2 <- test2 %>% select( which(colMeans(train2=="") < 0.95), -X )
train2 <- train2 %>% select( which(colMeans(train2=="") < 0.95), -X )
testing <- train2[-inTrain,]
train2 <- train2[inTrain,]


set.seed(91)
#ModelRF <- train(classe ~ ., method="rf",data=train2 )
#a<-confusionMatrix(train$classe,predict(ModelRF,train2))
folds <- createFolds(y = train2$classe, k = 20)



#trControl=trainControl(
 #     method='timeslice',
  #    initialWindow=12, fixedWindow=TRUE, 
   #   horizon=12, summaryFunction=mySummary,
    #  verboseIter=TRUE))
fitControl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      number = 10,
      ## repeated ten times
      repeats = 10)
##,  trControl=fitControl
#model <- train(classe ~., data = train2, method='rpart' )
#modeltreebag <- train(classe ~., data = train2, method='treebag' )
preProc <- preProcess(train2,method="pca",thresh = 0.9)
#newTrain <- training[,c(ilp,diagnosis)]
trainPC <- predict(preProc,train2)
### treebag 
modelFitPC <- train(classe ~ .,method="treebag",data=trainPC)
### randomforest
#modelFitPC <- train(classe ~ .,method="rf",data=trainPC)

testPC <- predict(preProc,testing)
tttr<- confusionMatrix(train2$classe,predict(modelFitPC,trainPC))
ttts<- confusionMatrix(testing$classe,predict(modelFitPC,testPC))


#btr<- confusionMatrix(train2$classe,predict(model,train2))
#bts<- confusionMatrix(testing$classe,predict(model,testing))



#tr1 <- train %>% select( X, classe)