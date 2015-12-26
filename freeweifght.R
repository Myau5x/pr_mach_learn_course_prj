library(caret)
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

ColNums_NotAllMissing <- function(df){ # helper function
      as.vector(which(colSums(is.na(df)) != nrow(df)))
}

train2 <- train %>% select( which(colMeans(is.na(.)) < 0.95))
train2 <- train2 %>% select( which(colMeans(.=="") < 0.95), -X )

set.seed(91)
ModelRF <- train(classe ~ ., method="rf",data=train )
a<-confusionMatrix(train$classe,predict(ModelRF,train))

#tr1 <- train %>% select( X, classe)