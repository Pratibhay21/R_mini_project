#1st way
library(randomForest)
library(caret)
library(e1071)
library(dplyr)
library(tidyverse)
library(dplyr)
library(Metrics)
mydata<-read.csv("cpdata.csv")
splitting<-sample(2,nrow(mydata),replace=TRUE,prob=c(0.7,0.3))
train<-mydata[splitting==1,]
test<-mydata[splitting==2,]
rf_fit<-randomForest(as.factor(label) ~ . , data=train,ntree=1000)
rf_fit
rf_fit$confusion[,'class.error']
importance(rf_fit)
varImpPlot(rf_fit)
saveRDS(rf_fit,"model.rds")
y_pred=predict(rf_fit,test)
test$prediction=y_pred
view(test) #for comparison of actual and predicted
cfm=table(test$label,test$prediction) #best way to evaluate the accuracy of our model with confusion matrix
cfm
classification_accuracy=sum(diag(cfm)/sum(cfm))
classification_accuracy
actual=test$label
plot(y_pred,type='s',col='red',xlab='crops',ylab='names',main='line chart')
lines(actual,type='l',col='blue')

  plot(1:10, type = "l")
#as.numeric(as.character(choices)) 
  
  ##2nd way
  library(randomForest)
  library(caret)
  library(e1071)
  library(dplyr)
  library(ggplot2)
  mydata_train<-mydata_new[1:2480,] #80% for training
  mydata_test<-mydata_new[2481:3100,] #20% for testing
  mydata_train_target<-mydata[1:2480,5] #get crop label
  mydata_test_target<-mydata[2481:3100,5] #get crop label
  Feature_extraction <- function(mydata) {
    labels <- c("temperature",
                "humidity",
                "ph",
                "rainfall")
    features <- mydata[,labels]
    features$temperature[is.na(features$temperature)] <- -1
    features$humidity[is.na(features$humidity)] <- median(features$humidity, na.rm=TRUE)
    features$ph[features$ph==""] = "mango"
    features$rainfall     <- as.factor(features$rainfall)
    return(features)
  }
summary(Feature_extraction(mydata_train))
summary(Feature_extraction(mydata_test))
  # r_forest<-randomForest(Feature_extraction(mydata_train),as.factor(mydata_train$label),ntree=100)
  #r_forest
  #table(factor(mydata_train$label))
