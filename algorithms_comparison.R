#setwd("C:/Users/user/OneDrive/Desktop/R_miniproject")
mydata<-read.csv("cpdata.csv")
library(mlbench)
library(caret)
library(randomForest)
set.seed(100)
train<-sample(nrow(mydata),0.7*nrow(mydata),replace = FALSE)
TrainSet<-mydata[train,]
ValidSet <- mydata[-train,]

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# CART
set.seed(7)
fit.cart <- train(as.factor(label)~., data=TrainSet, method="rpart", trControl=control)
# LDA
set.seed(7)
fit.lda <- train(as.factor(label)~., data=TrainSet, method="lda", trControl=control)
# SVM
set.seed(7)
fit.svm <- train(as.factor(label)~., data=TrainSet, method="svmRadial", trControl=control)
# kNN
set.seed(7)
fit.knn <- train(as.factor(label)~., data=TrainSet, method="knn", trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(as.factor(label)~., data=TrainSet, method="rf", trControl=control)

# collect resamples
results <- resamples(list(CART=fit.cart, LDA=fit.lda, SVM=fit.svm, KNN=fit.knn, RF=fit.rf))
summary(results)

scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)

#dataset is arranged in too organized order which can create problem , so mix it
set.seed(9850) #to mix up/shuffle everthing as dataset is arranged in order
gp<-runif(nrow(mydata))
gp
mydata_gp<-mydata[order(gp),]
str(mydata_gp)
head(mydata_gp) #now it has good mixture

summary(mydata_gp[,c(1,2,3,4)]) #to get summary of 1,2,3,4 columns
normalize<-function(x){
  return(  (x-min(x)) / (max(x)-min(x)) )
}
#normalizing the data of 4 columns
mydata_new<-as.data.frame(lapply(mydata_gp[,c(1,2,3,4)],normalize))
str(mydata_new)
summary(mydata_new) #we see the normalized data
mydata_train<-mydata_new[1:2480,] #80% for training
mydata_test<-mydata_new[2481:3100,] #20% for testing
mydata_train_target<-mydata[1:2480,5] #get crop label
mydata_test_target<-mydata[2481:3100,5] #get crop label
require(class) #Loading required package: class
sqrt(3100) #55.67764


#knn algorithm
m1<-knn(train=mydata_train,test=mydata_test,cl=mydata_train_target,k=56)
m1
table(mydata_test_target,m1)

#linear regression(very poor here)
plot( temperature ~ humidity ,data=mydata_gp,col="yellow")
plot(humidity ~ rainfall , data=mydata_gp,col="brown")
cor(mydata_gp$temperature,mydata_gp$humidity) #poor
cor(mydata_gp$humidity,mydata_gp$rainfall)    #poor
cor(mydata_gp$temperature,mydata_gp$rainfall)  #poor
cor(mydata_gp$ph,mydata_gp$temperature)         #poor
m2<-lm(humidity ~ rainfall ,data=mydata_gp)
ls()
m2 #humidity &rainfall
summary(m2)
abline(m2,col="red",lty=2,lwd=2)
m2

##random forest 
library(randomForest)
set.seed(100)
train<-sample(nrow(mydata),0.7*nrow(mydata),replace = FALSE)
TrainSet<-mydata[train,]
ValidSet <- mydata[-train,]
summary(TrainSet)
summary(ValidSet)
# Create a Random Forest model with default parameters
model6 <- randomForest(as.factor(label) ~ ., data = TrainSet, importance = TRUE)
model6

model7 <- randomForest(as.factor(label) ~ ., data = TrainSet, ntree = 500, mtry = 6, importance = TRUE)
model7

# Predicting on train set
predTrain <- predict(model7, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$label)  
library(Metrics)
library(caret)
confusionMatrix(table(predTrain, TrainSet$label) )

head(as.data.frame(predTrain))
head(as.data.frame(predValid))
predTrain["temperature"]
plot(predTrain)

# Checking classification accuracy
mean(predValid == ValidSet$label) #shows 0.937                   
table(predValid,ValidSet$label)
# To check important variables
importance(model7)        
varImpPlot(model7)

# Using For loop to identify the right mtry for model
a=c()
i=5
for (i in 3:8) {
  model8 <- randomForest(as.factor(label) ~ ., data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model8, ValidSet, type = "class")
  a[i-2] = mean(predValid == ValidSet$label)
}
a
plot(3:8,a)

# Compare with Decision Tree 
library(rpart)
library(caret)
library(e1071)
# We will compare model 6 of Random Forest with Decision Tree model
model_dt = train(as.factor(label) ~ ., data = TrainSet, method = "rpart")
model_dt_6 = predict(model_dt, data = TrainSet)
table(model_dt_6, TrainSet$label)
mean(model_dt_6 == TrainSet$label)
####On the training dataset, the accuracy is around 0.2811% and there is lot of misclassification. 
###Now, looking at the validation dataset.
model_dt_vs = predict(model_dt, newdata = ValidSet)
table(model_dt_vs, ValidSet$label)
mean(model_dt_vs == ValidSet$label)
#####The accuracy on validation dataset has decreased further to 0.2021%.

library(dplyr)
library(randomForest)
library(caret)
library(e1071)
randomForest(formula, ntree=n, mtry=FALSE, maxnodes = NULL)
trainControl(method = "cv", number = n, search ="grid")
# Define the control
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")
##view later##train(formula, mydata, method = "rf", metric= "Accuracy", trControl = trainControl(), tuneGrid = NULL)


