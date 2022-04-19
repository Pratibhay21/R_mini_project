## crop production DATA SET Prediction
crop_data<-read.csv("cpdata.csv")
library(dplyr)
library(randomForest)

## Lets have a look at the data
#glimpse(crop_data)

#Lets Build a Random forest model
cropdata_rf = randomForest(as.factor(label) ~ temperature + humidity 
                       + rainfall +ph,
                       data = crop_data, ntree = 1000)
## Make a prediction function
NewPredictions <- function(model, newdata){
  new.predictions <- predict(object = model, newdata = newdata)
  return(new.predictions)
  }

modellist <- vector(mode = 'list')
# Save fitted model here.
modellist$modelobject <-cropdata_rf
modellist$NewPredictions <- NewPredictions
saveRDS(object = modellist , file = 'cropmodel.rds')

