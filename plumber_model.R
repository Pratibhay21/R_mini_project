library(plumber)
library(randomForest)
#* return the input
#* 
#* @get /patrol

function(messg = ""){
  list(messg = paste0("Hi I am listening '", messg, "'"))
}
## Load the model
modellist = readRDS("cropmodel.rds")
## Lets make the predictions

#* @param temperature
#* @param humidity
#* @param ph
#* @param rainfall
#* @get /predict
predictions <- function(temperature, humidity, ph, rainfall){
  temperature <- as.numeric(temperature)
  humidity <-  as.numeric(humidity)
  ph  <- as.numeric(ph)
  rainfall <-  as.numeric(rainfall)
  
  X.new <- data.frame(temperature= temperature,
                      humidity = humidity ,
                     ph =ph,
                     rainfall = rainfall)
  
  #predict based on input
   ##predict(iris_rf, new_data= X.new, type ="class")
  y.pred <- modellist$NewPredictions(model = modellist$modelobject, newdata = X.new)
   return(y.pred)
}

