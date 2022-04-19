######EPLORATORY DATA ANALYSIS###########
##Exploring data  consists of 1.Summarization 2.Visualization 3.Normalization


#setting the working directory
setwd("C:/Users/user/OneDrive/Desktop/R_miniproject")
#read the CSV file
mydata<-read.csv("cpdata.csv")
#know the type of object (ie. here is dataframe)
class(mydata)
#print the dataset
print(mydata)
#show structure of data object
str(mydata)
#shows total number of rows ie.3100
nrow(mydata)
#shows total number of columns ie. 5
ncol(mydata)
#show first 6 observations of dataset
head(mydata)
#show last 6 observations of dataset
tail(mydata)
#shows categorial representation of data and their frequency
table(mydata$label) 
#shows distinct column  names ie.attributes
dimnames(mydata) 
#shows unique crop names ie. "label"
unique(mydata$label) 
#shows number of unique crops ie.31
length(unique(mydata$label))
#overall summary of dataset
summary(mydata)
#checking if there is any null value ie.not available and missing values
is.na(mydata)  
# locate position exactly where is null
which(is.na(mydata)) 

#maximum and minimum values of each parameters
max(mydata$temperature) #get maximum temperature value
min(mydata$temperature) #get minimum temperature value
max(mydata$humidity) #get maximum humidity value
min(mydata$humidity) #get minimum humidity value
max(mydata$ph) #get maximum ph value
min(mydata$ph)  #get minimum ph value
max(mydata$rainfall) #get maximum rainfall value
min(mydata$rainfall) #get minimum rainfall value


###central tendencies###
#get individual parameter mean
mean(mydata$temperature) 
mean(mydata$ph)
mean(mydata$rainfall)
mean(mydata$humidity)

#get individual parameter median
median(mydata$temperature) 
median(mydata$ph)
median(mydata$rainfall)
median(mydata$humidity)

##mode of temperature parameter
t=mydata$temperature
#first create a mode funtion
Mode=function(x){
  ta=table(x)
  tam=max(ta)
  if(all(ta==tam))
    mod=NA
  else
    if(is.numeric(x))
      mod=as.numeric(names(ta)[ta==tam])
  else
    mod=names(ta)[ta==tam]
  return(mod)

}

#calling mode function
Mode(t)

#get range of temperature
range(mydata$temperature) 
range(mydata$humidity)
range(mydata$ph)
range(mydata$rainfall)

#get quantile of individual parameter
quantile(mydata$temperature) 
quantile(mydata$ph)
quantile(mydata$humidity)
quantile(mydata$rainfall)

#get IQR of individual parameter
IQR(mydata$temperature)  
IQR(mydata$ph)
IQR(mydata$rainfall)
IQR(mydata$humidity)

#variance of individual parameter
var(mydata$temperature) 
var(mydata$ph)
var(mydata$rainfall)
var(mydata$humidity)

#standard deviation of individual parameter
sd(mydata$temperature) 
sd(mydata$ph)
sd(mydata$rainfall)
sd(mydata$humidity)

#get aggregate on basis of individual parameter
aggregate(ph ~ label, data = mydata, mean) 
aggregate(humidity ~ label, data = mydata, mean)
aggregate(rainfall ~ label, data = mydata, mean)
aggregate(temperature ~ label, data = mydata, mean)

#get individual parameter summary
summary(mydata$ph) 
summary(mydata$temperature)
summary(mydata$humidity)
summary(mydata$rainfall)

#other analysis
sort(mydata$temperature) #sorts ascending
rev(sort(mydata$temperature)) #sorts descending
rank(mydata$temperature) #ranks the positionwise
order(mydata$temperature) #positions ascending manner


#Data visualization
crops=mydata$label
#numeric data- histograms
hist(t)

#categorial data -barplot
table(mydata$label)
barplot(table(mydata$label))

# categorial data -boxplot
boxplot(table(mydata$label))

#data normalization
library(stats) #for data exploration
library(dplyr) #for data manipulation
mydata_numeric=select(mydata,c(1,2,3,4))

#Normalize Dataset(Z-scores) -all variables on the same scale
Zscore_mydata=scale(mydata_numeric) ##to get standardized form


#check all variables onverted to Z-scores
View(Zscore_mydata)





