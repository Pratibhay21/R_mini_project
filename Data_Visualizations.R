#####data visualizations##### 
#setwd("C:/Users/user/OneDrive/Desktop/R_miniproject")
mydata<-read.csv("cpdata.csv")

library(ggplot2)
library(tidyverse)
library(dplyr)
library(hrbrthemes)

########histogram plots############
#temperature histogram of crops
ggplot(data=mydata, aes(x=temperature, fill=label)) +
  geom_histogram(binwidth=0.2, alpha=0.5, position="identity") +
  theme_minimal()

#ph histogram of crops
ggplot(data=mydata, aes(x=ph, fill=label)) +
  geom_histogram(binwidth=0.2, alpha=0.5, position="identity") +
  theme_minimal()

#humidity histogram of crops
ggplot(data=mydata, aes(x=humidity, fill=label)) +
  geom_histogram(binwidth=0.2, alpha=0.5, position="identity") +
  theme_minimal()

#rainfall histogram of crops
ggplot(data=mydata, aes(x=rainfall, fill=label)) +
  geom_histogram(binwidth=0.2, alpha=0.5, position="identity") +
  theme_minimal()

########density plots############
#ph density plot of crops
ggplot(data=mydata, aes(x=ph, fill=label)) +
  geom_density(alpha=0.5) +xlim(3,10)
theme_minimal()

#temperature density plot of crops
ggplot(data=mydata, aes(x=temperature, fill=label)) +
  geom_density(alpha=0.5) +xlim(8,55)
theme_minimal()

#rainfall density plot of crops
ggplot(data=mydata, aes(x=rainfall, fill=label)) +
  geom_density(alpha=0.5) +xlim(20,398)
theme_minimal()

#humidity density plot of crops
ggplot(data=mydata, aes(x=humidity, fill=label)) +
  geom_density(alpha=0.5) +xlim(10,100)
theme_minimal()

#########scatterplots############
plot(mydata$ph,xlab="observations",ylab="ph values",main="Ph plot",col="green")
plot(mydata$temperature,,xlab="observations",ylab="Temperature values",main="Temperature plot",col="orange")
plot(mydata$rainfall,,xlab="observations",ylab="rainfall values",main="Rainfall plot",col="purple")
plot(mydata$humidity,,xlab="observations",ylab="humidity values",main="humidity plot",col=" hotpink")

#2 parameters scatterplot with filters
plot(humidity ~ rainfall,data=mydata,main="scatterplot ",col="red",pch=2) #shows all
plot(humidity ~ rainfall,data=filter(mydata,mydata$rainfall>300),main="scatterplot",col="green") #gets filtered on rainfall constraints
plot(humidity ~ rainfall,data=filter(mydata,mydata$humidity>94),main="scatterplot",col="purple",pch=2) ##gets filtered on humidity constraints
plot(ph ~ rainfall,data=filter(mydata,mydata$ph>9),main="scatterplot",col="maroon",pch=2) #gets filtered on ph constraints
plot(ph ~ temperature,data=filter(mydata,mydata$temperature>35),main="scatterplot",col="blue",pch=2) #gets filtered on temperature constraints

##########piechart##########
pie(table(mydata$label),main="Crops",col=rainbow(31)) #crops piechart
pie(table(mydata$label),main="Crops",col=heat.colors(31)) #same as above
pie(table(mydata$label),main="Crops",col=terrain.colors(31)) #same as above
pie(table(mydata$label),main="Crops",col=topo.colors(31)) #same as above
pie(table(mydata$label),main="Crops",col=cm.colors(31)) #same as above

######### simple histograms###########
hist(mydata$ph,col="yellow",border="blue",main="Ph Histogram",xlab="ph",ylab="counts")
hist(mydata$temperature,col="red",border="yellow",main="temperature Histogram",xlab="temperature",ylab="counts")
hist(mydata$humidity,col="blue",border="yellow",main="humidity Histogram",xlab="humidity",ylab="counts")
hist(mydata$rainfall,col="green",border="yellow",main="rainfall Histogram",xlab="humidity",ylab="counts")

##########boxplot simple of different parameters
boxplot(mydata$temperature,col="red",main="temperature")
boxplot(mydata$ph,col="blue",main="ph")
boxplot(mydata$rainfall,col="green",main="rainfall")
boxplot(mydata$humidity,col="yellow",main="humidity")

#ggplot boxplot
box_humidity<- ggplot(mydata, aes(x=label, y=humidity,fill=label)) + geom_boxplot()
box_humidity

############barplot of different parameters
barplot(mydata$temperature,col="yellow",main="temperature")
barplot(mydata$ph,col="blue",main="ph")
barplot(mydata$rainfall,col="green",main="rainfall")
barplot(mydata$humidity,col="yellow",main="humidity")

##crops density
mydata$label<-as.factor(mydata$label)
str(mydata$label)
cropId<-as.numeric(mydata$label)
cropId
my_temperature <-mydata$temperature
my_humidity <-mydata$humidity
plot(my_temperature,my_humidity,pch=cropId,from=-5,to=5,col=cropId,xlab="Temperature(degree celcius)",ylab="Humidity",main="Temperature Vs. Humidity",cex=0.8) #different shapes for crops each
abline(lm(my_temperature~my_humidity))
PCC <- cor(my_temperature, my_humidity) # Pearson's correlation coefficient
PCC <- round(PCC, 2) # round to the 2nd place after decimal point.
paste("R =", PCC)
text(0.1, 0.05, paste("R=", PCC),cex=0.2) # add text annotation.
legend("topleft", # specify the location of the legend
       levels(mydata$label), # specify the levels of species
       pch = 1:31, # specify three symbols used for the three species
       col = 1:31, # specify three colors for the three species
       box.lty=0 #removing border,
) 

#Ranking of Ranking of crops by ph Score
ggplot(data=mydata,aes(x=reorder(mydata$label,mydata$ph),y=ph)) + 
  geom_bar(stat ='identity',aes(fill=ph))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradientn(colors=heat.colors(31),name="crops ph level")+
  labs(title = 'Ranking of crops by ph Score',
       y='ph',x='crops')+ 
  geom_hline(yintercept = mean(mydata$ph),size = 1, color = 'blue')

#Ranking of Ranking of crops by rainfall
ggplot(data=mydata,aes(x=reorder(mydata$label,mydata$rainfall),y=rainfall)) + 
  geom_bar(stat ='identity',aes(fill=rainfall))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradient(name="crops rainfall level")+
  labs(title = 'Ranking of crops by rainfall',
       y='rainfall',x='crops')+ 
  geom_hline(yintercept = mean(mydata$rainfall),size = 1, color = 'green')

#Ranking of Ranking of crops by humidity
ggplot(data=mydata,aes(x=reorder(mydata$label,mydata$humidity ),y=humidity )) + 
  geom_bar(stat ='identity',aes(fill=humidity ))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradientn(colors=rainbow(31),name="crops humidity level")+
  labs(title = 'Ranking of crops by humidity ',
       y='humidity ',x='crops')+ 
  geom_hline(yintercept = mean(mydata$humidity ),size = 1, color = 'green')

#Ranking of Ranking of crops by temperature
x<-ggplot(data=mydata,aes(x=reorder(mydata$label,mydata$temperature),y=temperature )) + 
  geom_bar(stat ='identity',aes(fill=temperature ))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradientn(colors=topo.colors(31),name="crops humidity level")+
  labs(title = 'Ranking of crops by temperature ',
       y='temperature ',x='crops')+ 
  geom_hline(yintercept = mean(mydata$temperature ),size = 1, color = 'green')
x

#scatterplot others
temp_scatterplt<- ggplot(mydata,aes(label, temperature)) +
  geom_point() +
  labs(title = "Temperature",x="crop",y="temperature")
temp_scatterplt




