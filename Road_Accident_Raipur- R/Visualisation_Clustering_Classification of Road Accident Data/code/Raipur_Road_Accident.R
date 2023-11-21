library(lubridate)
library(scales)
library(ggplot2)
library(dplyr)
library(caTools)
library(rpart.plot)
library(randomForest)
library(caret)
library(readr)
library(urca)
library(stats)

# load the data into R

#Load data
ds <- read.csv(file.choose())# Select file Raipur_Accident_Data_Visua.csv


#Vehicle Road Accident
count_Vh <- table(ds$Vehicle)

barplot(count_Vh, main="Vehicle wise accidents", las=2, ylab = "No of Accidents",col="blue")


#National, State highway or Other road type Road Accident
count_RT <- table(ds$Road.Type)

barplot(count_RT, main="Road wise accidents", ylab = "No of Accidents",col="lightblue")


#Urban/Rural wise Road Accident
count_Place <- table(ds$Place)
barplot(count_Place, main="Urban/Rural wise road accidents", ylab = "No of Accidents",col="gray")

#Police Station wise Road Accident
count_PS <- table(ds$ï..Police_Station)

barplot(count_PS, main="Police Station wise road accidents",las=2, ylab = "No of Accidents",col="lightyellow",cex.names = 0.6)


# data manipulation
ds$Date = as.POSIXct(ds$Date, format="%m/%d/%Y")
ds$Year = year(ds$Date) 
ds$Month = month(ds$Date)
ds$Day = day(ds$Date)
ds$Time = as.POSIXct(ds$Time, format="%H:%M")
ds$Hour = hour(ds$Time)
ds$Minute = minute(ds$Time)

# time series
install.packages("dplyr")
library(dplyr)
daily = group_by(ds, Date)
day_counts = summarise(daily, count = n())

library(ggplot2)
require(ggplot2)
#ggplot(day_counts, aes(x = Date, y = count)) + geom_point(colour = "red") + 
#  geom_line(colour = "red", size = 4) + 
#  theme_light(base_size = 24) + xlab("Date") + ylab("Count of indicents") + 
#  ggtitle("The number of incidents in Raipur 2019") + 
#  theme(plot.title=element_text(size=24))

# average counts per hour
daily_group = group_by(ds, Month, Day, Hour)
day_hour_counts = summarise(daily_group, count = n())
hour_group = group_by(day_hour_counts, Hour)
hour_avg_counts = summarise(hour_group, count = mean(count))

# time series: average counts by time of day
ggplot(hour_avg_counts, aes(x = Hour, y = count)) + geom_point(colour = "red") + 
  geom_line(colour = "red", size = 1.5) + 
  theme_light(base_size = 12) + xlab("Time of day") + ylab("Count of incidents") + 
  scale_x_continuous(breaks=c(0:23)) + 
  ggtitle("The average number of incidents by time of day") + 
  theme(plot.title = element_text(size = 16))

# histogram: average counts by time of day
ggplot(hour_avg_counts, aes(x = Hour, y = count)) + 
  geom_bar(position = "dodge", stat = "identity", fill = "#FF9933") +
  theme_light(base_size = 12) + labs(x = "Time of day", y = "Count of Incidents") + 
  scale_x_continuous(breaks=c(0:23)) + 
  ggtitle("The average number of incident by time of day") + 
  theme(plot.title = element_text(size = 16))

hourly_group = group_by(ds, Road.Type, Month, Day, Hour)
Location_day_hour_counts = summarise(hourly_group, count = n())

Location_hourly_group = group_by(Location_day_hour_counts, Road.Type, Hour)
Location_hour_avg_counts = summarise(Location_hourly_group, count = mean(count))

ggplot(Location_hour_avg_counts, aes(x = Hour, y = Road.Type)) + 
  geom_tile(aes(fill = count)) + 
  scale_fill_gradient(name = "Average counts", low = "white", high = "red") +
  scale_x_continuous(breaks=c(0:23)) + 
  theme(axis.title.y = element_blank()) + theme_light(base_size = 10) + 
  theme(plot.title = element_text(size=16)) + 
  ggtitle("The number of incidents: Hour vs. Location")


hourly_group = group_by(ds, ï..Police_Station, Month, Day, Hour)
Police_Station_day_hour_counts = summarise(hourly_group, count = n())

Police_Station_hourly_group = group_by(Police_Station_day_hour_counts, ï..Police_Station, Hour)
Police_Station_hour_avg_counts = summarise(Police_Station_hourly_group, count = mean(count))

ggplot(Police_Station_hour_avg_counts, aes(x = Hour, y = ï..Police_Station)) + 
  geom_tile(aes(fill = count)) + 
  scale_fill_gradient(name = "Average counts", low = "white", high = "red") +
  scale_x_continuous(breaks=c(0:23)) + 
  theme(axis.title.y = element_blank()) + theme_light(base_size = 10) + 
  theme(plot.title = element_text(size = 16)) + 
  ggtitle("The number of incidents: Hour vs. Police_Station") 

Location_group = group_by(ds, Month, Day, ï..Police_Station, Road.Type)
day_Police_Station_Location_counts = summarise(Location_group, count = n())
Police_Station_Location_group = group_by(day_Police_Station_Location_counts,ï..Police_Station, Road.Type)
Police_Station_Location_avg_counts = summarise(Police_Station_Location_group, count = mean(count))

ggplot(Police_Station_Location_avg_counts, aes(x = ï..Police_Station, y =Road.Type)) + 
  geom_tile(aes(fill = count)) + 
  scale_fill_gradient(name="Average counts", low="white", high="red") +
  theme(axis.title.y = element_blank()) + theme_light(base_size = 10) + 
  theme(plot.title = element_text(size = 16)) + 
  ggtitle("The number of incidents: Police_Station vs. Location") + 
  theme(axis.text.x = element_text(angle = 45,size = 8, vjust = 0.5)) 

 
# scatter plot
ggplot(ds, aes(x = Longitude, y = Latitude)) + geom_point(aes(colour = factor(ï..Police_Station)), size = 1.25) + 
  theme_light(base_size = 10) + xlab("Longitude") + ylab("Latitude") +
  ggtitle("Police Stations on Map") + theme(plot.title=element_text(size = 16))


ggplot(ds, aes(x = Longitude, y = Latitude)) + geom_point(aes(colour = factor(Road.Type)), size = 1.25) + 
  theme_light(base_size = 10) + xlab("Longitude") + ylab("Latitude") +
  ggtitle("Accident location where held in Raipur on Map") + theme(plot.title=element_text(size = 16))




################### VISUALIZATION ---MAPPING----########################


# insert map layers to base map

library(leaflet)
library(readr)



# Base Map template include passing longitude and latitude information along with adding pop-ups4, 

m <- leaflet(ds) %>%
  addTiles() %>%  # Add default Open Street Map map tiles
  addCircleMarkers(radius = 2, color = "darkblue",
                   lng = ~Longitude, lat = ~Latitude, popup= ~paste('<b>', `ï..Police_Station`)) %>%
  addProviderTiles(providers$BasemapAT.grau) 

# BasemapAT.grau provides locations with names in local languages

m  # Print the map

m <- leaflet(ds) %>%
  addTiles() %>%  # Add default Open Street Map map tiles
  addMarkers(lng = ~Longitude, lat = ~Latitude, popup= ~paste('<b>', `ï..Police_Station`))
m  # Print the map

colnames(ds)

getcolor <- function(ds) {
  sapply(ds$Road.Type, function(Road.Type) {
    if (Road.Type == 'O'){ 
      'red' 
    } else if (Road.Type == 'NH') {
      'gray'
    } else {
      'blue'
    } })
}

icons <- awesomeIcons(
  icon = ' ',
  library = 'ion',
  markerColor = getcolor(ds)
)


m <- leaflet(ds) %>%
  addTiles() %>%  
  addCircleMarkers(radius = 8, color = getcolor(ds),
                   lng = ~Longitude, lat = ~Latitude, popup= ~paste('<b>', `ï..Police_Station)) %>%
  addProviderTiles(providers$BasemapAT.grau) 

m  # Print the map



###########Split data######################

split <- sample.split(ds, SplitRatio = 0.70)# we spliting the data in train test from 70% and 30%.
split


train <- subset(ds, split == "TRUE")
test <- subset(ds, split == "FALSE")

str(train)# check the structure of train
str(test)# check the structure of test

#####################LR######################

#Train model with logistic regression using glm function for ACT_279.304.A
logit_model <- glm(ACT_279.304.A~Latitude+Longitude, data = train, family = "binomial")
logit_model

summary(logit_model)



#Predict test data based on trained model
fitted_result <- predict(logit_model, test, type = "response")
fitted_result

fitted_result <- ifelse(fitted_result> 0.5,1,0)
fitted_result #Predict Result
test$ACT_279.304.A #Actual Result



#Evaluate Model Accuracy using with Confusion Matrix 
table(test$ACT_279.304.A, fitted_result)
MissclassError <- mean(fitted_result != test$ACT_279.304.A)
print(paste('Accuracy = ', 1-MissclassError))

#######################Decision Tree############################
#Train model using rpart
DTModel <- rpart(ACT_279.304.A~Latitude+Longitude, data= train, method = "class")


#predict on test data
fitted_value <- predict(DTModel, newdata = test, type = "class")

#Evaluate the model accuarcy
table(test$ACT_279.304.A, fitted_value)
misClassError <- mean(fitted_value != test$ACT_279.304.A)
print(paste('Accuracy =', 1-misClassError))




#################### Random Forest #######################

ds$ACT_279.304.A <- as.factor(ds$ACT_279.304.A)
#Variable selection
str(ds)

# Splitting Data in Training and Testing
index = sample(2,nrow(ds), replace = TRUE, prob=c(0.7,0.3))

#Training Data
Training = ds[index==1,]

#Testing Data
Testing = ds[index==2,]

# Random Forest model

RF = randomForest(ACT_279.304.A~.,data = Training)

# Evaluating Model Accuracy
ACT_279.304.A_Pred = predict(RF, Testing)
Testing$ACT_279.304.A_Pred = ACT_279.304.A_Pred
View(Testing)

# Confusion Matrix
 
CFM = table(Testing$ACT_279.304.A, Testing$ACT_279.304.A_Pred)
CFM

Mod_Accu = sum(diag(CFM)/sum(CFM))
Mod_Accu
