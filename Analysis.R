library(naniar)

setwd("/Users/Gerard/Desktop/MVA/Project/")
USAccidents <- read.csv("final_US_Accidents.csv")
str(USAccidents)


#Changing class of variables####
#Joining some of the similar weather conditions
USAccidents$Weather_Condition <- sub(" / Windy", "",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub(" Shower", "",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub(" Showers", "",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub("N/A Precipitation", "Rain",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub("Rains", "Rain",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub("Snows", "Snow",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub("Thunder and Hail", "Thunderstorms and Rain",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub("Showers in the Vicinity", "Shower",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub("Thunder in the Vicinity", "Thunder",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub("Light Freezing Fog", "Partial Fog",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub("Shallow Fog", "Partial Fog",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub("Patches of Fog", "Partial Fog",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub("Mist", "Partial Fog",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub(" T-Storm", "Thunderstorms",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub("T-Storm", "Thunderstorm",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub("Sleet", "Hail",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub("Ice Pellets", "Hail",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub("Wintry Mix", "Hail",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub("Heavy Drizzle", "Rain",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub("Snow and Hail", "Snow",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub("Light Freezing Drizzle", "Light Drizzle",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub("Light Freezing Rain", "Light Rain",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub("Widespread Dust", "Dust",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub("Mostly Cloudy", "Cloudy",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub("Fair", "Clear",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub("Scattered Clouds", "Partly Cloudy",USAccidents$Weather_Condition)

#Changing vars to factors
USAccidents$State <- as.factor(USAccidents$State)
USAccidents$City <- as.factor(USAccidents$City)
USAccidents$Weather_Condition <- as.factor(USAccidents$Weather_Condition)
USAccidents$Year <- as.factor(USAccidents$Year)
USAccidents$County <- as.factor(USAccidents$County)
USAccidents$Severity <- as.factor(USAccidents$Severity)

levels(USAccidents$Severity)

#Treating NA####
little_result <- mcar_test(USAccidents[,2:21])
