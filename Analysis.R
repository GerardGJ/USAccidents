library(naniar)
library(impute)

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

levels(USAccidents$Weather_Condition)

#Treating NA####
little_result <- mcar_test(USAccidents[,2:21]) #p-val = 0 55 different patterns 

summary(USAccidents) #First look at which variable have the most na and try to do a logical imputation

#Precipitation.in.
naprec <-USAccidents[is.na(USAccidents$Precipitation.in.),] #After looking at the na at Precipitation in we can see that when it doesn't rain sometimes it is not recorded
table(naprec$Weather_Condition) #From this table we can see that most of the na come from weather conditions with 0 rain and it would make sense to impute them with a 0 in the precipitation

naPrecipitationTo_0 <- c("Blowing Dust", "Clear", "Cloudy", "Dust", "Overcast", "Partly Cloudy", "Smoke")
precipitation_index <- which(is.na(USAccidents$Precipitation.in.) & USAccidents$Weather_Condition %in% naPrecipitationTo_0)
USAccidents$Precipitation.in.[precipitation_index] <- 0


#Wind_Chill.F. 
nawind <-USAccidents[is.na(USAccidents$Wind_Chill.F.),] #Observe the NA
table(nawind$Weather_Condition)
wind <- USAccidents[!is.na(USAccidents$Wind_Chill.F.),] #Observe the no na
table(wind$Weather_Condition)
  #We can see that good weather conditions have a lot of recordings, therefore we can impute them we will use the mean of each weather condition to impute them
goodWeather <- c("Clear","Cloudy","Partly Cloudy", "Overcast")
for(good  in goodWeather){
  good_df <- wind[which(wind$Weather_Condition == good),]
  meanwind <- mean(good_df$Wind_Chill.F.)
  print(meanwind)
  USAccidents$Wind_Chill.F.[which(is.na(USAccidents$Wind_Chill.F.) & USAccidents$Weather_Condition == good)] <- meanwind
}

little_result <- mcar_test(USAccidents[,2:21]) #p-val = 0, 54 different patterns


#Look at how many missing rows has each accident
summary(USAccidents)
na_count <- apply(USAccidents, 1, function(x) sum(is.na(x)))
table(na_count)

sum(table(na_count)[2:8])/1067434 #50606/1067434 = 0.04740902 
