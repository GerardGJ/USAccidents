library(naniar)
library(impute)
library(stringi)
library(class)

setwd("/Users/Gerard/Desktop/MVA/Project/")
USAccidents <- read.csv("final_US_Accidents.csv")
USAccidents <- USAccidents[,2:21]
str(USAccidents)


#Changing class of variables####
#Joining some of the similar weather conditions
USAccidents$Weather_Condition <- sub(" / Windy", "",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub(" Shower", "",USAccidents$Weather_Condition)
USAccidents$Weather_Condition <- sub(" Showers", "",USAccidents$Weather_Condition)
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
USAccidents$Humidity... <- as.numeric(USAccidents$Humidity...)

levels(USAccidents$Weather_Condition)

#Treating NA####
little_result <- mcar_test(USAccidents) #p-val = 0 55 different patterns 

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

#City: We impute the values of the city with the county capitals
USAccidents$City <- as.character(USAccidents$City)
USAccidents[is.na(USAccidents$City),] #Los Angeles, Santa Barbara, Hamilton, Placer, Siskiyou
USAccidents$City[50666] <- "Los Angeles"
USAccidents$City[519148] <- "Santa Barbara"
USAccidents$City[557014] <- "Santa Barbara"
USAccidents$City[596759] <- "Santa Barbara"
USAccidents$City[598693] <- "Los Angeles"
USAccidents$City[799137] <- "Chattanooga"
USAccidents$City[954736] <- "Auburn"
USAccidents$City[988180] <- "Yreka"
USAccidents$City <- as.factor(USAccidents$City)


USAccidents$Month <- as.numeric(substr(USAccidents$Start_Time,6,7))
USAccidents$Season <- NA
USAccidents$Season[USAccidents$Month == 12 | USAccidents$Month == 1 | USAccidents$Month == 2] = "Winter"
USAccidents$Season[USAccidents$Month == 6 | USAccidents$Month == 7 | USAccidents$Month == 8] = "Summer"
USAccidents$Season[USAccidents$Month == 9 | USAccidents$Month == 10 | USAccidents$Month == 11] = "Autumn"
USAccidents$Season[USAccidents$Month == 3 | USAccidents$Month == 4 | USAccidents$Month == 5] = "Spring"

#As temperature is normal will impute its values randomly from a normal distribution with the same mean an st dev:
qqnorm(USAccidents$Temperature.F., pch = 1, frame = FALSE)
qqline(USAccidents$Temperature.F., col = "steelblue", lwd = 2)  

set.seed(123)
sum(is.na(USAccidents$Temperature.F.))
USAccidents$Temperature.F.[is.na(USAccidents$Temperature.F.)] <- rnorm(sum(is.na(USAccidents$Temperature.F.)),mean = mean(USAccidents$Temperature.F.,na.rm = T), sd = sd(USAccidents$Temperature.F.,na.rm = T))

#Now we will add the missing weather condition as unknown
USAccidents$Weather_Condition <- as.character(USAccidents$Weather_Condition)
USAccidents$Weather_Condition[is.na(USAccidents$Weather_Condition)] <- "Unknown"
USAccidents$Weather_Condition <- as.factor(USAccidents$Weather_Condition)
#Impute the reamining missing values with the average of its weather conditon, state and season:
Index_na_weatherCondition <- unique(c(which(is.na(USAccidents$Wind_Chill.F.) == TRUE), which(is.na(USAccidents$Visibility.mi.) == TRUE), which(is.na(USAccidents$Precipitation.in.) == TRUE, which(is.na(USAccidents$Humidity...) == TRUE), which(is.na(USAccidents$Pressure.in.) == TRUE))))
USAccidents_noUnknown <- USAccidents[USAccidents$Weather_Condition %in% USAccidents$Weather_Condition[Index_na_weatherCondition] & USAccidents$Weather_Condition != "Unknown",]

USAccidents_noUnknown$Season <- as.factor(USAccidents_noUnknown$Season)

  for(w in levels(USAccidents_noUnknown$Weather_Condition)){
    for(state in levels(USAccidents_noUnknown$State)){
      for(season in levels(USAccidents_noUnknown$Season)){
        index <- which(USAccidents_noUnknown$Season == season & USAccidents_noUnknown$State == state & USAccidents_noUnknown$Weather_Condition == w)
        season_df <- USAccidents_noUnknown[index,]
        if(sum(is.na(season_df$Wind_Chill.F.)) > 0){
          season_df$Wind_Chill.F.[is.na(season_df$Wind_Chill.F.)] <- mean(season_df$Wind_Chill.F., na.rm = TRUE) 
        }
        if(sum(is.na(season_df$Visibility.mi.)) > 0){
          season_df$Visibility.mi.[is.na(season_df$Visibility.mi.)] <- mean(season_df$Visibility.mi., na.rm = TRUE)
        }
        if(sum(is.na(season_df$Precipitation.in.)) > 0){
          season_df$Precipitation.in.[is.na(season_df$Precipitation.in.)] <- mean(season_df$Precipitation.in., na.rm = TRUE)
        }
        if(sum(is.na(season_df$Humidity...)) > 0){
          season_df$Humidity...[is.na(season_df$Humidity...)] <- mean(season_df$Humidity..., na.rm = TRUE)
        }
        if(sum(is.na(season_df$Pressure.in.)) > 0){
          season_df$Pressure.in.[is.na(season_df$Pressure.in.)] <- mean(season_df$Pressure.in., na.rm = TRUE)
        }
        USAccidents_noUnknown[index,] <- season_df
      }
    }
  }

USAccidents[Index_na_weatherCondition,] = USAccidents_noUnknown
