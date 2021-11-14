# Overview

#### Cyclistic is a fictional bike-share company Chicago that has 5,284 bikes that can be rented from
#### or returned to 692 stations across Chicago. The company started in 2016 and this case study will be
#### looking at the user ride data from 2019. 

# Business Task

#### Explore how customers (pay as you go) users, use the bike share service differently from
#### subscribers (monthly subscription) users.

# Data Source

#### The data was provided by the content creators of the Google Data Analytics business specialization
#### offered on Coursera. The data will be cleaned and analyzed using RStudio and visualized using Tableau.
#### Raw data can be accessed through the link below.
#### https://divvy-tripdata.s3.amazonaws.com/index.html


#### Loading the required packages for analysis
library(dplyr)
library(tidyverse)
library(lubridate)


#### Set working directory to file path where data is stored
setwd("~/Education/Google_Data_Analytics_Certificate/Course_8_Capstone/Bike_Share_Data")


#### The data was provided in quarterly files. Each data set contains 12 variables with the number
#### of observations ranging between between 365,069 and 1,640,718 observations. The data collected
#### is consistent among all four files but the variable names, data types, and file formats have
#### inconsistencies

#### Upload quarterly data sets into Rstudio
Trips_Q1 <- read.csv("Divvy_Trips_2019_Q1.csv")
Trips_Q2 <- read.delim2("Divvy_Trips_2019_Q2", header = TRUE, sep = ",", dec = ".")
Trips_Q3 <- read.csv("Divvy_Trips_2019_Q3.csv")
Trips_Q4 <- read.csv("Divvy_Trips_2019_Q4.csv")

# Data cleaning and manipulation
colnames(Trips_Q1) <- c("trip_id", "start_time", "end_time", "bike_id", "trip_duration_seconds",
                        "from_station_id", "from_station_name", "to_station_id", "to_station_name",
                        "user_type", "gender", "birth_year")

colnames(Trips_Q2) <- c("trip_id", "start_time", "end_time", "bike_id", "trip_duration_seconds",
                        "from_station_id", "from_station_name", "to_station_id", "to_station_name",
                        "user_type", "gender", "birth_year")

colnames(Trips_Q3) <- c("trip_id", "start_time", "end_time", "bike_id", "trip_duration_seconds",
                        "from_station_id", "from_station_name", "to_station_id", "to_station_name",
                        "user_type", "gender", "birth_year")

colnames(Trips_Q4) <- c("trip_id", "start_time", "end_time", "bike_id", "trip_duration_seconds",
                        "from_station_id", "from_station_name", "to_station_id", "to_station_name",
                        "user_type", "gender", "birth_year")

#### Add a column in each table called trip_date that converts that start date and time provided
#### to a calendar date without a time stamp
Trips_Q1$trip_date <- as.Date(Trips_Q1$start_time, format = "%m/%d/%y")
Trips_Q2$trip_date <- as.Date(Trips_Q2$start_time)
Trips_Q3$trip_date <- as.Date(Trips_Q3$start_time)
Trips_Q4$trip_date <- as.Date(Trips_Q4$start_time)

#### Combine data set for each quarter into one data set named Trips_2019
Trips_2019 <- rbind(Trips_Q1, Trips_Q2, Trips_Q3, Trips_Q4)

#### Data is uploaded and combined into one data frame for the year 2019. It has 3.8 million
#### observations and 13 different variables. 

#### A preview can be found below
head(Trips_2019)

#### Convert the trip_duration_seconds column to an integer data type
#### Create a new column in Trips_2019 that converts trip_duration_seconds into trip_duration_minutes
#### Convert the trip_duration_minutes column to an integer data type
Trips_2019$trip_duration_seconds <- as.integer(Trips_2019$trip_duration_seconds)
Trips_2019["trip_duration_minutes"] <- Trips_2019$trip_duration_seconds / 60
Trips_2019$trip_duration_minutes <- as.integer(Trips_2019$trip_duration_minutes)


#### Create a new column in Trips_2019 called day_of_week based off the date the trip started
#### Convert the day_of_week column to a factor variable type
Trips_2019["day_of_week"] <- weekdays(Trips_2019$trip_date)
Trips_2019$day_of_week <- as.factor(Trips_2019$day_of_week)

#### Updated preview of Trips_2019 with new columns included
head(Trips_2019)

#### Summary statistics for Trips_2019
summary(Trips_2019)

#### Data type information for Trips_2019
str(Trips_2019)

#### The cleaned and transformed data set will be saved locally and uploaded to Tableau
#### to create the visualizations that will be displayed later in this document.
write.csv(Trips_2019, "Trips_2019.csv")

# Analysis of usage trends between customer and subscribers

### Average trip_duration_minutes by user type
Average_Trip_Length <- data.frame(aggregate(Trips_2019$trip_duration_minutes ~ Trips_2019$user_type, FUN = mean))
colnames(Average_Trip_Length) <- c("user_type", "average_trip_duration_minutes")
Average_Trip_Length$average_trip_duration_minutes <- as.integer(Average_Trip_Length$average_trip_duration_minutes)
head(Average_Trip_Length)

### Average birth_year by user type
Average_Birth_Year <- data.frame(aggregate(Trips_2019$birth_year ~ Trips_2019$user_type, FUN = mean))
colnames(Average_Birth_Year) <- c("user_type", "average_birth_year")
Average_Birth_Year$average_birth_year <- as.integer(Average_Birth_Year$average_birth_year)
head(Average_Birth_Year)

# Day of the week usage by user type

#### Create a data frame for user type and day of the week
Trips_2019$day_of_week <- as.factor(Trips_2019$day_of_week)
Top_Days_Of_Week <- data.frame(Trips_2019$user_type, Trips_2019$day_of_week)
colnames(Top_Days_Of_Week) <- c("user_type", "day_of_week")

### Top days of the week for subscribers
Top_Days_Of_Week_Subscribers <- Top_Days_Of_Week %>%
  filter(user_type == "Subscriber") %>%
  count(day_of_week, sort = TRUE, name = "num_trips")
head(Top_Days_Of_Week_Subscribers, n = 7)


### Top days of the week for customers
Top_Days_Of_Week_Customers <- Top_Days_Of_Week %>%
  filter(user_type == "Customer") %>%
  count(day_of_week, sort = TRUE, name = "num_trips")
head(Top_Days_Of_Week_Customers, n = 7)


# Station usage by user type

#### Create data frame for users and the start station name
Top_Stations <- data.frame(Trips_2019$user_type, Trips_2019$from_station_name)
colnames(Top_Stations) <- c("user_type", "station_name")
Top_Stations <- na.omit(Top_Stations)

### Most popular stations for subscribers
Top_Stations_Subscribers <- Top_Stations %>%
  filter(user_type == "Subscriber") %>%
  count(station_name, sort = TRUE, name = "num_trips") %>%
  head(Top_Stations, n = 10)
head(Top_Stations_Subscribers, n = 10)


### Most popular stations for customers
Top_Stations_Customers <- Top_Stations %>%
  filter(user_type == "Customer") %>%
  count(station_name, sort = TRUE, name = "num_trips") %>%
  head(Top_Stations, n = 10)
head(Top_Stations_Customers, n = 10)


# Key Takeaways

#### Subscribers (monthly membership riders) on average are slightly older and have slightly longer
#### ride lengths than customers (pay as you go riders)

#### Subscribers tend to use the service more during the week with Tuesday-Thursday being the peak
#### ride days and customers tend to use the service more on the weekends with Saturday and Sunday
#### being the peak ride days

#### Subscribers have a wider distribution of stations used while a significant number of rides comes
#### from a small number of stations for customers












