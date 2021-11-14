# Loading tidyverse package required for analysis
library(tidyverse)
library(lubridate)

## Data access from Minnesota DNR. Raw data can be accessed through the link below 
## https://www.dnr.state.mn.us/climate/twin_cities/listings.html

## Set working directory to location where data is stored
setwd("~/Eric Files/minnesota_weather")

## Upload temperature data by decade into Rstudio
Decades1880s <- read_csv("1880_1889.csv")
Decades1890s <- read_csv("1890_1899.csv")
Decades1900s <- read_csv("1900_1909.csv")
Decades1910s <- read_csv("1910_1919.csv")
Decades1920s <- read_csv("1920_1929.csv")
Decades1930s <- read_csv("1930_1939.csv")
Decades1940s <- read_csv("1940_1949.csv")
Decades1950s <- read_csv("1950_1959.csv")
Decades1960s <- read_csv("1960_1969.csv")
Decades1970s <- read_csv("1970_1979.csv")
Decades1980s <- read_csv("1980_1989.csv")
Decades1990s <- read_csv("1990_1999.csv")
Decades2000s <- read_csv("2000_2009.csv")
Decades2010s <- read_csv("2010_2021.csv")

## Combine the data from each decade to create Twin Cities Weather
TwinCitiesWeather <- bind_rows(Decades1880s, Decades1890s, Decades1900s, Decades1910s,
                               Decades1920s, Decades1930s, Decades1940s, Decades1950s,
                               Decades1960s, Decades1970s, Decades1980s, Decades1990s,
                               Decades2000s, Decades2010s)

## Rename the columns in Twin Cities Weather
TwinCitiesWeather <- rename(TwinCitiesWeather, date = "Date",
                            max_temp_f = "Maximum Temperature degrees (F)",
                            min_temp_f = "Minimum Temperature degrees (F)",
                            precip_in = "Precipitation (inches)",
                            snow_in = "Snow (inches)",
                            snow_depth_in = "Snow Depth (inches)")

## Drop columns keeping only the date and temperature data
TwinCitiesWeather <- select(TwinCitiesWeather, date, max_temp_f, min_temp_f)

## Create new columns for the year, month, day of the month, and day of the week for each observation
TwinCitiesWeather <- mutate(TwinCitiesWeather, year = year(date),
                            month = month(date, label = TRUE, abbr = FALSE),
                            day_of_month = day(date),
                            day_of_week = weekdays(date))

## Create a new column called decade based on the information from the year
TwinCitiesWeather <- TwinCitiesWeather %>%
  mutate(starter = substr(year, 1, 3),
         closer = "0s",
         decade = paste0(starter, closer)) %>%
  select(-starter, -closer)

## Change the data type for month, day of the week and decade to factor
TwinCitiesWeather$month <- as.factor(TwinCitiesWeather$month)
TwinCitiesWeather$day_of_week <- as.factor(TwinCitiesWeather$day_of_week)
TwinCitiesWeather$decade <- as.factor(TwinCitiesWeather$decade)


## View data type information
str(TwinCitiesWeather)
## View first 10 rows of data
head(TwinCitiesWeather, n = 10)
## Summary statistics about data
summary(TwinCitiesWeather)


## Average temperature by month and year
AverageTemperatures <- TwinCitiesWeather %>%
  group_by(year, month) %>%
  summarise(average_high_f = round(mean(max_temp_f), 0),
            average_low_f = round(mean(min_temp_f), 0))


## Average temperature by decade
AverageTemperatureDecade <- TwinCitiesWeather %>%
  group_by(decade) %>%
  summarise(average_high = round(mean(max_temp_f), 2),
            average_low = round(mean(min_temp_f, na.rm = TRUE), 2))

## Average temperature by decade and month
AverageTemperatureDecadeMonth <- TwinCitiesWeather %>%
  group_by(decade, month) %>%
  summarise(average_high = round(mean(max_temp_f), 2),
            average_low = round(mean(min_temp_f, na.rm = TRUE), 2))

## Save Average Temperatures table as a csv to be used to create a visual
write.csv(TwinCitiesWeather, "twin_cities_weather.csv")
write.csv(AverageTemperatures, "average_temperatures_twin_cities.csv")
write.csv(AverageTemperatureDecade, "average_temperatures_decade.csv")
write.csv(AverageTemperatureDecadeMonth, "average_temperatures_decade_month.csv")
