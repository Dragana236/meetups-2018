##### Initial setup ######
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

setwd('D:/') # set your working directory to folder where your file is

# Load the file
weather <- read_delim('weather.txt', delim = '/')


##### Explore the data #####

# View the top
head(weather)

# View the bottom
tail(weather)

# View structure with glimpse
glimpse(weather)

# View a summary
summary(weather)

##### Cleaning the data #####

# Gather the columns
weather <- gather(weather, X1:X31, key = 'day', value = 'value', na.rm = TRUE)

# View the head
head(weather, 20)

# Remove the first column
weather <- weather %>%
  select(-1)

# View the head
head(weather)

# Spread the data
weather <- spread(weather, key = measure, value = value)

# View the head
head(weather)

# Separate column day
weather <- separate(weather, day, c('extra', 'day'), sep = 1)

# View the head
head(weather)

# Check the extra column
weather %>%
  count(extra)

# Remove extra column
weather <- weather %>%
  select(-3)

# View the head
head(weather)


##### Preparing data for analysis #####

# Convert year and month to characters
weather <- mutate_each(weather, funs(as.character), c(year, month))

# View the head
head(weather)


# Unite the year, month, and day columns
weather <- unite(weather, date, year, month, day, sep = '-')

# View the head
head(weather)


# Coerce date column to proper date format
weather <- weather %>%
  mutate(date = ymd(date))

# View the head
head(weather)


# Rearrange columns
weather <- select(weather, date, Events, CloudCover:WindDirDegrees)

# View the structure of weather
glimpse(weather)


# Convert characters to numeric
weather <- mutate_each(weather, funs(as.numeric), CloudCover:WindDirDegrees)

# Look at result
glimpse(weather)

##### Inspecting missing and suspected values #####

# Count missing values
sum(is.na(weather))

# Find missing values
summary(weather)

# Check missing values for Events column
sum(is.na(weather$Events))

# Find indices of NAs in Max.Gust.SpeedMPH
weather %>%
  filter(is.na(Max.Gust.SpeedMPH))

# Get index of outlier
ind <- which(weather$Max.Humidity == 1000)

# Change 1000 to 100
weather$Max.Humidity[ind] <- 100


# Inspect one more time
head(weather)

# Write full code in one row
weather_full <- weather %>%
  gather(X1:X31, key = 'day', value = 'value', na.rm = TRUE) %>%
  select(-1) %>%
  spread(key = measure, value = value) %>%
  separate(day, c('extra', 'day'), sep = 1) %>%
  select(-3) %>%
  mutate_each(funs(as.character), c(year, month)) %>%
  unite(date, year, month, day, sep = '-') %>%
  mutate(date = ymd(date)) %>%
  select(date, Events, CloudCover:WindDirDegrees) %>%
  mutate_each(funs(as.numeric), CloudCover:WindDirDegrees)




