
# install.packages("tidyverse")
# install.packages("lubridate")
library(tidyverse) #helps wrangle data
library(lubridate) #helps wrangle date attributes
library(ggplot2) #helps visualize data
getwd() #displays your working directory
setwd("C:/Users/Jnn/Desktop/Cyclistic") #sets your working directory to simplify calls to data

# STEP 1: COLLECT DATA
#=====================
# Upload Divvy datasets (csv files) here
oct <- read_csv("202210-divvy-tripdata.csv")
nov <- read_csv("202211-divvy-tripdata.csv")
dec <- read_csv("202212-divvy-tripdata.csv")
jan <- read_csv("202301-divvy-tripdata.csv")
feb <- read_csv("202302-divvy-tripdata.csv")
mar <- read_csv("202303-divvy-tripdata.csv")


# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need
#to match perfectly before we can use a command to join them into one file

colnames(oct)
colnames(nov)
colnames(dec)
colnames(jan)
colnames(feb)
colnames(mar)

# Stack individual month's data frames into one big data frame
all_data <- bind_rows(oct, nov, dec, jan, feb, mar)
rm(oct, nov, dec, jan, feb, mar)

# Inspect the dataframes and look for incongruencies
str(all_data)

# Rename some columns to make them consistent and easier to read
(all_data <- rename(all_data,
                    bike_type = rideable_type,
                    start_time = started_at,
                    end_time = ended_at,
                    usertype = member_casual))

# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ...
# before completing these operations we could only aggregate at the ride level

all_data$date <- as.Date(all_data$start_time) #The default format is yyyy-mm-dd
all_data$month <- format(as.Date(all_data$date), "%m")
all_data$day <- format(as.Date(all_data$date), "%d")
all_data$year <- format(as.Date(all_data$date), "%Y")
all_data$day_of_week <- format(as.Date(all_data$date), "%A")

# Add a "ride_length" column calculation to the dataset (in seconds)
all_data$ride_length <- difftime(all_data$end_time, all_data$start_time)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_data$ride_length)
all_data$ride_length <- as.numeric(as.character(all_data$ride_length))
is.numeric(all_data$ride_length) # Check that it's been converted

# Inspect the new table that has been created
colnames(all_data)  #List of column names
nrow(all_data)  #How many rows are in data frame?
dim(all_data)  #Dimensions of the data frame? Row/cols
head(all_data)  #See the first 6 rows of data frame.  Also tail(qs_raw)
str(all_data)  #See list of columns and data types (numeric, character, etc)
summary(all_data)  #Statistical summary of data. Mainly for numerics

# Check how many observations fall under each columns
table(all_data$usertype) # Two - casual & member
table(all_data$bike_type) #Three - classic, docked, electric

# Export the original dataset to a directory before removing bad data
write.csv(all_data, file = "C:/Users/Jnn/Desktop/Cyclistic/all_data.csv")

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (2) since data is being removed
all_data2 <- all_data[!(all_data$start_station_name == "HQ QR" | 
                          all_data$ride_length < 60 | 
                          all_data$ride_length > 172800), ]


# Remove main data frame from environment to save memory
rm(all_data)

# Review new dataset
summary(all_data2)

# Check for the number of empty/null values in each column
colSums(is.na(all_data2))
# Delete empty/null values
all_data2 <- na.omit(all_data2)

# Remove unwanted columns from the data frames

all_data2 <- all_data2[, !colnames(all_data2) %in% c(
  "ride_id", "start_station_id", "end_station_id", "end_time")]



# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_data2$ride_length) #straight average (total ride length / rides)
median(all_data2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_data2$ride_length) #longest ride
min(all_data2$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_data2$ride_length)

# Compare members and casual users
aggregate(all_data2$ride_length ~ all_data2$usertype, FUN = mean)
aggregate(all_data2$ride_length ~ all_data2$usertype, FUN = median)
aggregate(all_data2$ride_length ~ all_data2$usertype, FUN = max)
aggregate(all_data2$ride_length ~ all_data2$usertype, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_data2$ride_length ~ all_data2$usertype + all_data2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_data2$day_of_week <- ordered(all_data2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_data2$ride_length ~ all_data2$usertype + all_data2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_data2 %>%
  mutate(weekday = wday(start_time, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(usertype, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n() #calculates the number of rides and average duration
            ,average_duration = mean(ride_length)) %>% # calculates the average duration
  arrange(usertype, weekday)

# Let's visualize the number of rides by rider type
all_data2 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(usertype, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = usertype)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_data2 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(usertype, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = usertype)) +
  geom_col(position = "dodge")

# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
counts <- aggregate(all_data2$ride_length ~ all_data2$usertype + all_data2$day_of_week, FUN = mean)
write.csv(counts, file = 'C:/Users/Jnn/Desktop/Cyclistic/avg_ride_length.csv')
