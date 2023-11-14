library(ggplot2)

# Average Ride Length by Hour of day per User Type
ggplot(all_data2, aes(x = as.integer(format(start_time, "%H")), 
                           y = ride_length, color = usertype)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_smooth() +
  labs(x = "Hour of the Day", y = "Average Ride Length (seconds)") +
  ggtitle("Average Ride Length by Hour of the Day")

# Average Ride Length by Hour Between Weekend and Weekdays (Casual and Member Riders):
ggplot(all_data2 %>% filter(!day_of_week %in% c("Saturday", "Sunday")), 
       aes(x = as.integer(format(start_time, "%H")), y = ride_length, color = usertype)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_smooth() +
  labs(x = "Hour of the Day", y = "Average Ride Length (seconds)") +
  ggtitle("Average Ride Length by Hour on Weekdays")

ggplot(all_data2 %>% filter(day_of_week %in% c("Saturday", "Sunday")), 
       aes(x = as.integer(format(start_time, "%H")), y = ride_length, color = usertype)) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_smooth() +
  labs(x = "Hour of the Day", y = "Average Ride Length (seconds)") +
  ggtitle("Average Ride Length by Hour on Weekends")

# Average Ride Length by Month
ggplot(all_data2, aes(x = as.integer(format(start_time, "%d")), 
                      y = ride_length, color = usertype)) +
  geom_point(stat = "summary", fun.y = "mean", position = "dodge") +
  geom_smooth() +
  labs(x = "Day of the Month", y = "Average Ride Length (seconds)") +
  ggtitle("Average Ride Length by Day of the Month")

# Total Number of Rides by Each User
ggplot(all_data2, aes(x = usertype, fill = usertype)) +
  geom_bar() +
  labs(x = "User Type", y = "Total Number of Rides") +
  ggtitle("Total Number of Rides by Each User")

# Calculate total number of rides by each user type
total_rides <- all_data2 %>%
  group_by(usertype) %>%
  summarise(total_rides = n())
ggplot(total_rides, aes(x = "", y = total_rides, fill = usertype, label = total_rides)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(position = position_stack(vjust = 0.5), color = "white", size = 4) +
  coord_polar("y") +
  labs(title = "Total Number of Rides by User Type") +
  theme_void()

# Number of Rides by Hour of Day
ggplot(all_data2, aes(x = as.integer(format(start_time, "%H")), fill = usertype)) +
  geom_bar(position = "dodge") +
  labs(x = "Hour of the Day", y = "Number of Rides") +
  ggtitle("Number of Rides by Hour of Day")

# Number of Rides by Weekends and Weekdays
ggplot(all_data2, aes(x = day_of_week, fill = usertype)) +
  geom_bar(position = "dodge") +
  scale_x_discrete(labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  labs(x = "Day of the Week", y = "Number of Rides") +
  ggtitle("Number of Rides by Weekdays and Weekends")

# Number of Rides by Day of the Month
ggplot(all_data2, aes(x = as.integer(format(start_time, "%d")), fill = usertype)) +
  geom_bar(position = "dodge") +
  labs(x = "Day of the Month", y = "Number of Rides") +
  ggtitle("Number of Rides by Day of the Month")


# Convert month to factor with correct order
all_data2$month <- factor(all_data2$month, levels = c("10", "11", "12", "01", "02", "03"),
                          labels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar"))
# Create a grid of plots for each month
ggplot(all_data2, aes(x = as.integer(format(start_time, "%d")), fill = usertype)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ month, scales = "free_x", ncol = 2) +
  labs(x = "Day of the Month", y = "Number of Rides") +
  ggtitle("Number of Rides by Day of the Month")


# Calculate bike type usage per user type
bike_type_usage_table <- all_data2 %>%
  group_by(bike_type, usertype) %>%
  summarise(total_rides = n()) %>%
  spread(usertype, total_rides, fill = 0)

# Display the table in the console
print("Bike Type Usage - Figures:")
print(bike_type_usage_table)

# Calculate the total number of unique stations
total_stations <- length(unique(c(all_data2$start_station_name, all_data2$end_station_name)))
# Display the total number of stations
cat("Total Number of Stations:", total_stations, "\n")


# Calculate the total number of rides per station
station_visits <- all_data2 %>%
  group_by(station_name = start_station_name) %>%
  summarise(total_rides = n()) %>%
  arrange(desc(total_rides))
# Display the top 10 most visited stations
cat("Top 10 Most Visited Stations:\n")
print(head(station_visits, 10))


# Calculate the total number of rides per station and user type
station_visits_user_type <- all_data2 %>%
  group_by(station_name = start_station_name, usertype) %>%
  summarise(total_rides = n()) %>%
  arrange(usertype, desc(total_rides))

# Combine the top 10 most visited stations for casual and member users
top_stations_combined <- station_visits_user_type %>%
  group_by(usertype) %>%
  slice_max(order_by = total_rides, n = 10)

# Display the combined top 10 most visited stations
cat("Combined Top 10 Most Visited Stations for Casual and Member Users:\n")
print(top_stations_combined)


# library(ggmap)
# # Replace 'latitude' and 'longitude' with the actual column names in your dataset
# latitude_col <- "start_lat"
# longitude_col <- "start_lon"
# 
# # Extract the top 10 most visited stations for casual users
# top_casual_stations <- filter(station_visits_user_type, usertype == "casual") %>%
#   arrange(desc(total_rides)) %>%
#   head(10)
# 
# # Extract the top 10 most visited stations for member users
# top_member_stations <- filter(station_visits_user_type, usertype == "member") %>%
#   arrange(desc(total_rides)) %>%
#   head(10)
# 
# # Get the map background using ggmap
# map <- get_map(location = c(lon = mean(top_casual_stations[[longitude_col]]),
#                             lat = mean(top_casual_stations[[latitude_col]])),
#                zoom = 13)
# 
# # Create a ggplot object with the map background
# ggmap(map) +
#   geom_point(data = top_casual_stations, aes(x = get(longitude_col), y = get(latitude_col), color = "Casual"),
#              size = 3, alpha = 0.7) +
#   geom_point(data = top_member_stations, aes(x = get(longitude_col), y = get(latitude_col), color = "Member"),
#              size = 3, alpha = 0.7) +
#   scale_color_manual(values = c("Casual" = "blue", "Member" = "green")) +
#   labs(title = "Top 10 Most Visited Stations by User Type")






