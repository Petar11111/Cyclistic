# Cyclistic
Data Analysis project 

# installing packages 
install.packages("tidyverse")
install.packages("geosphere")
install.packages("magrittr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("ggmap")
install.packages("RColorBrewer")
install.packages("leaflet")
install.packages("dplyr")
install.packages("htmlwidgets")

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(geosphere)
library(magrittr)
library(scales)
library(ggmap)
library(RColorBrewer)
library(leaflet)
library(dplyr)
library(htmlwidgets)

# import files 
csv_files <- c(
  "202112-divvy-tripdata.csv", "202201-divvy-tripdata.csv", "202202-divvy-tripdata.csv",
  "202203-divvy-tripdata.csv", "202204-divvy-tripdata.csv", "202205-divvy-tripdata.csv",
  "202206-divvy-tripdata.csv", "202207-divvy-tripdata.csv", "202208-divvy-tripdata.csv",
  "202209-divvy-publictripdata.csv", "202210-divvy-tripdata.csv", "202211-divvy-tripdata.csv"
)

# Read and combine all CSV files into one data frame using tidyverse functions
df <- map_dfr(csv_files, read_csv)


# Specify the file path where you want to save the CSV file
file_path <- "Cyclistic.csv"

# Save the dataframe as a CSV file
write.csv(df, file = file_path, row.names = FALSE)

# Optionally, you can set 'row.names = FALSE' to exclude row numbers in the CSV file





# Rename columns
df <- df %>%
  rename(bike_type = rideable_type,
         start_time = started_at,
         end_time = ended_at,
         user_type = member_casual)

# Create rent_duration column in seconds
df$rent_duration <- difftime(df$end_time, df$start_time, units = "secs")

# Create columns for year, month, day, and day of the week
#df$year <- year(df$start_time)
df$month <- month(df$start_time, label = TRUE)
df$month <- month.name[match(df$month, month.abb)]
#df$day <- day(df$start_time)
df$day_of_week <- weekdays(as.Date(df$start_time))

# Drop unnecessary columns
df <- df %>%
  select(-ride_id, -start_station_id, -end_station_id)

# Remove docked bikes, and extremely short rides
df <- df %>% filter(bike_type != "docked_bike", rent_duration >= 120)

# Drop rows with missing values in start/end station name
df <- df %>%
  drop_na(start_station_name, end_station_name)

# Create df_casual dataframe containing data for casual users
df_casual <- df %>%
  filter(user_type == "casual")

# Create df_member dataframe containing data for member users
df_member <- df %>%
  filter(user_type == "member")

rm(df)

# Define the order of months
month_order <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

# Convert the "month" column to a factor with the defined order
df_casual$month <- factor(df_casual$month, levels = month_order)
df_member$month <- factor(df_member$month, levels = month_order)

# Function to create a bar plot for user counts per month
create_month_bar_plot <- function(data, title) {
  month_counts <- data.frame(table(data$month))
  month_counts <- month_counts[order(month_counts$Var1), ]
  month_counts$Var1 <- factor(month_counts$Var1, levels = month_order)
  ggplot(month_counts, aes(x = Var1, y = Freq, fill = Var1)) +
    geom_bar(stat = "identity") +
    labs(x = "Month", y = "Ride Count", title = title) +
    geom_text(aes(label = Freq), vjust = -0.5, color = "black") +
    scale_fill_discrete(guide = FALSE) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.margin = margin(40, 20, 20, 20, "pt")) +
    coord_cartesian(ylim = c(0, max(month_counts$Freq) * 1.1))
}

# Create the bar plot for casual users
casual_plot <- create_month_bar_plot(df_casual, "Busyness of Each Month for Casual Users")

# Create the bar plot for member users
member_plot <- create_month_bar_plot(df_member, "Busyness of Each Month for Member Users")

# Display the plots
print(casual_plot)
print(member_plot)

unique(df_casual$month)

# df Peak Months 
casual_peak_months <- df_casual %>%
  filter(month %in% c("May", "June", "July", "August", "September", "October"))

member_peak_months <- df_member %>%
  filter(month %in% c("May", "June", "July", "August", "September", "October"))

# Function to create a bar plot for top 10 busiest stations
create_top_10_stations_bar_plot <- function(data, station_type, user_type, peak_type) {
  station_type_sym <- enquo(station_type)
  station_counts <- data %>%
    count(!!station_type_sym, sort = TRUE) %>%
    top_n(10) %>%
    arrange(desc(n))
    color_palette <- scales::hue_pal()(10)
    ggplot(station_counts, aes(x = reorder(!!station_type_sym, -n), y = n, fill = !!station_type_sym)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = color_palette, guide = "none") +
    labs(x = as_label(station_type_sym), y = "Count", 
         title = paste("Top 10", peak_type, "Stations"),
         subtitle = paste(user_type, "Users, Peak Months")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, margin = margin(b = 20)),
          plot.subtitle = element_text(hjust = 0.5),  # Justify subtitle
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white")) +
    geom_text(aes(label = n), vjust = -0.5, color = "black") +
    coord_cartesian(ylim = c(0, max(station_counts$n) * 1.1))
}

# Create bar plots for casual users and top 10 busiest starting stations
casual_start_plot <- create_top_10_stations_bar_plot(casual_peak_months, start_station_name, "Casual", "Starting")
print(casual_start_plot)
ggsave("Top_10_Busiest_Starting_Stations_for_Casual_Users.png", casual_start_plot, width = 8, height = 6, dpi = 300)

# Create bar plots for casual users and top 10 busiest ending stations
casual_end_plot <- create_top_10_stations_bar_plot(casual_peak_months, end_station_name, "Casual", "Ending")
print(casual_end_plot)
ggsave("Top_10_Busiest_Ending_Stations_for_Casual_Users.png", casual_end_plot, width = 8, height = 6, dpi = 300)

# Create bar plots for member users and top 10 busiest starting stations
member_start_plot <- create_top_10_stations_bar_plot(member_peak_months, start_station_name, "Member", "Starting")
print(member_start_plot)
ggsave("Top_10_Busiest_Starting_Stations_for_Members.png", member_start_plot, width = 8, height = 6, dpi = 300)

# Create bar plots for member users and top 10 busiest ending stations
member_end_plot <- create_top_10_stations_bar_plot(member_peak_months, end_station_name, "Member", "Ending")
print(member_end_plot)
ggsave("Top_10_Busiest_Ending_Stations_for_Members.png", member_end_plot, width = 8, height = 6, dpi = 300)

# Filter casual_peak_months data for top 10 starting stations for casual users
top_10_stations_start_casual <- casual_peak_months %>%
  group_by(start_station_name) %>%
  summarise(ride_count = n()) %>%
  top_n(10, ride_count) %>%
  arrange(desc(ride_count))

# Filter casual_peak_months data for top 10 starting stations
casual_peak_stations <- casual_peak_months %>%
  filter(start_station_name %in% top_10_stations_start_casual$start_station_name)

# Group the data by day of the week and count the number of rides per day
casual_day_counts <- casual_peak_stations %>%
  group_by(day_of_week) %>%
  summarise(ride_count = n()) %>%
  arrange(day_of_week)

# Function to create a bar plot for busyness of each day of the week
create_day_of_week_bar_plot <- function(data, title, subtitle) {
  color_palette <- scales::hue_pal()(length(unique(data$day_of_week)))
  
  ggplot(data, aes(x = day_of_week, y = ride_count, fill = day_of_week)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = ride_count), vjust = -0.5, color = "black") +
    labs(x = "Day of the Week", y = "Ride Count", title = title, subtitle = subtitle) +
    scale_fill_manual(values = color_palette, guide = "none") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14, margin = margin(b = 20)),
          plot.subtitle = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.margin = margin(40, 20, 20, 20, "pt")) +
    coord_cartesian(ylim = c(0, max(data$ride_count) * 1.1))
}

# Filter casual_peak_months data for top 10 ending stations for casual users
top_10_stations_end_casual <- casual_peak_months %>%
  group_by(end_station_name) %>%
  summarise(ride_count = n()) %>%
  top_n(10, ride_count) %>%
  arrange(desc(ride_count))

# Now you can use top_10_stations_end_casual to filter casual_peak_months data
casual_peak_end_stations <- casual_peak_months %>%
  filter(end_station_name %in% top_10_stations_end_casual$end_station_name)

# Filter casual_peak_months data for top 10 ending stations
casual_peak_end_stations <- casual_peak_months %>%
  filter(end_station_name %in% top_10_stations_end_casual$end_station_name)

# Group the data by day of the week and count the number of rides per day
casual_end_day_counts <- casual_peak_end_stations %>%
  group_by(day_of_week) %>%
  summarise(ride_count = n()) %>%
  arrange(day_of_week)

# Generate the bar plots and display them using the print function
casual_day_plot <- create_day_of_week_bar_plot(casual_day_counts, "Casual Users Peak Months Top 10 Start Stations", "Busyness of Each Day of the Week")
print(casual_day_plot)
ggsave("casual_users_peak_months_each_day_starting_stations.png", width = 8, height = 6, dpi = 300)

casual_end_day_plot <- create_day_of_week_bar_plot(casual_end_day_counts, "Casual Users Peak Months Top 10 End Stations", "Busyness of Each Day of the Week")
print(casual_end_day_plot)
ggsave("casual_users_peak_months_each_day_end_stations.png", width = 8, height = 6, dpi = 300)

# Function to create 4-hour time baskets
create_time_basket <- function(time) {
  hour <- as.numeric(format(time, "%H"))
  start_hour <- floor(hour/4) * 4
  end_hour <- start_hour + 3
  time_basket <- sprintf("%02d:00 - %02d:59", start_hour, end_hour)
  return(time_basket)
}

create_time_chart <- function(data, title, subtitle, day_type) {
  data_weekend <- data %>%
    filter(day_of_week %in% c("Saturday", "Sunday")) %>%
    mutate(time_basket = create_time_basket(as.POSIXct(start_time)))
    print(data_weekend)
    time_counts_weekend <- data_weekend %>%
    group_by(time_basket) %>%
    summarise(ride_count = n()) %>%
    arrange(time_basket)
    color_palette_weekend <- scales::hue_pal()(length(unique(time_counts_weekend$time_basket)))
    ggplot(data = time_counts_weekend, aes(x = time_basket, y = ride_count, fill = time_basket)) +
      geom_col() +
      geom_text(aes(label = ride_count), vjust = -0.5, color = "black", size = 3) +
      labs(x = "Time Basket", y = "Total Ride Count",
           title = title,
           subtitle = subtitle) +
      scale_fill_manual(values = color_palette_weekend, guide = "none") +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            plot.margin = margin(40, 20, 50, 20, "pt")) +
      coord_cartesian(ylim = c(0, max(time_counts_weekend$ride_count) * 1.1))
}

# Filter casual_peak_months data for top 10 starting stations for casual users
top_10_stations_start_casual <- casual_peak_months %>%
  group_by(start_station_name) %>%
  summarise(ride_count = n()) %>%
  top_n(10, ride_count) %>%
  arrange(desc(ride_count))

# Filter casual_peak_months data for top 10 starting stations
casual_peak_stations_start <- casual_peak_months %>%
  filter(start_station_name %in% top_10_stations_start_casual$start_station_name)

# Create bar chart for the sum of rides on Saturday and Sunday for starting stations
casual_start_time_chart <- create_time_chart(casual_peak_stations_start,
                                             "Busiest Time of the Day on Weekends (Starting Stations)",
                                             "Top 10 Starting Stations for Casual Users in Peak Months")

# Display the chart for starting stations
print(casual_start_time_chart)

# Save casual_start_time_chart as an image file
ggsave("casual_start_time_chart.png", plot = casual_start_time_chart, width = 8, height = 6, dpi = 300)

# Filter casual_peak_months data for top 10 ending stations for casual users
top_10_stations_end_casual <- casual_peak_months %>%
  group_by(end_station_name) %>%
  summarise(ride_count = n()) %>%
  top_n(10, ride_count) %>%
  arrange(desc(ride_count))

# Filter casual_peak_months data for top 10 ending stations
casual_peak_stations_end <- casual_peak_months %>%
  filter(end_station_name %in% top_10_stations_end_casual$end_station_name)

# Create bar chart for the sum of rides on Saturday and Sunday for ending stations
casual_end_time_chart <- create_time_chart(casual_peak_stations_end,
                                           "Busiest Time of the Day on Weekends (Ending Stations)",
                                           "Top 10 Ending Stations for Casual Users in Peak Months")

# Display the chart for ending stations
print(casual_end_time_chart)

# Save casual_end_time_chart as an image file
ggsave("casual_end_time_chart.png", plot = casual_end_time_chart, width = 8, height = 6, dpi = 300)

# Function to create bar chart for bike type preference
create_bike_type_chart <- function(data, title, subtitle) {
  ggplot(data, aes(x = bike_type, y = n, fill = bike_type)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = ifelse(n >= 1000, scales::comma(n), n)), vjust = -0.2, color = "black", size = 3) +  # Adjust vjust to move the labels above the bars
    labs(x = "Bike Type", y = "", title = title, subtitle = subtitle) +  # Remove the "Count" label
    theme_light() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.border = element_blank(),
      plot.margin = margin(40, 20, 20, 20, "pt"),
      legend.position = "none",
      axis.text.y = element_blank(),  # Hide y-axis labels
      axis.ticks.y = element_blank(),  # Hide y-axis ticks
      axis.title.y = element_blank()   # Hide y-axis title
    ) +
    scale_x_discrete(labels = c("Classic", "Electric"))
}

# Group the data by bike type and count the number of occurrences for casual users
bike_type_counts_all_year <- df_casual %>%
  count(bike_type, sort = TRUE)

# Create a bar chart to visualize the bike type preference for all casual users
chart_all_year <- create_bike_type_chart(
  bike_type_counts_all_year,
  "Bike Type Preference for Casual Users",
  "All Year Long"
)

# Display the chart for all casual users
print(chart_all_year)

# Save the plot as an image file in the current working directory
ggsave("Bike_Type_Preference_for_Casual_Users_all_year.png", chart_all_year, width = 8, height = 6, dpi = 300)

# Group the data by bike type and count the number of occurrences for casual users during peak months
bike_type_counts_peak_months <- casual_peak_months %>%
  count(bike_type, sort = TRUE)

# Create a bar chart to visualize the bike type preference for casual users during peak months
chart_peak_months <- create_bike_type_chart(
  bike_type_counts_peak_months,
  "Bike Type Preference for Casual Users",
  "Peak Months"
)

# Display the chart for casual users during peak months
print(chart_peak_months)

# Save the plot as an image file in the current working directory
ggsave("Bike_Type_Preference_for_Casual_Users_peak_months.png", chart_peak_months, width = 8, height = 6, dpi = 300)

# Assuming you have the dataset 'casual_peak_months' which contains columns 'bike_type', 'month', and 'day_of_week' among others

# Filter data for casual users during peak months on weekends (Saturdays and Sundays)
casual_weekend_peak <- casual_peak_months %>%
  filter(day_of_week %in% c("Saturday", "Sunday"))

# Group the data by bike type and count the number of occurrences on weekends during peak months
bike_type_counts_weekend_peak <- casual_weekend_peak %>%
  count(bike_type, sort = TRUE)

# Create a bar chart to visualize the bike type preference for casual users on weekends during peak months
chart_weekend_peak <- create_bike_type_chart(
  bike_type_counts_weekend_peak,
  "Bike Type Preference for Casual Users",
  "Weekends during Peak Months"
)

# Display the chart for weekends during peak months
print(chart_weekend_peak)

# Save the plot as an image file in the current working directory
ggsave("Bike_Type_Preference_for_Casual_Users_peak_months_weekends.png", chart_weekend_peak, width = 8, height = 6, dpi = 300)

# Function to create specific time charts
create_specific_time_chart <- function(data, station_data, filter_condition, user_type, title, subtitle) {
  specific_station_data <- data %>%
    filter({{ filter_condition }} & day_of_week %in% c("Saturday", "Sunday")) %>%
    mutate(time_basket = create_time_basket(as.POSIXct(start_time))) %>%
    group_by(time_basket) %>%
    summarise(ride_count = n()) %>%
    arrange(time_basket)
  
  plot <- ggplot(specific_station_data, aes(x = time_basket, y = ride_count)) +
    geom_bar(stat = "identity", fill = "blue") +
    geom_text(aes(label = ride_count), vjust = -0.5, color = "black", size = 3.5) +
    labs(x = "Time Basket", y = "Ride Count", title = title, subtitle = subtitle) +
    theme_light() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      plot.margin = margin(40, 20, 20, 20, "pt")
    ) +
    coord_cartesian(ylim = c(0, max(specific_station_data$ride_count) * 1.1))
  
  return(plot)
}

# Filter casual_peak_months data for the specific starting station, Saturdays, and Sundays
specific_start_station <- casual_peak_months %>%
  filter(start_station_name == "Streeter Dr & Grand Ave" &
           day_of_week %in% c("Sat", "Sun"))

# Create specific time chart for the starting station for casual users
specific_start_station_chart_casual <- create_specific_time_chart(
  casual_peak_months,
  specific_start_station,
  start_station_name == "Streeter Dr & Grand Ave",
  "Casual",
  "Streeter Dr & Grand Ave\nStarting Station, Peak months, Weekends",
  "Starting Station, Casual Users, Peak months, Weekends"
)
print(specific_start_station_chart_casual)
ggsave("specific_start_station_chart_casual.png", specific_start_station_chart_casual, width = 8, height = 6, dpi = 300)

# Filter casual_peak_months data for the specific ending station, Saturdays, and Sundays
specific_end_station <- casual_peak_months %>%
  filter(end_station_name == "Streeter Dr & Grand Ave" &
           day_of_week %in% c("Sat", "Sun"))

# Create specific time chart for the ending station for casual users
specific_end_station_chart_casual <- create_specific_time_chart(
  casual_peak_months,
  specific_end_station,
  end_station_name == "Streeter Dr & Grand Ave",
  "Casual",
  "Streeter Dr & Grand Ave\nEnding Station, Peak months, Weekends",
  "Ending Station, Casual Users, Peak months, Weekends"
)

# Print the ending station chart
print(specific_end_station_chart_casual)

# Save the ending station chart as an image
ggsave("specific_end_station_chart_casual.png", specific_end_station_chart_casual, width = 8, height = 6, dpi = 300)

# Filter member_peak_months data for the specific starting station, Saturdays, and Sundays
specific_start_station_member <- member_peak_months %>%
  filter(start_station_name == "Streeter Dr & Grand Ave" &
           day_of_week %in% c("Sat", "Sun"))

# Create specific time chart for the starting station for member users
specific_start_station_chart_member <- create_specific_time_chart(
  member_peak_months,
  specific_start_station_member,
  start_station_name == "Streeter Dr & Grand Ave",
  "Member",
  "Streeter Dr & Grand Ave\nStarting Station, Peak months, Weekends",
  "Starting Station, Member Users, Peak months, Weekends"
)

print(specific_start_station_chart_member)
ggsave("specific_start_station_chart_member.png", specific_start_station_chart_member, width = 8, height = 6, dpi = 300)

# Filter member_peak_months data for the specific ending station, Saturdays, and Sundays
specific_end_station_member <- member_peak_months %>%
  filter(end_station_name == "Streeter Dr & Grand Ave" &
           day_of_week %in% c("Sat", "Sun"))

# Create specific time chart for the ending station for member users
specific_end_station_chart_member <- create_specific_time_chart(
  member_peak_months,
  specific_end_station_member,
  end_station_name == "Streeter Dr & Grand Ave",
  "Member",
  "Streeter Dr & Grand Ave\nEnding Station, Peak months, Weekends",
  "Ending Station, Member Users, Peak months, Weekends"
)

print(specific_end_station_chart_member)
ggsave("specific_end_station_chart_member.png", specific_end_station_chart_member, width = 8, height = 6, dpi = 300)

# Function to create combined time chart for casual and member users
create_combined_time_chart <- function(casual_data, member_data, filter_condition, station_type, station_name, output_file) {
  # Filter data for casual and member users separately and create time baskets
  casual_data_filtered <- casual_data %>%
    filter({{ filter_condition }} & day_of_week %in% c("Saturday", "Sunday")) %>%
    mutate(time_basket = create_time_basket(as.POSIXct(start_time)))
  
  member_data_filtered <- member_data %>%
    filter({{ filter_condition }} & day_of_week %in% c("Saturday", "Sunday")) %>%
    mutate(time_basket = create_time_basket(as.POSIXct(start_time)))
  
  # Calculate the number of rides for each time basket and user type
  casual_counts <- casual_data_filtered %>%
    group_by(time_basket) %>%
    count() %>%
    rename(casual_ride_count = n)
  
  member_counts <- member_data_filtered %>%
    group_by(time_basket) %>%
    count() %>%
    rename(member_ride_count = n)
  
  # Merge the data for casual and member users
  combined_data <- merge(casual_counts, member_counts, by = "time_basket", all = TRUE)
  combined_data[is.na(combined_data)] <- 0
  
  # Plot the combined chart using base R plotting functions
  png(filename = output_file, width = 800, height = 600)
  barplot(
    rbind(combined_data$casual_ride_count, combined_data$member_ride_count),
    beside = TRUE,
    names.arg = combined_data$time_basket,
    col = c("blue", "red"),
    main = paste(station_type, "Station:", station_name, "\nWeekends"),
    xlab = "Time Basket",
    ylab = "Ride Count",
    legend.text = c("Casual", "Member"),
    args.legend = list(x = "topright", bty = "n", ncol = 2)
  )
  dev.off() 
  
  # Add a message to inform that the chart has been saved
  cat(paste("Combined time chart for", station_type, "station", station_name, "has been saved to:", output_file, "\n"))
}

# Create combined time chart for the starting station (Streeter Dr & Grand Ave) for casual and member users
create_combined_time_chart(
  casual_peak_months,
  member_peak_months,
  start_station_name == "Streeter Dr & Grand Ave",
  "Starting",
  "Streeter Dr & Grand Ave",
  "starting_station_chart.png"
)

# Create combined time chart for the ending station (Streeter Dr & Grand Ave) for casual and member users
create_combined_time_chart(
  casual_peak_months,
  member_peak_months,
  end_station_name == "Streeter Dr & Grand Ave",
  "Ending",
  "Streeter Dr & Grand Ave",
  "ending_station_chart.png"
)

# Filter member_peak_months data to get the latitude and longitude information for the top 10 start stations for members
top_start_stations_member <- member_peak_months %>%
  group_by(start_station_name, start_lat, start_lng) %>%
  summarise(ride_count = n()) %>%
  arrange(desc(ride_count)) %>%
  top_n(10) %>%
  ungroup() %>%
  select(start_station_name, start_lat, start_lng)

# Filter your data to get the latitude and longitude information for the top 10 start stations for casual users
top_start_stations_casual <- casual_peak_months %>%
  filter(start_station_name %in% top_10_stations_start_casual$start_station_name) %>%
  distinct(start_station_name, start_lat, start_lng)

# Limit the data to only the top 10 start stations for casual users
top_start_stations_casual <- top_start_stations_casual %>%
  slice(1:10)

# Filter your data to get the latitude and longitude information for the top 10 end stations for casual users
top_end_stations_casual <- casual_peak_months %>%
  filter(end_station_name %in% top_10_stations_end_casual$end_station_name) %>%
  distinct(end_station_name, end_lat, end_lng)

# Limit the data to only the top 10 end stations for casual users
top_end_stations_casual <- top_end_stations_casual %>%
  slice(1:10)

# Filter your data to get the latitude and longitude information for the top 10 start stations for members
top_start_stations_member <- member_peak_months %>%
  filter(start_station_name %in% top_10_stations_start_member$start_station_name) %>%
  distinct(start_station_name, start_lat, start_lng)

# Limit the data to only the top 10 start stations for members
top_start_stations_member <- top_start_stations_member %>%
  slice(1:10)

# Filter your data to get the latitude and longitude information for the top 10 end stations for members
top_end_stations_member <- member_peak_months %>%
  filter(end_station_name %in% top_10_stations_end_member$end_station_name) %>%
  distinct(end_station_name, end_lat, end_lng)

# Limit the data to only the top 10 end stations for members
top_end_stations_member <- top_end_stations_member %>%
  slice(1:10)

# Create the leaflet map
map <- leaflet() %>%
  # Add the tile layer to display the map of the city in the background
  addTiles() %>%
  # Set the initial map view (you can adjust this as needed)
  setView(lng = mean(c(top_start_stations_casual$start_lng, top_end_stations_casual$end_lng, 
                       top_start_stations_member$start_lng, top_end_stations_member$end_lng)),
          lat = mean(c(top_start_stations_casual$start_lat, top_end_stations_casual$end_lat, 
                       top_start_stations_member$start_lat, top_end_stations_member$end_lat)),
          zoom = 12) %>%
  # Add the start stations for casual users as green circles (bigger size)
  addCircleMarkers(data = top_start_stations_casual, lat = ~start_lat, lng = ~start_lng, 
                   fill = TRUE, fillOpacity = 1, fillColor = "green", radius = 6, stroke = FALSE,
                   label = ~start_station_name) %>%
  # Add the end stations for casual users as green circles (bigger size)
  addCircleMarkers(data = top_end_stations_casual, lat = ~end_lat, lng = ~end_lng, 
                   fill = TRUE, fillOpacity = 1, fillColor = "green", radius = 6, stroke = FALSE,
                   label = ~end_station_name) %>%
  # Add the start stations for members as red circles (smaller size)
  addCircleMarkers(data = top_start_stations_member, lat = ~start_lat, lng = ~start_lng, 
                   fill = TRUE, fillOpacity = 1, fillColor = "red", radius = 4, stroke = FALSE,
                   label = ~start_station_name) %>%
  # Add the end stations for members as red circles (smaller size)
  addCircleMarkers(data = top_end_stations_member, lat = ~end_lat, lng = ~end_lng, 
                   fill = TRUE, fillOpacity = 1, fillColor = "red", radius = 4, stroke = FALSE,
                   label = ~end_station_name)

# Save the map as an interactive HTML file
htmlwidgets::saveWidget(map, file = "top_stations_map.html")










rm(list = ls())
