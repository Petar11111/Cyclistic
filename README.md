# Import libraries
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
# Read csv, and merge everything into one df
filenames = [
    "202112-divvy-tripdata.csv",
    "202201-divvy-tripdata.csv",
    "202202-divvy-tripdata.csv",
    "202203-divvy-tripdata.csv",
    "202204-divvy-tripdata.csv",
    "202205-divvy-tripdata.csv",
    "202206-divvy-tripdata.csv",
    "202207-divvy-tripdata.csv",
    "202208-divvy-tripdata.csv",
    "202209-divvy-publictripdata.csv",
    "202210-divvy-tripdata.csv",
    "202211-divvy-tripdata.csv"
]

dataframes = []

for filename in filenames:
    df = pd.read_csv(filename)
    dataframes.append(df)

# Combine all DataFrames into a single DataFrame using pd.concat()
df = pd.concat(dataframes, ignore_index=True)

# Basic imfo about combined df
df.shape
df.columns
print(df.dtypes)
df.describe()
# Rename columns
df = df.rename(columns={
    'rideable_type': 'bike_type',
    'started_at': 'start_time',
    'ended_at': 'end_time',
    'member_casual': 'user_type'
})

# Create necessary columns 
df['start_time'] = pd.to_datetime(df['start_time'])
df['end_time'] = pd.to_datetime(df['end_time'])
df['rent_duration'] = (df['end_time'] - df['start_time']).dt.total_seconds()
df['month'] = df['start_time'].dt.strftime('%B')
df['day_of_the_week'] = df['start_time'].dt.day_name()

# Remove unnecessary columns
columns_to_remove = ['ride_id', 'start_station_id', 'end_station_id', 'start_lat', 'start_lng', 'end_lat', 'end_lng']
df = df.drop(columns=columns_to_remove)

# Filter out bad data
df = df[df['bike_type'] != 'docked_bike']
df = df[df['rent_duration'] >= 120]
df = df.dropna(subset=['start_station_name', 'end_station_name'])

# Create spearate df for casual users and members
Casual = df[df['user_type'] == 'casual'].copy()
Members = df[df['user_type'] == 'member'].copy()

# Find how busy each month is
def plot_rides_per_month(data, user_type):
    rides_per_month = data['month'].value_counts().sort_index()
    custom_order = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December']
    rides_per_month = rides_per_month.reindex(custom_order)

    custom_palette = sns.color_palette("Set2", len(rides_per_month))

    plt.figure(figsize=(10, 6))
    ax = sns.barplot(x=rides_per_month.index, y=rides_per_month.values, palette=custom_palette)

    plt.xlabel('Month')
    plt.ylabel('Number of Rides')
    plt.title(f'Number of Rides for Each Month ({user_type} Users)')

    for i, value in enumerate(rides_per_month.values):
        ax.text(i, value + 1000, str(value), ha='center', va='bottom', fontweight='bold')

    plt.xticks(rotation=45)

    plt.show()

plot_rides_per_month(Casual, 'Casual')
plot_rides_per_month(Members, 'Members')

# Peak months df
casual_peak_months = Casual[Casual['month'].isin(["May", "June", "July", "August", "September", "October"])]
member_peak_months = Members[Members['month'].isin(["May", "June", "July", "August", "September", "October"])]

# Function to process data and get top 10 busiest stations
def get_top_10_stations(df, station_column):
    station_counts = df[station_column].value_counts().reset_index()
    station_counts.columns = [station_column, 'n']
    top_10_stations = station_counts.nlargest(10, 'n')
    top_10_stations = top_10_stations.sort_values(by='n', ascending=False)
    return top_10_stations

# Get top 10 busiest starting stations for casual users
top_10_stations_start_casual = get_top_10_stations(casual_peak_months, 'start_station_name')

# Get top 10 busiest ending stations for casual users
top_10_stations_end_casual = get_top_10_stations(casual_peak_months, 'end_station_name')

# Get top 10 busiest starting stations for members
top_10_stations_start_member = get_top_10_stations(member_peak_months, 'start_station_name')

# Get top 10 busiest ending stations for members
top_10_stations_end_member = get_top_10_stations(member_peak_months, 'end_station_name')

# Function to plot the bar chart
def plot_bar_chart(data, x, y, title, xlabel, ylabel):
    plt.figure(figsize=(10, 6))
    ax = sns.barplot(x=x, y=y, data=data, palette="Set2")
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.title(title)
    plt.xticks(rotation=45, ha='right')
    for index, value in enumerate(data[y]):
        ax.text(index, value, str(value), ha='center', va='bottom', fontweight='bold')
    plt.tight_layout()
    plt.show()

# Plot the top 10 busiest starting stations for casual users
plot_bar_chart(top_10_stations_start_casual, 'start_station_name', 'n', 
               'Top 10 Busiest Starting Stations for Casual Users',
               'Starting Station', 'Count')

# Plot the top 10 busiest ending stations for casual users
plot_bar_chart(top_10_stations_end_casual, 'end_station_name', 'n', 
               'Top 10 Busiest Ending Stations for Casual Users',
               'Ending Station', 'Count')

# Plot the top 10 busiest starting stations for members
plot_bar_chart(top_10_stations_start_member, 'start_station_name', 'n', 
               'Top 10 Busiest Starting Stations for Members',
               'Starting Station', 'Count')

# Plot the top 10 busiest ending stations for members
plot_bar_chart(top_10_stations_end_member, 'end_station_name', 'n', 
               'Top 10 Busiest Ending Stations for Members',
               'Ending Station', 'Count')

# How busy is each day of the week for Casual users in peak months on top 10 stations
# Function to process data and get day counts
def get_day_counts(df, station_column, top_stations):
    filtered_stations = df[df[station_column].isin(top_stations[station_column])]
    day_counts = filtered_stations['day_of_the_week'].value_counts().reset_index()
    day_counts.columns = ['day_of_the_week', 'ride_count']
    day_counts = day_counts.sort_values(by='day_of_the_week')
    return day_counts

# Get day counts for top 10 starting stations for casual users
casual_day_counts_start = get_day_counts(casual_peak_months, 'start_station_name', top_10_stations_start_casual)

# Get day counts for top 10 ending stations for casual users
casual_day_counts_end = get_day_counts(casual_peak_months, 'end_station_name', top_10_stations_end_casual)

# Function to create a bar plot with wider bars
def plot_bar_chart(data, x, y, title, xlabel, ylabel):
    plt.figure(figsize=(10, 6))
    ax = sns.barplot(x=x, y=y, data=data, palette="Set2", hue='day_of_the_week', dodge=False)
    bar_width = 0.9  
    for bar in ax.patches:
        bar.set_width(bar_width)
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.title(title)
    plt.suptitle('Busyness of Each Day of the Week', fontsize=12, fontweight='bold', y=0.95)
    plt.xticks(rotation=45, ha='right')
    for index, value in enumerate(data[y]):
        ax.text(index, value, str(value), ha='center', va='bottom', fontweight='bold')
    plt.tight_layout()
    plt.show()

 # Create a bar plot for busyness of each day of the week for top starting and ending stations
plot_bar_chart(casual_day_counts_start, 'day_of_the_week', 'ride_count', 
               'Casual Users Peak Months Top 10 Start Stations',
               'Day of the Week', 'Ride Count')

plot_bar_chart(casual_day_counts_end, 'day_of_the_week', 'ride_count', 
               'Casual Users Peak Months Top 10 End Stations',
               'Day of the Week', 'Ride Count')

# Function to create 4-hour time baskets
def create_time_basket(time):
    hour = time.hour
    start_hour = (hour // 4) * 4
    end_hour = start_hour + 3
    time_basket = f"{start_hour:02d}:00 - {end_hour:02d}:59"
    return time_basket
# Filter casual_peak_months data for top 10 ending stations
casual_peak_stations_end = casual_peak_months[casual_peak_months['end_station_name'].isin(top_10_stations_end_casual['end_station_name'])].copy()

# Create time basket column for ending stations
casual_peak_stations_end['time_basket'] = casual_peak_stations_end['end_time'].apply(create_time_basket)

# Group the data by day_of_the_week, time_basket, and ending station, and calculate the sum of rides
casual_end_time_counts = casual_peak_stations_end.groupby(['day_of_the_week', 'time_basket', 'end_station_name'])['end_time'].count().reset_index()
casual_end_time_counts.rename(columns={'end_time': 'ride_count'}, inplace=True)
casual_end_time_counts.sort_values(by=['day_of_the_week', 'time_basket', 'end_station_name'], inplace=True)

# Summarize the data to get the total ride count for each time basket
casual_end_time_sum = casual_end_time_counts.groupby('time_basket')['ride_count'].sum().reset_index()

# Filter casual_peak_months data for top 10 starting stations
casual_peak_stations_start = casual_peak_months[casual_peak_months['start_station_name'].isin(top_10_stations_start_casual['start_station_name'])].copy()

# Create time basket column for starting stations
casual_peak_stations_start['time_basket'] = casual_peak_stations_start['start_time'].apply(create_time_basket)

# Group the data by day_of_the_week, time_basket, and starting station, and calculate the sum of rides
casual_start_time_counts = casual_peak_stations_start.groupby(['day_of_the_week', 'time_basket', 'start_station_name'])['start_time'].count().reset_index()
casual_start_time_counts.rename(columns={'start_time': 'ride_count'}, inplace=True)
casual_start_time_counts.sort_values(by=['day_of_the_week', 'time_basket', 'start_station_name'], inplace=True)

# Summarize the data to get the total ride count for each time basket
casual_start_time_sum = casual_start_time_counts.groupby('time_basket')['ride_count'].sum().reset_index()

def plot_barchart(data, x, y, title, subtitle, filename):
    plt.figure(figsize=(10, 6))
    ax = sns.barplot(x=x, y=y, data=data)
    for index, value in enumerate(data[y]):
        ax.text(index, value, str(value), ha='center', va='bottom', fontweight='bold')
    plt.xlabel('Time Basket')
    plt.ylabel('Total Ride Count')
    plt.title(title)
    plt.suptitle(subtitle, fontsize=14, fontweight='bold', y=0.95)
    plt.xticks(rotation=45, ha='right')
    plt.gca().axes.get_yaxis().set_visible(False)
    sns.despine(left=True)
    plt.tight_layout()
    plt.savefig(filename, dpi=300)
    plt.show()

# Create a bar chart for the sum of rides on Saturday, Sunday, and Monday for the top 10 starting stations
plot_barchart(casual_start_time_sum, 'time_basket', 'ride_count',
              'Busiest Time of the Day',
              'Top 10 Starting Stations for Casual Users in Peak Months',
              "casual_start_time_chart.png")

# Create a bar chart for the sum of rides on Saturday, Sunday, and Monday for the top 10 ending stations
plot_barchart(casual_end_time_sum, 'time_basket', 'ride_count',
              'Busiest Time of the Day',
              'Top 10 Ending Stations for Casual Users in Peak Months',
              "casual_end_time_chart.png")
							
# Function to create a bar plot for bike type distribution
def plot_bike_type_distribution(data, user_type):
    bike_type_counts = data['bike_type'].value_counts()
    plt.figure(figsize=(6, 4))
    ax = sns.barplot(x=bike_type_counts.index, y=bike_type_counts.values, palette="Set2")
    plt.xlabel('Bike Type')
    plt.ylabel('Number of Rides')
    plt.title(f'Bike Type Distribution for {user_type} Users')
    for index, value in enumerate(bike_type_counts.values):
        ax.text(index, value, str(value), ha='center', va='bottom', fontweight='bold')
    plt.tight_layout()
    plt.show()

# Plot bike type distribution for casual users in the entire dataset
plot_bike_type_distribution(Casual, 'Casual')

# Filter casual users data for peak months
casual_peak_months = Casual[Casual['month'].isin(["May", "June", "July", "August", "September", "October"])]

# Plot bike type distribution for casual users in peak months
plot_bike_type_distribution(casual_peak_months, 'Casual (Peak Months)')

# Filter casual users data for Saturdays and Sundays in peak months
casual_peak_weekends = casual_peak_months[casual_peak_months['day_of_the_week'].isin(['Saturday', 'Sunday'])]

# Plot bike type distribution for casual users on Saturdays and Sundays in peak months
plot_bike_type_distribution(casual_peak_weekends, 'Casual (Peak Weekends)')





