### Divvy_Exercise_Full_Year_Analysis ###

# This analysis is based on the Divvy case study "'Sophisticated, Clear, and Polished’: Divvy and Data Visualization" written by Kevin Hartman (found here: https://artscience.blog/home/divvy-dataviz-case-study). The purpose of this script is to consolidate downloaded Divvy data into a single dataframe and then conduct simple analysis to help answer the key question: “In what ways do members and casual riders use Divvy bikes differently?”

# # # # # # # # # # # # # # # # # # # # # # # 
# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # #  

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
getwd() #displays your working directory
setwd("/Users/kevinhartman/Desktop/Divvy_Exercise/csv") #sets your working directory to simplify calls to data ... make sure to use your OWN username instead of mine ;)

#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Divvy datasets (csv files) here
df1 <- read.csv('C:/Users/damindu pahasara/Desktop/Google_capstone/data/cycle/CSV/202112-divvy-tripdata.csv')
df2 <- read.csv('C:/Users/damindu pahasara/Desktop/Google_capstone/data/cycle/CSV/202201-divvy-tripdata.csv')
df3 <- read.csv('C:/Users/damindu pahasara/Desktop/Google_capstone/data/cycle/CSV/202202-divvy-tripdata.csv')
df4 <- read.csv('C:/Users/damindu pahasara/Desktop/Google_capstone/data/cycle/CSV/202203-divvy-tripdata.csv')
df5 <- read.csv('C:/Users/damindu pahasara/Desktop/Google_capstone/data/cycle/CSV/202204-divvy-tripdata.csv')
df6 <- read.csv('C:/Users/damindu pahasara/Desktop/Google_capstone/data/cycle/CSV/202205-divvy-tripdata.csv')
df7 <- read.csv('C:/Users/damindu pahasara/Desktop/Google_capstone/data/cycle/CSV/202206-divvy-tripdata.csv')
df8 <- read.csv('C:/Users/damindu pahasara/Desktop/Google_capstone/data/cycle/CSV/202207-divvy-tripdata.csv')
df9 <- read.csv('C:/Users/damindu pahasara/Desktop/Google_capstone/data/cycle/CSV/202208-divvy-tripdata.csv')
df10 <- read.csv('C:/Users/damindu pahasara/Desktop/Google_capstone/data/cycle/CSV/202209-divvy-publictripdata.csv')
df11<- read.csv('C:/Users/damindu pahasara/Desktop/Google_capstone/data/cycle/CSV/202210-divvy-tripdata.csv')
df12<- read.csv('C:/Users/damindu pahasara/Desktop/Google_capstone/data/cycle/CSV/202211-divvy-tripdata.csv')

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file
colnames(df1)
colnames(df2)
colnames(df3)
colnames(df4)
colnames(df5)
colnames(df6)
colnames(df7)
colnames(df8)
colnames(df9)
colnames(df10)
colnames(df11)
colnames(df12)




# Inspect the dataframes and look for incongruencies
str(df1)
str(df2)
str(df3)
str(df4)
str(df5)
str(df6)
str(df7)
str(df8)
str(df9)
str(df10)
str(df11)
str(df12)

# Convert ride_id and rideable_type to character so that they can stack correctly
df1 <-  mutate(df1, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
df2 <-  mutate(df2, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
df3 <-  mutate(df3, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
df4 <-  mutate(df4, ride_id = as.character(ride_id)
               ,rideable_type = as.character(rideable_type)) 
df5 <-  mutate(df5, ride_id = as.character(ride_id)
               ,rideable_type = as.character(rideable_type)) 
df6 <-  mutate(df6, ride_id = as.character(ride_id)
               ,rideable_type = as.character(rideable_type)) 
df7 <-  mutate(df7, ride_id = as.character(ride_id)
               ,rideable_type = as.character(rideable_type)) 
df8 <-  mutate(df8, ride_id = as.character(ride_id)
               ,rideable_type = as.character(rideable_type)) 
df9 <-  mutate(df9, ride_id = as.character(ride_id)
               ,rideable_type = as.character(rideable_type)) 
df10 <-  mutate(df10, ride_id = as.character(ride_id)
               ,rideable_type = as.character(rideable_type)) 
df11<-  mutate(df11, ride_id = as.character(ride_id)
               ,rideable_type = as.character(rideable_type)) 
df12<-  mutate(df12, ride_id = as.character(ride_id)
               ,rideable_type = as.character(rideable_type)) 

# Stack individual quarter's data frames into one big data frame
all_df <- bind_rows(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
all_df <- all_df %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_df)  #List of column names
nrow(all_df)  #How many rows are in data frame?
dim(all_df)  #Dimensions of the data frame?
head(all_df)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_df)  #See list of columns and data types (numeric, character, etc)
summary(all_df)  #Statistical summary of data. Mainly for numerics

# There are a few problems we will need to fix:
# (1) In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). We will need to consolidate that from four to two labels.
# (2) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (3) We will want to add a calculated field for length of ride since the 2020Q1 data did not have the "tripduration" column. We will add "ride_length" to the entire dataframe for consistency.
# (4) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.

# In the "member_casual" column, replace "Subscriber" with "member" and "Customer" with "casual"
# Before 2020, Divvy used different labels for these two types of riders ... we will want to make our dataframe consistent with their current nomenclature
# N.B.: "Level" is a special property of a column that is retained even if a subset does not contain any values from a specific level
# Begin by seeing how many observations fall under each usertype
table(all_df$member_casual)

# Reassign to the desired values (we will go with the current 2020 labels)
all_df <-  all_df %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

# Check to make sure the proper number of observations were reassigned
table(all_df$member_casual)

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
all_df$date <- as.Date(all_df$started_at) #The default format is yyyy-mm-dd

all_df$month <- format(as.Date(all_df$date), "%m")
all_df$day <- format(as.Date(all_df$date), "%d")
all_df$year <- format(as.Date(all_df$date), "%Y")
all_df$day_of_week <- format(as.Date(all_df$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
all_df$ride_length <- difftime(all_df$ended_at,all_df$started_at)

# Inspect the structure of the columns
str(all_df)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_df$ride_length)
all_df$ride_length <- as.numeric(as.character(all_df$ride_length))
is.numeric(all_df$ride_length)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
all_df_v2 <- all_df[!(all_df$start_station_name == "HQ QR" | all_df$ride_length<0),]

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_df_v2$ride_length) #straight average (total ride length / rides)
median(all_df_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_df_v2$ride_length) #longest ride
min(all_df_v2$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_df_v2$ride_length)

# Compare members and casual users
aggregate(all_df_v2$ride_length ~ all_df_v2$member_casual, FUN = mean)
aggregate(all_df_v2$ride_length ~ all_df_v2$member_casual, FUN = median)
aggregate(all_df_v2$ride_length ~ all_df_v2$member_casual, FUN = max)
aggregate(all_df_v2$ride_length ~ all_df_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_df_v2$ride_length ~ all_df_v2$member_casual + all_df_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_df_v2$day_of_week <- ordered(all_df_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_df_v2$ride_length ~ all_df_v2$member_casual + all_df_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_df_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Let's visualize the number of rides by rider type
all_df_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_df_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#Visualize average_duration of riders type and weekday

all_df_v2 %>% 
  mutate(weekday = wday(started_at , label = TRUE)) %>%
  group_by(member_casual, weekday) %>%  
  summarise(average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes( x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Average duration of ride by day of the week", subtitle = "ride_length and weekday")


#Visualize total number of ride by member_casual(rider type) and months of ride

all_df_v2 %>% 
  group_by(member_casual,month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual,month) %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual )) +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#Visualize the ride_length(total distance) and month of the ride

all_df_v2 %>% 
  group_by(member_casual,month) %>% 
  summarise(average_ride_length = mean(ride_length)) %>% 
  arrange(month) %>% 
  ggplot(aes(x = month, y = average_ride_length, fill = member_casual)) +
  labs(title = "Average ride distance of members and casual riders by month")+
  theme(axis.text.x = element_text(angle = 45)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#Visualize members and casuals by the number of ride taken (ride count)

all_df_v2 %>% 
  group_by(member_casual) %>% 
  summarise(ride_count = length(ride_id)) %>% 
  arrange(member_casual) %>% 
  ggplot(aes(x = member_casual,y = ride_count,fill = member_casual)) +
  geom_col(position = "dodge")+
  labs(title = "Total rides taken (ride_count) of Members and Casual riders")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#compare Members and Casual riders depending on ride distance

all_df_v2 %>% 
  group_by(member_casual) %>% 
  summarise(average_ride_length = mean(ride_length)) %>%
  ggplot() + 
  geom_col(mapping= aes(x= member_casual,y= average_ride_length,fill=member_casual), show.legend = FALSE)+
  labs(title = "Mean distance traveled by Members and Casual riders")


#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
# N.B.: This file location is for a Mac. If you are working on a PC, change the file location accordingly (most likely "C:\Users\YOUR_USERNAME\Desktop\...") to export the data. You can read more here: https://datatofish.com/export-dataframe-to-csv-in-r/
counts <- aggregate(all_df_v2$ride_length ~ all_df_v2$member_casual + all_df_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'C:/Users/damindu pahasara/Desktop/Google_capstone/all_df_v2.1.R')

#You're done! Congratulations!
