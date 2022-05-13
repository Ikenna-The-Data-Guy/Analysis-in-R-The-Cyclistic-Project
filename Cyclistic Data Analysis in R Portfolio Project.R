## DATA ANALYSIS IN R (THE CYCLISTIC PROJECT)

## First we install all the packages required for the project.

install.packages('tidyverse')
library(tidyverse)
install.packages('janitor')
library(janitor)
install.packages('lubridate')
library(lubridate)
install.packages('skimr')
library(skimr)
install.packages('readr')
library(readr)
install.packages('readxl')
library(readxl)
install.packages('here')
library(here)


## Import Data into R



Jan_Trips <- read_csv("R/unzipped/202101-divvy-tripdata.csv")
Feb_Trips <- read_csv("R/unzipped/202102-divvy-tripdata.csv")
March_Trips <- read_csv("R/unzipped/202103-divvy-tripdata.csv")
April_Trips <- read_csv("R/unzipped/202104-divvy-tripdata.csv")
May_Trips <- read_csv("R/unzipped/202105-divvy-tripdata.csv")
June_Trips <- read_csv("R/unzipped/202106-divvy-tripdata.csv")
July_Trips <- read_csv("R/unzipped/202107-divvy-tripdata.csv")
Aug_Trips <- read_csv("R/unzipped/202108-divvy-tripdata.csv")
Sept_Trips <- read_csv("R/unzipped/202109-divvy-tripdata.csv")
Oct_Trips <- read_csv("R/unzipped/202110-divvy-tripdata.csv")
Nov_Trips <- read_csv("R/unzipped/202111-divvy-tripdata.csv")
Dec_Trips <- read_csv("R/unzipped/202112-divvy-tripdata.csv")



## Saving the Data sets and using the 'column name' function to ensure uniformity.
## There are twelve data sets covering the twelve months under review. 

View(Jan_Trips)
colnames(Jan_Trips)
View(Feb_Trips)
colnames(Feb_Trips)
View(March_Trips)
colnames(March_Trips)
View(April_Trips)
colnames(April_Trips)
View(May_Trips)
colnames(May_Trips)
View(June_Trips)
colnames(June_Trips)
View(July_Trips)
colnames(July_Trips)
View(Aug_Trips)
colnames(Aug_Trips)
View(Sept_Trips)
colnames(Sept_Trips)
View(Oct_Trips)
colnames(Oct_Trips)
View(Nov_Trips)
colnames(Nov_Trips)
View(Dec_Trips)
colnames(Dec_Trips)



## Merging the 12 data sets into one table using the "bind_rows" function.  

Year_2021_Trips <- bind_rows(Jan_Trips, Feb_Trips, March_Trips, April_Trips, May_Trips, June_Trips, July_Trips, Aug_Trips, Sept_Trips, Oct_Trips, Nov_Trips, Dec_Trips)
View(Year_2021_Trips)


## DATA CLEANING.

## To Drop the "start_lat", "start_lng", "end_lat",and "end_lng" columns as well as the "na" values.

Year_2021_Trips <- Year_2021_Trips %>%
  select(-c(start_lat,start_lng,end_lat,end_lng)) %>%
  drop_na()

##
Year_2021_Trips %>% drop_na()
Year_2021_Trips_clean <- Year_2021_Trips_clean %>% drop_na()
##


## Inspect the New Table that was created

colnames(Year_2021_Trips)  #List of column names     
nrow(Year_2021_Trips)      #To determine the number of rows
dim(Year_2021_Trips)       #To determine the dimension of the data frame
heads(Year_2021_Trips)     #To view the first six rows with the header
tail(Year_2021_Trips)      #To determine the bottom six rows
str(Year_2021_Trips)       #To view the columns and data types
summary(Year_2021_Trips)   #Statistical summary of the data


## Conversion of "docked_bike" to "classic_bike" in the "rideable_type" column.

Year_2021_Trips <- Year_2021_Trips %>%
  mutate(rideable_type=str_replace(rideable_type,"docked_bike","classic_bike"))
View(Year_2021_Trips)


## Creation of columns "month", "day_of_week", "hour_of_day", "ride_duration_hrs", "ride_duration_mins",and "route".


Year_2021_Tripsv2 <- Year_2021_Trips %>% mutate(month=month(started_at,label=TRUE),
                                                day_of_week=wday(started_at,label=TRUE),
                                                hour_of_day=hour(started_at),
                                                ride_duration_hrs=difftime(ended_at,started_at,units="hours"),
                                                ride_duration_mins=difftime(ended_at,started_at,units="mins"),
                                                route=str_c(start_station_name,end_station_name,sep="--"))

View(Year_2021_Tripsv2)



## Renaming the "member_casual" column as "customer_type".

Year_2021_Trips_clean <- Year_2021_Tripsv2 %>% rename(customer_type = member_casual)
View(Year_2021_Trips_clean)



##Plotting the ride count by customer type chart

ggplot(data = Year_2021_Trips_clean) +
  geom_bar(mapping = aes(y = customer_type, fill = customer_type)) +
  labs(title = "Annual Breakdown 2021", subtitle = "Ride Count by Customer Type", caption = "Courtsey Motivate International Inc.")




##Plot of the customer type vs month

ggplot(data = Year_2021_Trips_clean) +
  geom_bar(mapping = aes(x=month,fill=customer_type)) +
  theme(axis.text.x = element_text(angle=90)) +
  facet_wrap(~customer_type) +
  labs(title = "Ride Count vs Month (2)", subtitle = "Ride Count by Customer Type", caption = "Courtsey Motivate International Inc.")
  



##Plot of the rideable type vs month

ggplot(data = Year_2021_Trips_clean) +
  geom_bar(mapping = aes(x=month,fill=rideable_type)) +
  theme(axis.text.x = element_text(angle=90)) +
  facet_wrap(~rideable_type) +
  labs(title = "Ride Count vs Month", subtitle = "Ride Count by Bike Type", caption = "Courtsey Motivate International Inc.")

## Plot of Rides by the Week.
ggplot(data = Year_2021_Trips_clean) +
  geom_bar(mapping = aes(y=day_of_week)) +
  labs(title = "Ride Count by day_of_week", caption = "Courtesy Motivate International Inc.")



## Developing tables and exporting them to excel for charts


w <- Year_2021_Trips_clean %>% group_by(customer_type) %>% count(day_of_week)
View(w)
write.table(w,file = "day_of_week.csv",sep = ",")

s <- Year_2021_Trips_clean %>% group_by(month) %>% count(day_of_week)
View(s)
write.table(s,file = "seasonal.csv",sep = ",")

r <- Year_2021_Trips_clean %>% group_by(rideable_type) %>% count(month)
View(r)
write.table(r,file = "rideable.csv",sep = ",")

g <- Year_2021_Trips_clean %>% group_by(customer_type) %>% count(month)
View(g)
write.table(g,file = "customer_type.csv",sep = ",")

sv2 <- Year_2021_Trips_clean %>% group_by(month) %>% group_by(day_of_week) %>% count(customer_type)
View(sv2)
snap <- Year_2021_Trips_clean %>% select(month, day_of_week, customer_type)
View(snap)
seas <- snap %>% group_by(day_of_week) %>% count(customer_type)
View(seas)

## to get the table for Winter
winter <- rows %>% filter(month %in% c("Dec", "Jan", "Feb"))
View(winter)
Winter <- winter %>% group_by(day_of_week) %>% count(customer_type)
View(Winter)
write.table(Winter,file = "Winter.csv",sep = ",")

## to get the table for Spring
spring <- rows %>% filter(month %in% c("Mar", "Apr", "May"))
Spring <- spring %>% group_by(day_of_week) %>% count(customer_type)
View(Spring)
write.table(Spring,file = "Spring.csv",sep = ",")

## to get the table for Summer
summer <- rows %>% filter(month %in% c("Jun", "Jul", "Aug"))
Summer <- summer %>% group_by(day_of_week) %>% count(customer_type)
View(Summer)
write.table(Summer,file = "Summer.csv",sep = ",")

## to get the table for Autumn
autumn <- rows %>% filter(month %in% c("Sep", "Oct", "Nov"))
Autumn <- autumn %>% group_by(day_of_week) %>% count(customer_type)
View(Autumn)
write.table(Autumn,file = "Autumn.csv",sep = ",")

## for ride duration weekly analysis
weekly <- Year_2021_Trips_clean %>% select(customer_type, day_of_week, ride_duration_mins) %>% group_by(day_of_week) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(weekly)
weekly2 <- Year_2021_Trips_clean %>% select(customer_type, day_of_week, ride_duration_mins) %>% group_by(day_of_week) %>% count(day_of_week)
View(weekly2)
write.table(weekly, file = "weekly.csv", sep = ",")
write.table(weekly2, file = "weekly2.csv", sep = ",")
weekly3 <- Year_2021_Trips_clean %>% select(customer_type, day_of_week, ride_duration_mins) %>% group_by(day_of_week) %>% count(customer_type)
View(weekly3)
write.table(weekly3, file = "weekly3.csv", sep = ",")

## for ride duration weekly analysis at the customer_type level
Sun <- Year_2021_Trips_clean %>% select(customer_type, day_of_week, ride_duration_mins) %>% filter(day_of_week == "Sun")
View(Sun)
Sun_org <- Sun %>% group_by(customer_type) %>% summarise(ave_daily_ride = mean(ride_duration_mins))
View(Sun_org)
write.table(Sun_org, file = "Sun_org.csv", sep = ",")
Mon <- Year_2021_Trips_clean %>% select(customer_type, day_of_week, ride_duration_mins) %>% filter(day_of_week == "Mon")
View(Mon)
Mon_org <- Mon %>% group_by(customer_type) %>% summarise(ave_daily_ride = mean(ride_duration_mins))
View(Mon_org)
write.table(Mon_org, file = "Mon_org.csv", sep = ",")
Tue <- Year_2021_Trips_clean %>% select(customer_type, day_of_week, ride_duration_mins) %>% filter(day_of_week == "Tue")
View(Tue)
Tue_org <- Tue %>% group_by(customer_type) %>% summarise(ave_daily_ride = mean(ride_duration_mins))
View(Tue_org)
write.table(Tue_org, file = "Tue_org.csv", sep = ",")
Wed <- Year_2021_Trips_clean %>% select(customer_type, day_of_week, ride_duration_mins) %>% filter(day_of_week == "Wed")
View(Wed)
Wed_org <- Wed %>% group_by(customer_type) %>% summarise(ave_daily_ride = mean(ride_duration_mins))
View(Wed_org)
write.table(Wed_org, file = "Wed_org.csv", sep = ",")
Thu <- Year_2021_Trips_clean %>% select(customer_type, day_of_week, ride_duration_mins) %>% filter(day_of_week == "Thu")
View(Thu)
Thu_org <- Thu %>% group_by(customer_type) %>% summarise(ave_daily_ride = mean(ride_duration_mins))
View(Thu_org)
write.table(Thu_org, file = "Thu_org.csv", sep = ",")
Fri <- Year_2021_Trips_clean %>% select(customer_type, day_of_week, ride_duration_mins) %>% filter(day_of_week == "Fri")
View(Fri)
Fri_org <- Fri %>% group_by(customer_type) %>% summarise(ave_daily_ride = mean(ride_duration_mins))
View(Fri_org)
write.table(Fri_org, file = "Fri_org.csv", sep = ",")
Sat <- Year_2021_Trips_clean %>% select(customer_type, day_of_week, ride_duration_mins) %>% filter(day_of_week == "Sat")
View(Sat)
Sat_org <- Sat %>% group_by(customer_type) %>% summarise(ave_daily_ride = mean(ride_duration_mins))
View(Sat_org)
write.table(Sat_org, file = "Sat_org.csv", sep = ",")

## creation of seasons row
season_rows <- Year_2021_Trips_clean %>% select(month,day_of_week,customer_type,ride_duration_mins)
View(season_rows)

## for ride duration weekly analysis at the customer_type level over the winter seasons
wint <- season_rows %>% filter(month %in% c("Dec", "Jan", "Feb"))
View(wint)
## to get the average duration for winter days
wint_Sun <- wint %>% filter(day_of_week == "Sun") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(wint_Sun)
write.table(wint_Sun, file = "wint_Sun.csv", sep = ",")
wint_Mon <- wint %>% filter(day_of_week == "Mon") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(wint_Mon)
write.table(wint_Mon, file = "wint_Mon.csv", sep = ",")
wint_Tue <- wint %>% filter(day_of_week == "Tue") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(wint_Tue)
write.table(wint_Tue, file = "wint_Tue.csv", sep = ",")
wint_Wed <- wint %>% filter(day_of_week == "Wed") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(wint_Wed)
write.table(wint_Wed, file = "wint_Wed.csv", sep = ",")
wint_Thu <- wint %>% filter(day_of_week == "Thu") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(wint_Thu)
write.table(wint_Thu, file = "wint_Thu.csv", sep = ",")
wint_Fri <- wint %>% filter(day_of_week == "Fri") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(wint_Fri)
write.table(wint_Fri, file = "wint_Fri.csv", sep = ",")
wint_Sat <- wint %>% filter(day_of_week == "Sat") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(wint_Sat)
write.table(wint_Sat, file = "wint_Sat.csv", sep = ",")


## for ride duration weekly analysis at the customer_type level over the spring seasons
spring <- season_rows %>% filter(month %in% c("Mar", "Apr", "May"))
View(spring)
## to get the average duration for spring days
spring_Sun <- spring %>% filter(day_of_week == "Sun") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(spring_Sun)
write.table(spring_Sun, file = "spring_Sun.csv", sep = ",")
spring_Mon <- spring %>% filter(day_of_week == "Mon") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(spring_Mon)
write.table(spring_Mon, file = "spring_Mon.csv", sep = ",")
spring_Tue <- spring %>% filter(day_of_week == "Tue") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(spring_Tue)
write.table(spring_Tue, file = "spring_Tue.csv", sep = ",")
spring_Wed <- spring %>% filter(day_of_week == "Wed") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(spring_Wed)
write.table(spring_Wed, file = "spring_Wed.csv", sep = ",")
spring_Thu <- spring %>% filter(day_of_week == "Thu") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(spring_Thu)
write.table(spring_Thu, file = "spring_Thu.csv", sep = ",")
spring_Fri <- spring %>% filter(day_of_week == "Fri") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(spring_Fri)
write.table(spring_Fri, file = "spring_Fri.csv", sep = ",")
spring_Sat <- spring %>% filter(day_of_week == "Sat") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(spring_Sat)
write.table(spring_Sat, file = "spring_Sat.csv", sep = ",")

## for ride duration weekly analysis at the customer_type level over the summer seasons
summer <- season_rows %>% filter(month %in% c("Jun", "Jul", "Aug"))
View(summer)
## to get the average duration for summer days
summer_Sun <- summer %>% filter(day_of_week == "Sun") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(summer_Sun)
write.table(summer_Sun, file = "summer_Sun.csv", sep = ",")
summer_Mon <- summer %>% filter(day_of_week == "Mon") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(summer_Mon)
write.table(summer_Mon, file = "summer_Mon.csv", sep = ",")
summer_Tue <- summer %>% filter(day_of_week == "Tue") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(summer_Tue)
write.table(summer_Tue, file = "summer_Tue.csv", sep = ",")
summer_Wed <- summer %>% filter(day_of_week == "Wed") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(summer_Wed)
write.table(summer_Wed, file = "summer_Wed.csv", sep = ",")
summer_Thu <- summer %>% filter(day_of_week == "Thu") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(summer_Thu)
write.table(summer_Thu, file = "summer_Thu.csv", sep = ",")
summer_Fri <- summer %>% filter(day_of_week == "Fri") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(summer_Fri)
write.table(summer_Fri, file = "summer_Fri.csv", sep = ",")
summer_Sat <- summer %>% filter(day_of_week == "Sat") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(summer_Sat)
write.table(summer_Sat, file = "summer_Sat.csv", sep = ",")

## for ride duration weekly analysis at the customer_type level over the autumn seasons
autumn <- season_rows %>% filter(month %in% c("Sep", "Oct", "Nov"))
View(autumn)
## to get the average duration for autumn days
autumn_Sun <- autumn %>% filter(day_of_week == "Sun") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(autumn_Sun)
write.table(autumn_Sun, file = "autumn_Sun.csv", sep = ",")
autumn_Mon <- autumn %>% filter(day_of_week == "Mon") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(autumn_Mon)
write.table(autumn_Mon, file = "autumn_Mon.csv", sep = ",")
autumn_Tue <- autumn %>% filter(day_of_week == "Tue") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(autumn_Tue)
write.table(autumn_Tue, file = "autumn_Tue.csv", sep = ",")
autumn_Wed <- autumn %>% filter(day_of_week == "Wed") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(autumn_Wed)
write.table(autumn_Wed, file = "autumn_Wed.csv", sep = ",")
autumn_Thu <- autumn %>% filter(day_of_week == "Thu") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(autumn_Thu)
write.table(autumn_Thu, file = "autumn_Thu.csv", sep = ",")
autumn_Fri <- autumn %>% filter(day_of_week == "Fri") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(autumn_Fri)
write.table(autumn_Fri, file = "autumn_Fri.csv", sep = ",")
autumn_Sat <- autumn %>% filter(day_of_week == "Sat") %>% group_by(customer_type) %>% summarise(ave_ride_duration = mean(ride_duration_mins))
View(autumn_Sat)
write.table(autumn_Sat, file = "autumn_Sat.csv", sep = ",")

## start time analysis
start_time_row <- Year_2021_Trips_clean %>% select(month, day_of_week, customer_type, hour_of_day)
View(start_time_row)

## season week analysis/winter
start_time_week_winter <- start_time_row %>% filter(month %in% c("Dec", "Jan", "Feb"))
View(start_time_week_winter)
## for winter Sun
winter_sun_start <- start_time_week_winter %>% filter(day_of_week == "Sun") %>% group_by(customer_type) %>% count(hour_of_day) 
View(winter_sun_start)
write.table(sun_start, file = "sun_start.csv", sep = ",")
## for winter Mon
winter_mon_start <- start_time_week_winter %>% filter(day_of_week == "Mon") %>% group_by(customer_type) %>% count(hour_of_day) 
View(winter_mon_start)
write.table(winter_mon_start, file = "winter_mon_start.csv", sep = ",")
## for winter Tue
winter_tue_start <- start_time_week_winter %>% filter(day_of_week == "Tue") %>% group_by(customer_type) %>% count(hour_of_day) 
View(winter_tue_start)
write.table(winter_tue_start, file = "winter_tue_start.csv", sep = ",")
## for winter Wed
winter_wed_start <- start_time_week_winter %>% filter(day_of_week == "Wed") %>% group_by(customer_type) %>% count(hour_of_day) 
View(winter_wed_start)
write.table(winter_wed_start, file = "winter_wed_start.csv", sep = ",")
## for winter Thu
winter_thu_start <- start_time_week_winter %>% filter(day_of_week == "Thu") %>% group_by(customer_type) %>% count(hour_of_day) 
View(winter_thu_start)
write.table(winter_thu_start, file = "winter_thu_start.csv", sep = ",")
## for winter Fri
winter_fri_start <- start_time_week_winter %>% filter(day_of_week == "Fri") %>% group_by(customer_type) %>% count(hour_of_day) 
View(winter_fri_start)
write.table(winter_fri_start, file = "winter_fri_start.csv", sep = ",")
## for winter Sat
winter_sat_start <- start_time_week_winter %>% filter(day_of_week == "Sat") %>% group_by(customer_type) %>% count(hour_of_day) 
View(winter_sat_start)
write.table(winter_sat_start, file = "winter_sat_start.csv", sep = ",")

## season week analysis/spring
start_time_week_spring <- start_time_row %>% filter(month %in% c("Mar", "Apr", "May"))
View(start_time_week_spring)
## for spring Sun
spring_sun_start <- start_time_week_spring %>% filter(day_of_week == "Sun") %>% group_by(customer_type) %>% count(hour_of_day) 
View(spring_sun_start)
write.table(spring_sun_start, file = "spring_sun_start.csv", sep = ",")
## for spring Mon
spring_mon_start <- start_time_week_spring %>% filter(day_of_week == "Mon") %>% group_by(customer_type) %>% count(hour_of_day) 
View(spring_mon_start)
write.table(spring_mon_start, file = "spring_mon_start.csv", sep = ",")
## for spring Tue
spring_tue_start <- start_time_week_spring %>% filter(day_of_week == "Tue") %>% group_by(customer_type) %>% count(hour_of_day) 
View(spring_tue_start)
write.table(spring_tue_start, file = "spring_tue_start.csv", sep = ",")
## for spring Wed
spring_wed_start <- start_time_week_spring %>% filter(day_of_week == "Wed") %>% group_by(customer_type) %>% count(hour_of_day) 
View(spring_wed_start)
write.table(spring_wed_start, file = "spring_wed_start.csv", sep = ",")
## for spring Thu
spring_thu_start <- start_time_week_spring %>% filter(day_of_week == "Thu") %>% group_by(customer_type) %>% count(hour_of_day) 
View(spring_thu_start)
write.table(spring_thu_start, file = "spring_thu_start.csv", sep = ",")
## for spring Fri
spring_fri_start <- start_time_week_spring %>% filter(day_of_week == "Fri") %>% group_by(customer_type) %>% count(hour_of_day) 
View(spring_fri_start)
write.table(spring_fri_start, file = "spring_fri_start.csv", sep = ",")
## for spring Sat
spring_sat_start <- start_time_week_spring %>% filter(day_of_week == "Sat") %>% group_by(customer_type) %>% count(hour_of_day) 
View(spring_sat_start)
write.table(spring_sat_start, file = "spring_sat_start.csv", sep = ",")

## season week analysis/summer
start_time_week_summer <- start_time_row %>% filter(month %in% c("Jun", "Jul", "Aug"))
View(start_time_week_summer)
## for summer Sun
summer_sun_start <- start_time_week_summer %>% filter(day_of_week == "Sun") %>% group_by(customer_type) %>% count(hour_of_day) 
View(summer_sun_start)
write.table(summer_sun_start, file = "summer_sun_start.csv", sep = ",")
## for summer Mon
summer_mon_start <- start_time_week_summer %>% filter(day_of_week == "Mon") %>% group_by(customer_type) %>% count(hour_of_day) 
View(summer_mon_start)
write.table(summer_mon_start, file = "summer_mon_start.csv", sep = ",")
## for summer Tue
summer_tue_start <- start_time_week_summer %>% filter(day_of_week == "Tue") %>% group_by(customer_type) %>% count(hour_of_day) 
View(summer_tue_start)
write.table(summer_tue_start, file = "summer_tue_start.csv", sep = ",")
## for summer Wed
summer_wed_start <- start_time_week_summer %>% filter(day_of_week == "Wed") %>% group_by(customer_type) %>% count(hour_of_day) 
View(summer_wed_start)
write.table(summer_wed_start, file = "summer_wed_start.csv", sep = ",")
## for summer Thu
summer_thu_start <- start_time_week_summer %>% filter(day_of_week == "Thu") %>% group_by(customer_type) %>% count(hour_of_day) 
View(summer_thu_start)
write.table(summer_thu_start, file = "summer_thu_start.csv", sep = ",")
## for summer Fri
summer_fri_start <- start_time_week_summer %>% filter(day_of_week == "Fri") %>% group_by(customer_type) %>% count(hour_of_day) 
View(summer_fri_start)
write.table(summer_fri_start, file = "summer_fri_start.csv", sep = ",")
## for summer Sat
summer_sat_start <- start_time_week_summer %>% filter(day_of_week == "Sat") %>% group_by(customer_type) %>% count(hour_of_day) 
View(summer_sat_start)
write.table(summer_sat_start, file = "summer_sat_start.csv", sep = ",")

## season week analysis/autumn
start_time_week_autumn <- start_time_row %>% filter(month %in% c("Sep", "Oct", "Nov"))
View(start_time_week_autumn)
## for autumn Sun
autumn_sun_start <- start_time_week_autumn %>% filter(day_of_week == "Sun") %>% group_by(customer_type) %>% count(hour_of_day) 
View(autumn_sun_start)
write.table(autumn_sun_start, file = "autumn_sun_start.csv", sep = ",")
## for autumn Mon
autumn_mon_start <- start_time_week_autumn %>% filter(day_of_week == "Mon") %>% group_by(customer_type) %>% count(hour_of_day) 
View(autumn_mon_start)
write.table(autumn_mon_start, file = "autumn_mon_start.csv", sep = ",")
## for autumn Tue
autumn_tue_start <- start_time_week_autumn %>% filter(day_of_week == "Tue") %>% group_by(customer_type) %>% count(hour_of_day) 
View(autumn_tue_start)
write.table(autumn_tue_start, file = "autumn_tue_start.csv", sep = ",")
## for autumn Wed
autumn_wed_start <- start_time_week_autumn %>% filter(day_of_week == "Wed") %>% group_by(customer_type) %>% count(hour_of_day) 
View(autumn_wed_start)
write.table(autumn_wed_start, file = "autumn_wed_start.csv", sep = ",")
## for autumn Thu
autumn_thu_start <- start_time_week_autumn %>% filter(day_of_week == "Thu") %>% group_by(customer_type) %>% count(hour_of_day) 
View(autumn_thu_start)
write.table(autumn_thu_start, file = "autumn_thu_start.csv", sep = ",")
## for autumn Fri
autumn_fri_start <- start_time_week_autumn %>% filter(day_of_week == "Fri") %>% group_by(customer_type) %>% count(hour_of_day) 
View(autumn_fri_start)
write.table(autumn_fri_start, file = "autumn_fri_start.csv", sep = ",")
## for autumn Sat
autumn_sat_start <- start_time_week_autumn %>% filter(day_of_week == "Sat") %>% group_by(customer_type) %>% count(hour_of_day) 
View(autumn_sat_start)
write.table(autumn_sat_start, file = "autumn_sat_start.csv", sep = ",")

## most popular route/casual riders
popular_route <- Year_2021_Trips_clean %>% group_by(customer_type) %>% count(route)
View(popular_route)
popular_route_casual <- popular_route %>% filter(customer_type == "casual") %>% arrange(-n)
View(popular_route_casual)
popular_route_casual2 <- head(popular_route_casual, 10)
View(popular_route_casual2)
write.table(popular_route_casual2, file = "popular_route_casual2.csv", sep = ",")

## most popular route/members
popular_route <- Year_2021_Trips_clean %>% group_by(customer_type) %>% count(route)
View(popular_route)
popular_route_member <- popular_route %>% filter(customer_type == "member") %>% arrange(-n)
View(popular_route_member)
popular_route_member2 <- head(popular_route_member, 10)
View(popular_route_member2)
write.table(popular_route_member2, file = "popular_route_member2.csv", sep = ",")



