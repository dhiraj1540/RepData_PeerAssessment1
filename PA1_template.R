## Load necessary packages 
library(knitr)
opts_chunk$set(echo = TRUE)
library(lubridate)
library(dplyr)
library(ggplot2)
install.packages("RcppRoll")
library(RcppRoll)

## Loading and Reprocessing the data
## Download data if doesn't exist in current director
if(!file.exists("activity")) {
  temp <- tempfile()
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", temp)
  file <- unzip(temp)
  unlink(temp)
}

## 1. Read and clean the data for analysis
activity <- read.csv("activity.csv", header = TRUE)
## 2. Process the data for analysis
activity$date <- ymd(activity$date)
data <- tbl_df(activity)

## Mean and Median number of steps taken per day
## 1. Total number of steps taken per day
steps <- data %>%  filter(!is.na(steps)) %>%  group_by(date) %>%  summarize(steps = sum(steps)) %>%print
## 2. Histogram of total number of steps taken each day

hist(Steps$steps, col= "red", xlab="Steps Per Day", main= "Total Steps Per Day")
## 3. Mean of total number of steps taken per day
MeanSteps <- mean(Steps$steps)
print(MeanSteps)
## Answer = 10766.19
## Median of total number of steps taken per day
MedianSteps <- median(Steps$steps)
print(MedianSteps)
## Answer = 10765


## Average daily activity pattern

## Time series plot of the average number of steps taken
steps_interval <- data%>% filter(!is.na(data$steps)) %>% 
  group_by(interval) %>% summarize(steps = mean(steps))
plot(steps_interval$interval, steps_interval$steps, col="red",
     xlab="Interval (5 Min)", ylab="Average Number of Steps", 
     main="Time Series Plot", type="l")

## The 5-minute interval that, on average, contains the maximum number of steps
## First we filter by maximum value of steps and then record corresponding interval
intervals <- data%>% filter(!is.na(data$steps)) %>% 
  group_by(interval) %>% summarize(steps = sum(steps)) 

(filter(intervals, steps==max(intervals$steps)))$interval
## Answer = 835th interval

## Imputing missing data
## 1. Total number of missing values in data
MissingValues <- length(which(is.na(data$steps)))
## Number of missing values =2304
## 2. We will use average number of steps in the same 5 Min interval to fill out NA values
data_full <- data
nas <- is.na(data_full$steps)
avg_interval <- tapply(data_full$steps, data_full$interval, mean, na.rm=TRUE, simplify=TRUE)

## 3. Creating a new data setequal to original data set but with filled missing values
data_full$steps[nas] <- avg_interval[as.character(data_full$interval[nas])]
sum(is.na(data_full$steps))

### 4. Histogram for total number of steps taken each day after imputing missing data
steps_full <- data_full %>%   filter(!is.na(steps)) %>%  group_by(date) %>%  summarize(steps = sum(steps)) %>%
ggplot(steps_full, aes(x = steps)) +  geom_histogram(fill = "firebrick", binwidth = 1000) +
    labs(title = "Histogram of Steps per day after imputing missing values", x = "Steps per day", y = "Frequency")
## Mean and median for the new data set
mean_steps_full <- mean(steps_full$steps, na.rm = TRUE)
print(mean_steps_full)
median_steps_full <- median(steps_full$steps, na.rm = TRUE)
print(median_steps_full)
## Imputing missing data didn't affect the mean and median of our original data set.

## Activity pattern on weekdays and weekends
## 1. Creating a new factor variable weektype to distinguish between weekdays and weekend data.
data_full$date <- as.Date(as.character(data_full$date))
data_full <- mutate(data_full, weektype = ifelse(weekdays(data_full$date) == "Saturday" | weekdays(data_full$date) == "Sunday", "weekend", "weekday"))
data_full$weektype <- as.factor(data_full$weektype)
head(data_full)
## Creating the panel plot for weekdays and weekends
interval_full <- data_full %>%  group_by(interval, weektype) %>%  summarise(steps = mean(steps))
p <- ggplot(interval_full, aes(x=interval, y=steps, color = weektype)) +
  geom_line() +
  facet_wrap(~weektype, ncol = 1, nrow=2)
print(p)


