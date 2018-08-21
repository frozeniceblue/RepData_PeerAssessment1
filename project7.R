## Code for reading in the dataset and/or processing the data
raw_data=read.csv("C:\\Users\\frozenl\\Desktop\\coursera\\project 7\\activity.csv",header = TRUE, sep=",",na.strings = "NA")
summary(raw_data)
str(raw_data)
head(raw_data)
raw_data$date=as.Date(raw_data$date,format="%Y-%m-%d")
raw_data$interval=factor(raw_data$interval)

## Histogram of the total number of steps taken each day
data=raw_data[!(is.na(raw_data$steps)), ]
day_steps=aggregate(steps~date, data=data, sum)
names(day_steps)=c("date","steps")
hist(day_steps$steps, breaks=10, col="blue", xlab="Number of Steps", main="Histogram of the total number of steps taken each day")

## Mean and median number of steps taken each day
mean(day_steps$steps)
median(day_steps$steps)

## Time series plot of the average number of steps taken
interval_steps=aggregate(data$steps, by=list(interval=data$interval),FUN=mean)
names(interval_steps)=c("interval","ave_steps")
plot(levels(interval_steps$interval), interval_steps$ave_steps,type="l",col="blue",
     xlab="Interval", ylab="Average Number of Steps", main="Average Daily Activity")

## The 5-minute interval that, on average, contains the maximum number of steps
max_steps=max(interval_steps$ave_steps)
max_steps
max_interval=interval_steps$interval[which.max(interval_steps$ave_steps)]
max_interval

## Code to describe and show a strategy for imputing missing data
sum(is.na(raw_data))
sum(is.na(raw_data$steps))
sum(is.na(raw_data$date))
sum(is.na(raw_data$interval))
processed_data=raw_data
position=which(is.na(raw_data$steps))
processed_data[position, ]$steps=unlist(lapply(position, FUN=function(position){
    interval_steps[raw_data[position, ]$interval==interval_steps$interval, ]$ave_steps
}))
summary(processed_data)

## Histogram of the total number of steps taken each day after missing values are imputed
processed_day_steps=aggregate(steps~date, data=processed_data, sum)
names(processed_day_steps)=c("date","steps")
hist(processed_day_steps$steps, breaks=10, col="blue", xlab="Number of Steps", main="Histogram of the total number of steps taken each day")

## Mean and median number of steps taken each day with filling data
mean(processed_day_steps$steps)
median(processed_day_steps$steps)

## Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
week_days=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
processed_data$day=as.factor(ifelse(is.element(weekdays(as.Date(processed_data$date)),week_days), "Weekday", "Weekend"))
week_steps=aggregate(steps~interval + day, data=processed_data, mean)
library(lattice)
xyplot(week_steps$steps ~ week_steps$interval | week_steps$day, layout = c(1,2), type ="l", ylab="Number of Steps", xlab="Interval", main="Average Steps per Day by Interval")
