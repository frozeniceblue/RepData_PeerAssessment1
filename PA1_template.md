---
title: "PA1_template"
author: "Zhengmao Zhu"
date: "August 21, 2018"
output: html_document
---



#Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

- Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as 𝙽𝙰) 
date: The date on which the measurement was taken in YYYY-MM-DD format 
interval: Identifier for the 5-minute interval in which measurement was taken 
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

#Code for reading in the dataset and/or processing the data

```r
raw_data=read.csv("C:\\Users\\frozenl\\Desktop\\coursera\\project 7\\activity.csv",header = TRUE, sep=",",na.strings = "NA")
summary(raw_data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
str(raw_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(raw_data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
raw_data$date=as.Date(raw_data$date,format="%Y-%m-%d")
raw_data$interval=factor(raw_data$interval)
```

#Histogram of the total number of steps taken each day

```r
data=raw_data[!(is.na(raw_data$steps)), ]
day_steps=aggregate(steps~date, data=data, sum)
names(day_steps)=c("date","steps")
hist(day_steps$steps, breaks=10, col="blue", xlab="Number of Steps", main="Histogram of the total number of steps taken each day")
```

![plot of chunk hist of each day steps](figure/hist of each day steps-1.png)

#Mean and median number of steps taken each day

```r
mean(day_steps$steps)
```

```
## [1] 10766.19
```

```r
median(day_steps$steps)
```

```
## [1] 10765
```

#Time series plot of the average number of steps taken

```r
interval_steps=aggregate(data$steps, by=list(interval=data$interval),FUN=mean)
names(interval_steps)=c("interval","ave_steps")
plot(levels(interval_steps$interval), interval_steps$ave_steps,type="l",col="blue",
     xlab="Interval", ylab="Average Number of Steps", main="Average Daily Activity")
```

![plot of chunk time series plot of ave steps](figure/time series plot of ave steps-1.png)

#The 5-minute interval that, on average, contains the maximum number of steps

```r
max_steps=max(interval_steps$ave_steps)
max_steps
```

```
## [1] 206.1698
```

```r
max_interval=interval_steps$interval[which.max(interval_steps$ave_steps)]
max_interval
```

```
## [1] 835
## 288 Levels: 0 5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 ... 2355
```

#Code to describe and show a strategy for imputing missing data

```r
sum(is.na(raw_data))
```

```
## [1] 2304
```

```r
sum(is.na(raw_data$steps))
```

```
## [1] 2304
```

```r
sum(is.na(raw_data$date))
```

```
## [1] 0
```

```r
sum(is.na(raw_data$interval))
```

```
## [1] 0
```

```r
processed_data=raw_data
position=which(is.na(raw_data$steps))
processed_data[position, ]$steps=unlist(lapply(position, FUN=function(position){
    interval_steps[raw_data[position, ]$interval==interval_steps$interval, ]$ave_steps
}))
summary(processed_data)
```

```
##      steps             date               interval    
##  Min.   :  0.00   Min.   :2012-10-01   0      :   61  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   5      :   61  
##  Median :  0.00   Median :2012-10-31   10     :   61  
##  Mean   : 37.38   Mean   :2012-10-31   15     :   61  
##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   20     :   61  
##  Max.   :806.00   Max.   :2012-11-30   25     :   61  
##                                        (Other):17202
```

#Histogram of the total number of steps taken each day after missing values are imputed

```r
processed_day_steps=aggregate(steps~date, data=processed_data, sum)
names(processed_day_steps)=c("date","steps")
hist(processed_day_steps$steps, breaks=10, col="blue", xlab="Number of Steps", main="Histogram of the total number of steps taken each day")
```

![plot of chunk hist of each day steps with filling missing data](figure/hist of each day steps with filling missing data-1.png)

#Mean and median number of steps taken each day

```r
mean(processed_day_steps$steps)
```

```
## [1] 10766.19
```

```r
median(processed_day_steps$steps)
```

```
## [1] 10766.19
```

From the results, we know that compared with raw data, the mean of filling data is unchanged and the median of the filling data changes from 10765 to 10766.19, which is same as the mean of the filling data.

#Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
week_days=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
processed_data$day=as.factor(ifelse(is.element(weekdays(as.Date(processed_data$date)),week_days), "Weekday", "Weekend"))
week_steps=aggregate(steps~interval + day, data=processed_data, mean)
library(lattice)
xyplot(week_steps$steps ~ week_steps$interval | week_steps$day, layout = c(1,2), type ="l", ylab="Number of Steps", xlab="Interval", main="Average Steps per Day by Interval")
```

![plot of chunk compare steps of weekdays and weekend](figure/compare steps of weekdays and weekend-1.png)
