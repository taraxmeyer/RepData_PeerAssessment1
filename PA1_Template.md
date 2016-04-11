---
title: "Reproducible Research - Peer Assessement 1"
author: "taraxmeyer"
date: "8 April 2016"
output: 
   html_document:
   keep_md: TRUE
---
## Work space, loading, processing data
```{r load data}
library(rio)
library(knitr)
setwd("~/Desktop/data_science/ReproData/")
opts_chunk$set(echo = TRUE, results = 'hold')

data <- read.csv("activity.csv")
data$date <- as.POSIXct(data$date, format="%Y-%m-%d")
data      <- data.frame(date=data$date,weekday=tolower(weekdays(data$date)),
             steps=data$steps,interval=data$interval) 
data      <- cbind(data,daytype=ifelse(data$weekday=="saturday"|data$weekday==
             "sunday","weekend","weekday"))
data      <- data.frame(date=data$date,weekday=data$weekday,daytype=data$daytype, 
             interval=data$interval,steps=data$steps)
export(data,"hw1.csv")
```

Processed data set to make it easier to answer all questions, specifically for "dates", 
in addition to those having to deal with "weekday" and "weekend". I saved the tidy data 
in a new file called hw1.csv that I will read in for the rest of the analysis. 

## What is the mean total number of steps taken per day?
```{r total steps}
total.steps <- aggregate(data$steps,by=list(data$date),FUN=sum,na.rm=TRUE) 
names(total.steps) <- c("date","steps") 
summary(total.steps)
```

Create a histogram. 
```{r hist ts}
hist(total.steps$steps, 
     breaks=seq(from=0, to=25000, by=2500),
     col="red", 
     xlab="total number of steps", 
     ylim=c(0, 20), 
     main="Histogram: Total Daily Steps (-NA)")
```
![1](https://github.com/taraxmeyer/ReproData_PA1/blob/master/figure/PA1-1.png)

Find the mean and median daily steps taken. See summary OR:
```{r ts mm}
mean(total.steps$steps,na.rm=TRUE)
median(total.steps$steps,na.rm=TRUE)
```
There are 9354 mean daily steps and 10395 median daily steps. 

## What is the average daily activity pattern? 
```{r avg daily}
mean_steps <- aggregate(data$steps, 
                       by=list(data$interval), 
                       FUN=mean, 
                       na.rm=TRUE) 
names(mean_steps) <- c("interval","mean") 
```
Create the time series plot. 
```{r avg step int}
plot(mean_steps$interval, 
     mean_steps$mean, 
     type="l", 
     col="red", 
     lwd=2, 
     xlab="Interval [min.]", 
     ylab="Average Steps", 
     main="Time-Series: Average Steps per Interval (-NA)")
```
![2](https://github.com/taraxmeyer/ReproData_PA1/blob/master/figure/PA1-2.png)

Look at the interval with the maximum steps.
```{r max int}
max_pos <- which(mean_steps$mean == max(mean_steps$mean)) 
max_interval <- mean_steps[max_pos, 1] 
```

## Imputing missing values. 

Calculate and report total number of missing values in the dataset, then fill in
MV. 
```{r MV}
MVcount <- sum(is.na(data$steps))
mv_pos <- which(is.na(data$steps))
mean_vec <- rep(mean(data$steps,na.rm=TRUE),times=length(mv_pos))
data[mv_pos, "steps"] <- mean_vec 
total.steps <- aggregate(data$steps,by=list(data$date),FUN=sum)
names(total.steps) <- c("date","steps")
```
Make a histogram of the total number of steps taken each day.
```{r MV histogram, results="hide"}
hist(total.steps$steps, 
     breaks=seq(from=0, to=25000, by=2500),
     col="blue", 
     xlab="total number of steps", 
     ylim=c(0, 30), 
     main="Histogram: Total Daily Steps (NA=Mean)")
```
![3](https://github.com/taraxmeyer/ReproData_PA1/blob/master/figure/PA1-3.png)

Calculate the mean and median of total daily steps.
```{r mean and median total steps}
mean(total.steps$steps) 
median(total.steps$steps) 
export(data,"hw1new.csv")
```
The mean and median of total daily steps are both 10766.19 respectively. 
hw1new.csv reads in the MV-fixed data set. 

The mean and median values increase after imputing the missing values/data. Why
this occurs is due to the way the missing values are coded for any interval. 
When there is a missing value (NA), then the number of steps taken are set to 
0 by default. However, after replacing missing values with the mean steps of 
a related interval value, missing values are accounted for. The older 0 values
get removed from the histogram and the total number of daily steps increases
overall. 

## Are there differences in activity patterns between weekdays and weekends?
As noted, data is already prepped and processed to make this comparison (see 
first section.)
```{r days}
data <- read.csv("hw1new.csv")
mean_data <- aggregate(data$steps,by=list(data$daytype,data$weekday,
             data$interval), mean) 
names(mean_data) <- c("daytype", "weekday", "interval", "mean") 
```
Create a panel plot comparing the average number of steps taken during 
the week and on the weekend. 
```{r lattice}
library(lattice)
xyplot(mean~interval|daytype,mean_data, 
       type="l", 
       lwd=1, 
       xlab="interval", 
       ylab="number of steps", 
       layout=c(1,2))
```
![4](https://github.com/taraxmeyer/ReproData_PA1/blob/master/figure/PA1-4.png)
