#----------------------------------------------------#

#### taraxmeyer ####
#### Reproducible Data Assignment 1 ####
#### April 2016 ####

#----------------------------------------------------#

# Set up work space

setwd("~/Desktop/data_science/ReproData/")
library(knitr)
library(rio)

data <- read.csv("activity.csv") 
data$date <- as.POSIXct(data$date, format="%Y-%m-%d") 
data      <- data.frame(date=data$date,weekday=tolower(weekdays(data$date)),
             steps=data$steps,interval=data$interval) 
data      <- cbind(data,daytype=ifezlse(data$weekday=="saturday"|data$weekday==
             "sunday","weekend","weekday"))
data      <- data.frame(date=data$date,weekday=data$weekday,daytype=data$daytype, 
             interval=data$interval,steps=data$steps) #reassign
export(data, "hw1.csv")

#----------------------------------------------------#

total.steps <- aggregate(data$steps,by=list(data$date),FUN=sum,na.rm=TRUE) #sum
names(total.steps) <- c("date","steps") #change column names
summary(total.steps)

hist(total.steps$steps, 
     breaks=seq(from=0, to=25000, by=2500),
     col="red", 
     xlab="total number of steps", 
     ylim=c(0, 20), 
     main="Histogram: Total Daily Steps (-NA)")

mean(total.steps$steps,na.rm=TRUE)
median(total.steps$steps,na.rm=TRUE)

dev.copy(png,'PA1-1.png')
dev.off()

#----------------------------------------------------#

mean_steps <- aggregate(data$steps, 
                       by=list(data$interval), 
                       FUN=mean, 
                       na.rm=TRUE)
names(mean_steps) <- c("interval","mean")

plot(mean_steps$interval, 
     mean_steps$mean, 
     type="l", 
     col="red", 
     lwd=2, 
     xlab="Interval [min.]", 
     ylab="Average Steps", 
     main="Time-Series: Average Steps per Interval (-NA)")

max_pos <- which(mean_steps$mean == max(mean_steps$mean)) 
max_interval <- mean_steps[max_pos, 1] 

dev.copy(png,'PA1-2.png')
dev.off()

#----------------------------------------------------#

MVcount <- sum(is.na(data$steps))
mv_pos <- which(is.na(data$steps))
mean_vec <- rep(mean(data$steps,na.rm=TRUE),times=length(mv_pos))
data[mv_pos, "steps"] <- mean_vec

total.steps <- aggregate(data$steps,by=list(data$date),FUN=sum)
names(total.steps) <- c("date","steps") 

hist(total.steps$steps, 
     breaks=seq(from=0, to=25000, by=2500),
     col="blue", 
     xlab="total number of steps", 
     ylim=c(0, 30), 
     main="Histogram: Total Daily Steps (NA=Mean)")

mean(total.steps$steps) 
median(total.steps$steps) 
export(data,"hw1new.csv")

dev.copy(png,'PA1-3.png')
dev.off()

#----------------------------------------------------#

library(lattice)
mean_data <- aggregate(data$steps,by=list(data$daytype,data$weekday,
             data$interval), mean) 
names(mean_data) <- c("daytype", "weekday", "interval", "mean") 
head(mean_data)

xyplot(mean~interval|daytype,mean_data, 
       type="l", 
       lwd=1, 
       xlab="interval", 
       ylab="number of steps", 
       layout=c(1,2))
remove(mean_data) 

dev.copy(png,'PA1-4.png')
dev.off()

#----------------------------------------------------#





