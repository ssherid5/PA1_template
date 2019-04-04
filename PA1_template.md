---
title: "PA1_template"
author: "Steven Sheridan"
date: "April 4, 2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Loading and Processing the data
Read data
```{r}
activity<-read.csv(file='activity.csv')
```

#What is the mean total number of steps taken per day?
Calculate steps per day
```{r}
stepsperday<-aggregate(steps~date,activity,sum)
```

Create steps per day histogram
```{r}
hist(stepsperday$steps,xlab='Number of Steps',ylab='Frequency',main='Steps per Day')
```

Mean and median of steps per day
```{r}
summary(stepsperday$steps)
```

#What is the average daily activity pattern?
Calculate steps per interval
```{r}
stepsperinterval<-aggregate(steps~interval,activity,mean)
```

Plot steps per interval
```{r}
plot(stepsperinterval,type='l',xlab="Interval", ylab="Steps",main="Steps per Interval")
```

Most steps in a single interval
```{r}
max(stepsperinterval$steps)
```
Interval with the most steps
```{r}
stepsperinterval[stepsperinterval$steps==max(stepsperinterval$steps),1]
```

#Imputing missing values
Number of missing values
```{r}
nrow(activity[is.na(activity$steps),])
```

Impute interval mean steps
```{r}
impute<-activity
nas<-is.na(impute$steps)
impute$steps[nas]<-stepsperinterval$steps
```

Calculate new steps per day
```{r}
newstepsperday<-aggregate(steps~date,impute,sum)
```

Create new imputed steps per day histogram
```{r}
hist(newstepsperday$steps,xlab='Number of Steps',ylab='Frequency',main='Steps per Day')
```

New imputed mean and median of steps per day
```{r}
summary(newstepsperday$steps)
```
The imputed mean and median are nearly identical to the originals.

#Are there differences in activity patterns between weekdays and weekends?
Create weekend/weekday categories
```{r}
impute$day <- ifelse(weekdays(as.Date(impute$date)) == "Saturday" | 
                        weekdays(as.Date(impute$date)) == "Sunday", "Weekend", "Weekday")
```

Calculate new steps per interval by weekend/weekday
```{r}
library(plyr)
newstepsperinterval<-ddply(impute,.(interval,day),summarize,avg=mean(steps))
```

Plot steps per interval by weekend/weekday
```{r}
library(lattice)
xyplot(avg~interval|day, data=newstepsperinterval, type="l",  layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")
```
Yes, it looks like people tend to start walking earlier during weekdays and there is a sharp spike at around the 800th interval. On weekends the average number of steps per interval seems to be higher during the day and afternoon.