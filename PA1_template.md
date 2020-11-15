---
title: "PROJECT-1, Course- Reproducable "
author: "Anirudh Gambhir"
date: "11/13/2020"
output: html_document
---

* Loading Dataset *
```{r}
library(dplyr)
library(ggplot2)
library(lubridate)
dataset=read.csv("/Users/anirudhgambhir/Documents/COLLEGE/SEM5/R Prog./Datasets/Coursera Datasets/activity.csv")
```
* Total Steps and daily steps *
```{r}
sum(dataset$steps,na.rm = T)
daily_steps<-aggregate(steps~date, data=dataset, FUN=sum, na.rm=TRUE)
```

Q. What is mean total number of steps taken per day?
-------------------------------------------------

* histogram of the total number of steps taken each day *
```{r}
ggplot(data = daily_steps)+geom_histogram(aes(x=steps),binwidth = 5000,fill='orange',col='red')+labs(x="Number of Steps",y="Frequency",title = "Number of Steps taken each day")
```


```{r}
mean(daily_steps$steps)
median(daily_steps$steps)
```
    
Q. What is the average daily activity pattern?
-------------------------------------------

* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) *

```{r}
time_series <- aggregate(steps~interval, data=dataset, FUN=mean, na.rm=TRUE)
ggplot(data = time_series)+geom_line(aes(x=interval,y=steps),col="pink")+labs(x="Time Interval",y="Number of Steps",title = "Time Series Graph")+theme_bw()
```

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? *
```{r}
max_steps <- max(time_series$steps)
for (i in 1:288) 
{
  if (time_series$steps[i] == max_steps)
    time_series_max <- time_series$interval[i]
}
time_series_max
```

Q. Imputing missing values
-----------------------

* Calculate and report the total number of missing values in the dataset*
```{r}
colSums(is.na(dataset))
```

* Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
dataset1 <- dataset
for (i in 1:17568) 
{
  if(is.na(dataset1$steps[i])) 
  { 
    check <- dataset1$interval[i]
    for (j in 1:288)
    {
      if (time_series$interval[j] == check) 
        dataset1$steps[i] <-time_series$steps[j] 
      
    }
  }
}
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
```{r}
total_steps <- aggregate(steps~date, data=dataset1, FUN=sum, na.rm=TRUE)
hist(total_steps$steps)
mean(total_steps$steps)
median(total_steps$steps)
```

Q.Are there differences in activity patterns between weekdays and weekends?
---------------------------------------------------------------------------

* Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r}
week <- wday(dataset1$date)
week_day <- week
for (i in 1:17568)
{
  if(week[i] == 1)
    week_day[i] <- 'weekend'
  if(week[i] == 2)
    week_day[i] <- 'weekday'
  if(week[i] == 3)
    week_day[i] <- 'weekday'
  if(week[i] == 4)
    week_day[i] <- 'weekday'
  if(week[i] == 5)
    week_day[i] <- 'weekday'
  if(week[i] == 6)
    week_day[i] <- 'weekday'
  if(week[i] == 7)
    week_day[i] <- 'weekend'
}
dataset1$weekday <-week_day
weekday <- grep("weekday",dataset1$weekday)
weekday_frame <- dataset1[weekday,]
weekend_frame <- dataset1[-weekday,]
```


* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
```{r}
time_series_weekday <- aggregate(steps~interval, data=weekday_frame, FUN=mean, na.rm=TRUE)
time_series_weekend <- aggregate(steps~interval, data=weekend_frame, FUN=mean, na.rm=TRUE)
plot(x = time_series_weekday$interval, y = time_series_weekday$steps, type = "l")
plot(x = time_series_weekend$interval, y = time_series_weekend$steps, type = "l")
