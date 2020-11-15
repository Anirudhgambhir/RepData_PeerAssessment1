-   Loading Dataset \*

<!-- -->

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(ggplot2)
    library(lubridate)

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

    dataset=read.csv("/Users/anirudhgambhir/Documents/COLLEGE/SEM5/R Prog./Datasets/Coursera Datasets/activity.csv")

-   Total Steps and daily steps \*

<!-- -->

    sum(dataset$steps,na.rm = T)

    ## [1] 570608

    daily_steps<-aggregate(steps~date, data=dataset, FUN=sum, na.rm=TRUE)

Q. What is mean total number of steps taken per day?
----------------------------------------------------

-   histogram of the total number of steps taken each day \*

<!-- -->

    ggplot(data = daily_steps)+geom_histogram(aes(x=steps),binwidth = 5000,fill='orange',col='red')+labs(x="Number of Steps",y="Frequency",title = "Number of Steps taken each day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    mean(daily_steps$steps)

    ## [1] 10766.19

    median(daily_steps$steps)

    ## [1] 10765

Q. What is the average daily activity pattern?
----------------------------------------------

-   Make a time series plot (i.e. type = “l”) of the 5-minute interval
    (x-axis) and the average number of steps taken, averaged across all
    days (y-axis) \*

<!-- -->

    time_series <- aggregate(steps~interval, data=dataset, FUN=mean, na.rm=TRUE)
    ggplot(data = time_series)+geom_line(aes(x=interval,y=steps),col="pink")+labs(x="Time Interval",y="Number of Steps",title = "Time Series Graph")+theme_bw()

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

-   Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps? \*

<!-- -->

    max_steps <- max(time_series$steps)
    for (i in 1:288) 
    {
      if (time_series$steps[i] == max_steps)
        time_series_max <- time_series$interval[i]
    }
    time_series_max

    ## [1] 835

Q. Imputing missing values
--------------------------

-   Calculate and report the total number of missing values in the
    dataset\*

<!-- -->

    colSums(is.na(dataset))

    ##    steps     date interval 
    ##     2304        0        0

-   Create a new dataset that is equal to the original dataset but with
    the missing data filled in.

<!-- -->

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

-   Make a histogram of the total number of steps taken each day and
    Calculate and report the mean and median total number of steps taken
    per day.

<!-- -->

    total_steps <- aggregate(steps~date, data=dataset1, FUN=sum, na.rm=TRUE)
    hist(total_steps$steps)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-9-1.png)

    mean(total_steps$steps)

    ## [1] 10766.19

    median(total_steps$steps)

    ## [1] 10766.19

Q.Are there differences in activity patterns between weekdays and weekends?
---------------------------------------------------------------------------

-   Create a new factor variable in the dataset with two levels –
    “weekday” and “weekend” indicating whether a given date is a weekday
    or weekend day.

<!-- -->

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

-   Make a panel plot containing a time series plot (i.e. type = “l”) of
    the 5-minute interval (x-axis) and the average number of steps
    taken, averaged across all weekday days or weekend days (y-axis).

<!-- -->

    time_series_weekday <- aggregate(steps~interval, data=weekday_frame, FUN=mean, na.rm=TRUE)
    time_series_weekend <- aggregate(steps~interval, data=weekend_frame, FUN=mean, na.rm=TRUE)

    plot(x = time_series_weekday$interval, y = time_series_weekday$steps, type = "l")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)

    plot(x = time_series_weekend$interval, y = time_series_weekend$steps, type = "l")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-2.png)
