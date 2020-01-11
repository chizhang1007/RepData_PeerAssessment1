---
title: "Reproducible Research: Peer Assessment 1"
author: "Chi Zhang"
date: "January 7, 2019"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

First the data and libraries used are loaded using the following code.

```r
setwd("~/Documents/R Directory")
activity_data <- read.csv("activity.csv")
library(ggplot2)
library(dplyr)
```

The data set is explored.

```r
str(activity_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(activity_data)
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

The following code is used to remove NA values.

```r
activity_data_clean <- na.omit(activity_data)
```

## What is mean total number of steps taken per day?


```r
steps_perday <- group_by(activity_data_clean, date)
steps_perday <- summarize(steps_perday, steps=sum(steps))
hist(steps_perday$steps, breaks = 10, xlab = "Daily Steps")
```

![](Reproducible-Research-Project-1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean(steps_perday$steps)
```

```
## [1] 10766.19
```

```r
median(steps_perday$steps)
```

```
## [1] 10765
```
As shown in the histogram and the results from the previous code, the mean total number of steps taken per day is 10766.19, and the median is 10765.

## What is the average daily activity pattern?


```r
daily_pattern <- group_by(activity_data_clean, interval)
daily_pattern <- summarize(daily_pattern, steps=mean(steps))
ggplot(daily_pattern, aes(interval, steps)) + geom_line() + xlab("Interval") + ylab("Average Steps")
```

![](Reproducible-Research-Project-1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

To determine the 5-minute interval that, on average, contains the maximum number of steps, the following code is applied.

```r
max_interval <- max(daily_pattern$steps)
daily_pattern[daily_pattern$steps==max_interval, ]
```

```
## # A tibble: 1 x 2
##   interval steps
##      <int> <dbl>
## 1      835  206.
```
The interval "835"" contains the maximum number of steps on average, which is 206 steps.

## Imputing missing values
First, it is necessary to determine how many missing values are there in the data set.

```r
nrow(activity_data[is.na(activity_data$steps), ])
```

```
## [1] 2304
```
The total number of missing values in the dataset is 2304.

The strategy is to replace the missing values with the mean number of steps for each specific 5-min time interval. Those means were calculated in the previous section and were stored in daily_pattern.

```r
names(daily_pattern)[2] <- "mean"
complete_data <- merge(activity_data, daily_pattern)
complete_data$steps[is.na(complete_data$steps)] <- complete_data$mean[is.na(complete_data$steps)]
```
The histrogram of the total number of steps taken each day after missing values are imputed is shown below:

```r
complete_perday <- group_by(complete_data, date)
complete_perday <- summarize(complete_perday, steps = sum(steps))
hist(complete_perday$steps, breaks = 10, xlab = "Daily Steps")
```

![](Reproducible-Research-Project-1_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
mean(complete_perday$steps)
```

```
## [1] 10766.19
```

```r
median(complete_perday$steps)
```

```
## [1] 10766.19
```
The mean values for both datasets are the same, the median value for the complete data set is slightly higher than the old data set. Because we replaced the missing values with the mean of that time interval and those missing values were excluded from the calculation in the first section, the mean and median values for both old and new data sets are very close.

## Are there differences in activity patterns between weekdays and weekends?
First, it is necessary to identify the weekdays and weekends and create a new factor variable in the complete data set.

```r
complete_data$weekdaycatergory <- ifelse(weekdays(as.Date(complete_data$date)) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Weekday", "Weekend")
```

Next, we can draw activity pattern plots for both weekdays and weekends.

```r
library(plyr)
library(lattice)
week_pattern <- ddply(complete_data, .(interval, weekdaycatergory), summarize, steps=mean(steps))
xyplot(steps~interval|weekdaycatergory, data=week_pattern, type="l", layout= c(1,2), xlab="Interval", ylab="Number of Steps")
```

![](Reproducible-Research-Project-1_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

From the graphs above, it can be concluded that there are differences between people's activity patterns during weekdays and weekends.
