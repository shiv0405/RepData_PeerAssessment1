---
  title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
  keep_md: true
---


```r
library(ggplot2)
library(lattice)
knitr::opts_chunk$set(echo = TRUE)
## Loading and preprocessing the data
raw_data <- read.csv(unz("activity.zip", "activity.csv"))
imp_data <- na.omit(raw_data)

## What is mean total number of steps taken per day?
step_a_day <- aggregate(imp_data$steps, by = list(Steps.Date = imp_data$date), FUN = "sum")
hist(step_a_day$x, col = "green", breaks = 20,
     main = "Total number of steps taken each day",
     xlab = "Number of steps per day")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
average_step <- mean(step_a_day[,2])
print(average_step)
```

```
## [1] 10766.19
```

```r
median_step <- median(step_a_day[,2])
print(median_step)
```

```
## [1] 10765
```

```r
## What is the average daily activity pattern?
ave_day <- aggregate(imp_data$steps,
                     by = list(Interval = imp_data$interval),
                     FUN = "mean")
plot(ave_day$Interval, ave_day$x, type = "l",
     main = "Average daily activity pattern",
     ylab = "Average number of steps taken",
     xlab = "5-min Intervals")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png)

```r
inter <- which.max(ave_day$x)
max_interval <- ave_day[inter, 1]
print(max_interval)
```

```
## [1] 835
```

```r
## Imputing missing values
NA_numb <- length(which(is.na(raw_data$steps)))
print (NA_numb)
```

```
## [1] 2304
```

```r
library(Hmisc)
```

```
## Warning: package 'Hmisc' was built under R version 4.0.5
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## Warning: package 'Formula' was built under R version 4.0.3
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, units
```

```r
raw_data_filled <- raw_data
raw_data_filled$steps <- impute(raw_data$steps, fun=mean)

step_noNA <- aggregate(raw_data_filled$steps,
                       by = list(Steps.date = raw_data_filled$date),
                       FUN = "sum")
hist(step_noNA$x, col = "green",
     breaks = 20,
     main = "Total number of steps taken each day",
     xlab = "Number of steps per day")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-3.png)

```r
average_noNA <- mean(step_noNA[,2])
print(average_noNA)
```

```
## [1] 10766.19
```

```r
median_noNA <- median(step_noNA[,2])
print(median_noNA)
```

```
## [1] 10766.19
```

```r
## Are there differences in activity patterns between weekdays and weekends?
raw_data_filled$date <- as.Date(raw_data_filled$date)
raw_data_filled$weekday <- weekdays(raw_data_filled$date)
raw_data_filled$type_of_day <- ifelse(raw_data_filled$weekday=="Saturday" | 
                                        raw_data_filled$weekday=="Sunday","Weekend","Weekday")
raw_data_filled$type_of_day <- factor(raw_data_filled$type_of_day)

data_of_day <- aggregate(steps ~ interval + type_of_day, data=raw_data_filled, mean)

ggplot(data_of_day, aes(interval, steps)) +
  geom_line() +
  facet_grid(type_of_day ~ .) +
  xlab("5-minute intervals") +
  ylab("Average number of steps taken") +
  ggtitle("Weekdays and weekends activity patterns")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-4.png)
