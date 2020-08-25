---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(data.table)
```

```
## 
## Attaching package: 'data.table'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     between, first, last
```

```r
library(ggplot2)
library(imputeR)
path <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, file.path(path, "activity.zip"))
unzip(zipfile = "activity.zip")
data <- fread(file.path(getwd(), "activity.csv"))
```



## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
tsteps <- data.frame(steps = tapply(data$steps, data$date, sum))
head(tsteps)
```

```
##            steps
## 2012-10-01    NA
## 2012-10-02   126
## 2012-10-03 11352
## 2012-10-04 12116
## 2012-10-05 13294
## 2012-10-06 15420
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
ggplot(tsteps, aes(steps)) +
        geom_histogram(fill = "red", na.rm = TRUE, binwidth = 1000) +
        labs(title ="Steps taken each day", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

Mean

```r
mean(tsteps$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```
Median

```r
median(tsteps$steps, na.rm = TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
data2 <- as_tibble(data)
data2 <- data2 %>%
        select(c(steps, interval)) %>%
        group_by(interval) %>%
        summarise(avg = mean(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
ggplot(data2, aes(interval, avg)) +
        geom_line(color = "red", size = 1) +
        labs(title = "Average Daily Steps", x = "Interval", y = "Avg. Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
filter(data2, avg == max(avg))
```

```
## # A tibble: 1 x 2
##   interval   avg
##      <int> <dbl>
## 1      835  206.
```


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)


```r
data2 <- as_tibble(data)
data2 %>% 
        select(steps) %>% 
        filter(is.na(steps)) %>% 
        summarise(missing = n())
```

```
## # A tibble: 1 x 1
##   missing
##     <int>
## 1    2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Imputed by the mean of 5-minute intervals

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
tidy <- data2 %>% 
        group_by(interval) %>% 
        mutate(steps = guess(steps, type = "mean")) %>% 
        ungroup()
head(tidy)
```

```
## # A tibble: 6 x 3
##   steps[,1] date       interval
##       <dbl> <date>        <int>
## 1    1.72   2012-10-01        0
## 2    0.340  2012-10-01        5
## 3    0.132  2012-10-01       10
## 4    0.151  2012-10-01       15
## 5    0.0755 2012-10-01       20
## 6    2.09   2012-10-01       25
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
tsteps <- data.frame(steps = tapply(tidy$steps, tidy$date, sum))
ggplot(tsteps, aes(steps)) +
        geom_histogram(fill = "red", na.rm = TRUE, binwidth = 1000) +
        labs(title ="Steps taken each day", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Mean

```r
mean(tsteps$steps)
```

```
## [1] 10766.19
```
Median

```r
median(tsteps$steps)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
data2 <- tidy
data2$day <- weekdays(data2$date)
data2[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = data2$day), "day"] <- "Weekday"
data2[grepl(pattern = "Saturday|Sunday", x = data2$day), "day"] <- "Weekend"
data2$day <- as.factor(data2$day)
```

2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
data2 <- data2 %>%
        select(c(steps, interval, day)) %>%
        group_by(day, interval) %>%
        summarise(avg = mean(steps, na.rm = TRUE))
```

```
## `summarise()` regrouping output by 'day' (override with `.groups` argument)
```

```r
ggplot(data2, aes(interval, avg, color = day)) +
        geom_line(size = 1) + facet_grid(day~.) +
        labs(title = "Average Daily Steps by Weektype", x = "Interval", y = "Avg. Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
