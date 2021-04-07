---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
df<-read.csv("activity.csv")
library(ggplot2)
```

## What is mean total number of steps taken per day?

```r
Ts<-tapply(df$steps,df$date,sum,na.rm=TRUE)

qplot(Ts,binwidth=1000,xlab = "total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
summary(Ts)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```
## What is the average daily activity pattern?

```r
As <- aggregate(x = list(steps = df$steps), by = list(interval = df$interval),FUN = mean, na.rm = TRUE)

ggplot(data = As, aes(x = interval, y = steps)) + geom_line() + xlab("5-minute interval") + ylab("average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
As$interval[max(As$steps)==As$steps]
```

```
## [1] 835
```
## Imputing missing values

```r
sum(is.na(df$steps))
```

```
## [1] 2304
```

```r
fill.value <- function(steps, interval) {
     filled <- NA
     if (!is.na(steps)) {
         filled <- c(steps)}
     else{
         filled <- (As[As$interval == interval, "steps"])}
     return(filled)}

filled.df <- df
filled.df$steps <- mapply(fill.value, filled.df$steps, filled.df$interval)

Ts.steps <- tapply(filled.df$steps, filled.df$date, FUN = sum)
qplot(Ts.steps, binwidth = 1000, xlab = "total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
summary(Ts)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```
## Are there differences in activity patterns between weekdays and weekends?

```r
 weekday.or.weekend <- function(date) {
     day <- weekdays(date)
     if (day %in% c("Δευτέρα", "Τρίτη", "Τετάρτη", "Πέμπτη", "Παρασκευή")){
         return("weekday")}
     else if (day %in% c("Σάββατο", "Κυριακή")){ 
         return("weekend")}
 }
filled.df$date <- as.Date(filled.df$date)
filled.df$day <- sapply(filled.df$date, FUN=weekday.or.weekend)
averages <- aggregate(steps ~ interval + day, data=filled.df, mean)

ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + xlab("5-minute interval") + ylab("Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
