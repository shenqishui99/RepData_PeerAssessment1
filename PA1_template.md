---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


```r
## Loading and preprocessing the data
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
library(ggplot2)
data <- read.csv("activity.csv")
complete_data <- na.omit(data)
head(data)
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
## What is mean total number of steps taken per day?
sum(complete_data$steps)
```

```
## [1] 570608
```

```r
hist(complete_data$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->



```r
steps.mean <- mean(complete_data$steps)
steps.median <- median(complete_data$steps)
steps.mean
```

```
## [1] 37.3826
```

```r
steps.median
```

```
## [1] 0
```



```r
## What is the average daily activity pattern?
act.patt <- group_by(complete_data, interval)
act.patt <- summarize(act.patt, steps=mean(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
#Time series plot
ggplot(act.patt, aes(interval, steps)) + geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


```r
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
act.patt[act.patt$steps == max(act.patt$steps),]
```

```
## # A tibble: 1 x 2
##   interval steps
##      <int> <dbl>
## 1      835  206.
```

```r
#835th interval
```





```r
## Imputing missing values
#number of missing values
nrow(data)-nrow(complete_data)
```

```
## [1] 2304
```

```r
#create new data and impute NA value
data.new <- data
data.new$steps[is.na(data.new$steps)] <- steps.mean 
summary(data.new)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 37.38                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0
```

```r
#Create histogram
data.new <- group_by(data.new, date)
data.new <- summarize(data.new, steps=sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
hist(data.new$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->



```r
#Calculate mean and median
mean(data.new$steps)
```

```
## [1] 10766.19
```

```r
median(data.new$steps)
```

```
## [1] 10766.19
```




```r
## Are there differences in activity patterns between weekdays and weekends?
data.new <- data
data.new$steps[is.na(data.new$steps)] <- steps.mean 
data.new$dayofweek <- weekdays(as.Date(data.new$date))
data.new$weekend <-as.factor(data.new$dayofweek=="Saturday"|data.new$dayofweek=="Sunday")
levels(data.new$weekend) <- c("Weekday", "Weekend")

meansteps <- data.new %>% 
            group_by(interval,weekend) %>% 
            summarise(average = mean(steps))
```

```
## `summarise()` regrouping output by 'interval' (override with `.groups` argument)
```

```r
qplot(interval,average,data=meansteps,geom="line",facets=weekend~.,xlab="5-minute interval",ylab="average number of steps",main="Average steps pattern between Weekday and Weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

