# Reproducible Research : Peer Assessment 1


```r
library(ggplot2)
```

## Load the data

```r
unzip("activity.zip", "activity.csv")
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

```r
stepsPerDay <- aggregate(steps~date, activity, sum, na.rm=TRUE, na.action=na.pass)
qplot(steps, data=stepsPerDay, xlab="total steps per day", binwidth=500)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean.stepsPerDay = mean(stepsPerDay$steps)
median.stepsPerDay = median(stepsPerDay$steps)
```

- Mean : 9354.2295082
- Median : 10395

## What is the average daily activity pattern?

```r
aveOfDailyActPattern <- aggregate(steps~interval, activity, mean, na.rm=TRUE, na.action=na.pass)

ggplot(data=aveOfDailyActPattern, aes(x=interval,y=steps)) + 
        geom_line() +
        xlab("5 minute interval") +
        ylab("average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

### Which 5-minute interval, on average across all the days in the dataset, contains the maxmimum number of steps?

```r
maxInterval = aveOfDailyActPattern[which.max(aveOfDailyActPattern$steps),]$interval
```

- Interval of the maximun number of steps : 835

## Imputing missing values
### Calculate and report the total number of missing values in the dataset

```r
totalNumOfNA <- length(which(is.na(activity$steps)))
```
- the total number of missing values : 2304

### Devise the strategy for filling in all of the missing values in the dataset.

```r
medianOfDailyActPattern <- aggregate(steps~interval, activity, median)
```
### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
imputedActivity <- activity
imputedActivity$imputedStep <- ifelse(is.na(imputedActivity$steps), 
                               medianOfDailyActPattern$steps,
                               imputedActivity$steps)
```
### Make a histogram of the total number of steps taken each day.

```r
imputedStepsPerDay <- aggregate(imputedStep~date, imputedActivity, sum, na.rm=TRUE, na.action=na.pass)
qplot(imputedStep, data=imputedStepsPerDay, xlab="total steps per day (imputed)", binwidth=500)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

### Calculate and report the mean and median total number of steps taken per day.

```r
mean.imputedStepsPerDay = mean(imputedStepsPerDay$imputedStep)
median.imputedStepsPerDay = median(imputedStepsPerDay$imputedStep)
```
- Mean (imputed dataset) : 9503.8688525
- Medina (imputed dataset) : 10395

## Are there differences in activity patterns between weekdays and weekends?
### Create a new factor variable in the dataset with two levels - "weekday" and "weekend"

```r
imputedActivity$isWeekday <- ifelse(weekdays(as.Date(imputedActivity$date), abbreviate=TRUE) %in% c("Sat", "Sun"), "weekend", "weekday")
imputedActivity$isWeekday <- as.factor(imputedActivity$isWeekday)
```
### Make a panel plot

```r
imputedAveOfActPattern <- aggregate(imputedStep~interval+isWeekday, imputedActivity, mean, na.rm=TRUE, na.action=na.pass)
ggplot(imputedAveOfActPattern, aes(x=interval, y=imputedStep)) +
        geom_line() +
        facet_grid(isWeekday~.) + 
        labs(x="5 minute interval", y="average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 
