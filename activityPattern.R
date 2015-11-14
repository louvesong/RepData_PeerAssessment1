library(ggplot2)

# unzip and load dataset
unzip("activity.zip", "activity.csv")
activity <- read.csv("activity.csv")

# What is mean total number of steps taken per day?
stepsPerDay <- aggregate(steps~date, activity, sum, na.rm=TRUE, na.action=na.pass)
qplot(steps, data=stepsPerDay, xlab="total steps per day", binwidth=500)

mean.stepsPerDay = mean(stepsPerDay$steps)
median.stpesPerDay = median(stepsPerDay$steps)

# What is the average daily activity pattern?
aveOfDailyActPattern <- aggregate(steps~interval, activity, mean, na.rm=TRUE, na.action=na.pass)
#qplot(interval, steps, data=aveOfActPattern, geom=c("line")
ggplot(data=aveOfDailyActPattern, aes(x=interval,y=steps)) + 
        geom_line() +
        xlab("5 minute interval") +
        ylab("average steps")

maxInterval = aveOfDailyActPattern[which.max(aveOfDailyActPattern$steps),]$interval

# Imputing missing values
## the total number of missing values
totalNumOfNA <- length(which(is.na(activity$steps)))
## Create the imputed column by median value of interval
medianOfDailyActPattern <- aggregate(steps~interval, activity, median)
activity$imputedStep <- ifelse(is.na(activity$steps), 
                               medianOfDailyActPattern$steps,
                               activity$steps)

imputedStepsPerDay <- aggregate(imputedStep~date, activity, sum, na.rm=TRUE, na.action=na.pass)
qplot(imputedStep, data=imputedStepsPerDay, xlab="total steps per day (imputed)", binwidth=500)

mean.imputedStepsPerDay = mean(imputedStepsPerDay$imputedStep)
median.imputedStpesPerDay = median(imputedStepsPerDay$imputedStep)

# Are there differences in activity patterns between weekdays and weekends?
activity$isWeekday <- ifelse(weekdays(as.Date(activity$date), abbreviate=TRUE) %in% c("Sat", "Sun"), "weekend", "weekday")
activity$isWeekday <- as.factor(activity$isWeekday)

imputedAveOfActPattern <- aggregate(imputedStep~interval+isWeekday, activity, mean, na.rm=TRUE, na.action=na.pass)
ggplot(imputedAveOfActPattern, aes(x=interval, y=imputedStep)) +
        geom_line() +
        facet_grid(isWeekday~.) + 
        labs(x="5 minute interval", y="average steps")
