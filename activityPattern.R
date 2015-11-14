library(ggplot2)

# unzip and load dataset
unzip("activity.zip", "activity.csv")
activity <- read.csv("activity.csv")

# What is mean total number of steps taken per day?
stepsPerDay <- aggregate(steps~date, activity, sum)
qplot(steps, data=stepsPerDay)

meanOfstepsPerDay = mean(stepsPerDay$steps)
medianOfstpesPerDay = median(stepsPerDay$steps)

# What is the average daily activity pattern?
aveOfActPattern <- aggregate(steps~interval, activity, mean)
qplot(interval, steps, data=aveOfActPattern, geom=c("line"))

maxInterval = aveOfActPattern[which.max(aveOfActPattern$steps),]$interval

# Imputing missing values
#       more simple and sophisticated method
totalNumOfNA <- table(complete.cases(activity))[1]

medianOfActPattern <- aggregate(steps~interval, activity, median)
imputedAct <- activity
for (i in 1:nrow(imputedAct)) {
        imputedStep <- imputedAct[i,]
        if (is.na(imputedStep$steps)) {
                imputedAct[i,]$steps <- aveOfActPattern[imputedStep$interval==aveOfActPattern$interval,]$steps
        }
}

imputedStepsPerDay <- aggregate(steps~date, imputedAct, sum)
qplot(steps, data=imputedStepsPerDay)

meanOfImputedStepsPerDay = mean(imputedStepsPerDay$steps)
medianOfImputedStpesPerDay = median(imputedStepsPerDay$steps)

# Are there differences in activity patterns between weekdays and weekends?
imputedAct$isWeekday <- ifelse(weekdays(as.Date(imputedAct$date), abbreviate=TRUE) %in% c("Sat", "Sun"), "weekend", "weekday")
imputedAct$isWeekday <- as.factor(imputedAct$isWeekday)

imputedAveOfActPattern <- aggregate(steps~interval+isWeekday, imputedAct, mean)
qplot(interval, steps, data=imputedAveOfActPattern, facets = isWeekday~., geom=c("line"))
