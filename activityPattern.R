library(ggplot2)

# unzip and load dataset
unzip("activity.zip", "activity.csv")
activity <- read.csv("activity.csv")

# total steps per day
stepsPerDay <- aggregate(steps~date, activity, sum)
qplot(steps, data=stepsPerDay)

meanOfstepsPerDay = mean(stepsPerDay$steps)
medianOfstpesPerDay = median(stepsPerDay$steps)

# 
aveOfDailyActPattern <- aggregate(steps~interval, activity, mean)
qplot(interval, steps, data=aveOfDailyActPattern, geom=c("line"))

maxInterval = aveOfDailyActPattern[which.max(aveOfDailyActPattern$steps),]$interval