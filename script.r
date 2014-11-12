setwd ("/home/nikita/R/RR/RepData_PeerAssessment1")
library(lattice)

if (!file.exists("activity.csv")) {unzip("activity.zip")}

activity <- read.table ("activity.csv", sep = ',', header = TRUE)

steps_by_day <- aggregate (steps~date, data=activity, sum)
hist (steps_by_day$steps, main="Histogram of the daily total number of steps", xlab= "Daily total steps number", col="red", breaks = 12 )

mean (steps_by_day$steps)

median (steps_by_day$steps)

day_pattern <- aggregate (steps~interval, data=activity, mean, na.rm=TRUE)
plot (day_pattern, type = "l", main = "Average daily activity pattern")
day_pattern$interval[which.max(day_pattern$steps)]

sum(is.na(activity$steps))

complete <- activity

for (i in 1:nrow(complete)){
        if (is.na(complete$steps[i])){
                complete$steps[i] <- day_pattern$steps[day_pattern$interval == complete$interval[i]]
        }       
}

steps_by_day_complete <- aggregate (steps~date, data=complete, sum)
hist (steps_by_day_complete$steps, main="Total number of steps per day", xlab= "Daily total steps number", col="red", breaks = 12 )

mean (steps_by_day_complete$steps)

median (steps_by_day_complete$steps)

complete$date <- as.Date(complete$date)


complete$day <- factor(ifelse(weekdays(complete$date) %in% c('Saturday','Sunday'), 'weekend', 'weekday'), levels = c('weekend','weekday'))
pattern_by_day_type <- aggregate (steps~interval+day, data=complete, mean)


xyplot(steps ~ interval|day, data = pattern_by_day_type, type = 'l', layout = c(1,2))