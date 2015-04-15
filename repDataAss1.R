#load data and packages
data <- read.csv("./RepData_PeerAssessment1/activity.csv")
library (lattice)
library (lubridate)

#PART ONE What is the mean total number of steps taken per day(ignore NAs)?
#1.Make a histogram of the total number of steps taken each day
data_naomit <- data[complete.cases(data),]
stepTotalByDay <- aggregate(steps ~ date, data_naomit, sum)
hist(stepTotalByDay$steps, breaks = 10, main = "Total Steps Per Day", xlab = "Steps")

#2. Calculate and report the mean and median total number of steps taken per day
mean(stepTotalByDay$steps)
median(stepTotalByDay$steps)
summary(stepTotalByDay$steps) #different results than previous two

#PART TWO What is the average daily activity pattern?

#1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all days (y-axis)
averageDailyPattern <- aggregate(steps ~ interval, data_naomit, mean)
with(averageDailyPattern, plot(interval, steps, type = 'l', main = "Average Steps Per Interval"))
summary(averageDailyPattern)

#2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maximumSteps <- averageDailyPattern[averageDailyPattern$steps == max(averageDailyPattern$steps), ]
maximumSteps$steps

#PART THREE Imputing missing values
#Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing 
#days may introduce bias into some calculations or summaries of the data.

#1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
naValues <- sum(is.na(data))
naValues  

#2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be 
#sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
meansVector<-tapply(data$steps,data$interval,mean,na.rm=TRUE) #vector of means for intervals
completeVector<-ifelse(is.na(data$steps), meansVector, data$steps) # if there is na value, place value from mean

#3.Create a new dataset that is equal to the original dataset but with the missing data filled in.
dataComplete<-data.frame(completeVector,data$date,data$interval) #reassemble complete dataframe with new mean for steps
names(dataComplete)<-names(data)
head(dataComplete)

#4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number 
#of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the 
#impact of imputing missing data on the estimates of the total daily number of steps?
dataCompleteStepTotal <- aggregate(steps ~ date, dataComplete, sum)
hist(dataCompleteStepTotal$steps, breaks = 10, main = "Total Steps Per Day", xlab= "Steps")
mean(dataCompleteStepTotal$steps)
median(dataCompleteStepTotal$steps)

#PART FOUR Are there differences in activity patterns between weekdays and weekends?

#For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values 
#for this part.

#Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given 
#date is a weekday or weekend day.
dataComplete$date2 <- as.Date(dataComplete$date, "%Y-%m-%d")
head(dataComplete)
dataComplete$dayofweek <- wday(dataComplete$date2, label = TRUE)
levels(dataComplete$dayofweek) <- c("weekend", rep("weekday", 5), "weekend")
dataComplete2 <- aggregate(steps ~ interval + dayofweek, dataComplete, mean)
head(dataComplete2)

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average 
#number of steps taken, averaged across all weekday days or weekend days (y-axis). 
#base package


xyplot (steps~interval | dayofweek, groups=dayofweek, data=dataComplete2, type="l",
        layout=c(1, 2), as.table=T, xlab="Interval", ylab="Number of Steps", main = "Average Steps Per Interval")
?xyplot

