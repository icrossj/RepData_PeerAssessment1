# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Have the activity.csv extracted from the .zip before proceeding.


```r
activity <- read.csv("activity.csv",stringsAsFactors = FALSE)
```


## What is mean total number of steps taken per day?

This is a histogram of the total number of steps taken per day.


```r
a <- tapply(activity$steps,activity$date,sum, na.rm=TRUE)
hist(a,main="Total Number of Steps per Day",xlab = "Total Steps")
```

![](PA1_template_files/figure-html/Making_Histograms-1.png) 

```r
#hist(a)
```

The mean and median are reported below.


```r
mean(a)
```

```
## [1] 9354.23
```

```r
median(a)
```

```
## [1] 10395
```


## What is the average daily activity pattern?


```r
a0 <- tapply(activity$steps,activity$interval,mean, na.rm=TRUE)
answer = as.data.frame(as.table(a0)) 
colnames(answer) <- c("Interval","AvgSteps")
#time_interval <- as.numeric(levels(answer$Interval))
#avg_steps <- as.numeric(answer$AvgSteps)
answer$Interval <- as.numeric(levels(answer$Interval))
answer$AvgSteps <- as.numeric(answer$AvgSteps)

#plot(answer$Interval,answer$AvgSteps,type="l")
#plot(time_interval,avg_steps,type="l", ylab ="Average Number of Steps", xlab="")
plot(answer$Interval,answer$AvgSteps,type="l", ylab ="Average Number of Steps", xlab="", main="Average Daily Pattern")
```

![](PA1_template_files/figure-html/Avg_Daily_Activity-1.png) 

For part 2, which interval contains max step?


```r
maxSteps <- max(answer$AvgSteps)
answer[answer$AvgSteps == maxSteps,]
```

```
##     Interval AvgSteps
## 104      835 206.1698
```

Interval 835 has the most steps with Avg of 206 steps



## Imputing missing values

This following code will search out the rows that have NA steps and replace them with the mean over all days during the 5 minute interval.


```r
#generate a numeric vector with same number of rows as the activity set
new_activity_set <- rep(NA,nrow(activity))

#poor function to output the average steps when inputting a 5 minute time interval.
#   the averaged 5 minute interval is not explicitly defined. use lexical scoping
#   "answer" is defined in the previous block.
find_mean_steps <- function(interval){
  temp <- answer[answer$Interval == interval,] #hope it finds it...
  temp2 <- as.numeric(temp[2])
  return(temp2)
}


#simple condition check - if it is NA, it will replace with 5 min interval mean
impute <- function(row_feed){
  condition <- is.na(row_feed)
  if(condition[1]){
    int_steps <- as.numeric(row_feed[3])
    steps <- find_mean_steps(int_steps) #return the 5 minute interval mean
    return (steps)
  }
  else{
    steps <- row_feed[1] #return steps
    return (steps)
  }
}

#using a for loop and a custom function to do the replacement
#inefficient, but works

for(i in 1:nrow(activity)){
  new_activity_set[i] <- impute(activity[i,])
}

activity_new_steps <- as.numeric(new_activity_set)
activity_new <- cbind(activity_new_steps,activity)

a_new <- tapply(activity_new$activity_new_steps,activity_new$date,sum, na.rm=TRUE)

a_new <- tapply(activity_new$activity_new_steps,activity_new$date,sum, na.rm=TRUE)
hist(a_new,main="Total Number of Steps per Day",xlab = "Total Steps")
```

![](PA1_template_files/figure-html/Impute_missing_values-1.png) 

```r
#hist(a)
```

Yes, there are more days where the total number of steps are in the range of 10,000 and 15,000.

The mean and median are reported below.


```r
mean(a_new)
```

```
## [1] 10766.19
```

```r
median(a_new)
```

```
## [1] 10766.19
```


The mean and median are 10766.19 after imputing. Before imputing, it was 9354.23 and 10395. After imputing, the mean and median are closer to each other.



## Are there differences in activity patterns between weekdays and weekends?



```r
par(mfcol  = c(2,1))

#convert date -> date object
activity_new$date <- strptime(activity_new$date,"%Y-%m-%d")

# add a new column for days of the week
which_day <- weekdays(activity_new$date)
activity_new <- cbind(activity_new,which_day)

# subset into a weekday and weekend, then apply the same codelines as part 3
a_weekday <- subset(activity_new, which_day %in%  c("Monday","Tuesday","Wednesday","Thursday","Friday"))
a_weekend <- subset(activity_new, which_day %in%  c("Saturday","Sunday"))


# making histograms of weekday and weekend


#weekday plot
a_weekday1 <- tapply(a_weekday$activity_new_steps,a_weekday$interval,mean, na.rm=TRUE)
answer_weekday = as.data.frame(as.table(a_weekday1)) 
colnames(answer_weekday) <- c("Interval","AvgSteps")
answer_weekday$Interval <- as.numeric(levels(answer_weekday$Interval))
answer_weekday$AvgSteps <- as.numeric(answer_weekday$AvgSteps)

plot(answer_weekday$Interval,answer_weekday$AvgSteps,type="l", ylab ="Average Number of Steps", xlab="", main="Weekday Mean Steps")

#weekend plot
a_weekend1 <- tapply(a_weekend$activity_new_steps,a_weekend$interval,mean, na.rm=TRUE)
answer_weekend = as.data.frame(as.table(a_weekend1)) 
colnames(answer_weekend) <- c("Interval","AvgSteps")
answer_weekend$Interval <- as.numeric(levels(answer_weekend$Interval))
answer_weekend$AvgSteps <- as.numeric(answer_weekend$AvgSteps)

plot(answer_weekend$Interval,answer_weekend$AvgSteps,type="l", ylab ="Average Number of Steps", xlab="", main="Weekend Mean Steps")
```

![](PA1_template_files/figure-html/Differences_in_Weekdays_and_Weekends-1.png) 


Yes, there seems to be more activity on the weekends than the weekdays. Perhaps people go out to do recreation on those days.
