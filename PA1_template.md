# Reproducible Research: Peer Assessment 1


## Loading  the data

```r
fileName='activity.csv'

myDatasetGross <- read.csv2(file =fileName,
                       header = TRUE, 
                       sep = ",",
                       stringsAsFactors= FALSE)
```

## Preprocessing the data

```r
# convert type
myDatasetGross$steps.int <- as.integer(myDatasetGross$steps)
myDatasetGross$date.dateVal <- as.Date(myDatasetGross$date,format="%Y-%m-%d")
myDataset <- subset(myDatasetGross,!(is.na(myDatasetGross$steps)))
```

###Calculate the total number of steps taken per day

```r
stepPerDay<- aggregate(myDataset$steps.int, by=list(date=myDataset$date.dateVal), FUN=sum)
# redefine columns name
colnames(stepPerDay) <- c( "day","steps")
```
---
If you do not understand the difference between a histogram and a barplot, research the difference between them. 
Make a histogram of the total number of steps taken each day
---
###Plot the histograme


```r
hist(stepPerDay$steps,main ="total number of steps taken each day",
     ylim=c(0, 10),
     nclass=20,xlim=c(0,25000),
     col="cornflowerblue", border="blue",
     ylab="Density",
     xlab = "total number of steps per day")
```

![](PA1_template_files/figure-html/histograme total_number_of_steps_taken_per_day-1.png) 

## What is mean total number of steps taken per day?

```r
sumResult<- aggregate(myDataset$steps.int, by=list(date=myDataset$date.dateVal), FUN=mean)
colnames(sumResult) <- c( "day","steps")
plot(sumResult$day,
     sumResult$steps,
     main =" mean of : total number steps taken per day",
     type = "l",
     ylab = "steps",
     xlab="")
```

![](PA1_template_files/figure-html/mean_total_number_of_steps_taken_per_day-1.png) 

---
Calculate and report the mean and median of the total number of steps taken per day
---

```r
meanStepPerDay<-mean(sumResult$steps) 
medianStepPerDay<-median(sumResult$steps) 

cat(sprintf("mean step per day = %2.4f \n", meanStepPerDay))
```

```
## mean step per day = 37.3826
```

```r
cat(sprintf("median step per day = %2.4f \n", medianStepPerDay))
```

```
## median step per day = 37.3785
```

## What is the average daily activity pattern?

```r
# compute the mean value 
AverageStepPerInterval<- aggregate(myDataset$steps.int,  by=list(myDataset$interval), FUN=mean)
colnames(AverageStepPerInterval) <- c( "interval_jour","steps")

plot(AverageStepPerInterval,
     type = "l",
      xaxt="n",
     main = "the average(mean) number of steps taken, averaged across all days",
     xlab="")


# redefine axis 1200 become 12:00
xtick<-seq(0, 2400, by=400)
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,offset=1 , par("usr")[3],labels =sprintf("%d:%.2d \n",xtick%/%100, xtick%%100), srt = 0, pos = 1, xpd = TRUE)

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxStep<-AverageStepPerInterval$interval_jour[which (AverageStepPerInterval$steps==max(AverageStepPerInterval$steps))]
abline(v = maxStep, col = "red", lwd = 2)
```

![](PA1_template_files/figure-html/verage_daily_activity_pattern-1.png) 


```r
cat(sprintf("5-minute interval with maximum number of steps = %d:%d \n", maxStep%/%100, maxStep%%100))
```

```
## 5-minute interval with maximum number of steps = 8:35
```

## Imputing missing values

---
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
---

```r
numberNa <-sum(is.na(myDatasetGross))

cat(sprintf("number of Na :%d/%d total rows \n",numberNa, nrow(myDatasetGross)))
```

```
## number of Na :4608/17568 total rows
```

---
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5minute interval, etc.
---
# missing value fill with mean value 

```r
texte <-" I decide to fill empty values with the mean value"
texte
```

```
## [1] " I decide to fill empty values with the mean value"
```

```r
# find the mean value for the data set 
grossMean<-mean(myDatasetGross$steps.int,na.rm = TRUE)
# copy a new data.frame
myDataFillMissingValue <-myDatasetGross
# fill new dataframe na value
myDataFillMissingValue[is.na(myDataFillMissingValue)] <- grossMean
```
--
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
--

```r
stepPerDayFill<- aggregate(myDataFillMissingValue$steps.int, 
                           by=list(date=myDataFillMissingValue$date.dateVal), FUN=sum)
colnames(stepPerDayFill) <- c( "day","steps")


hist(stepPerDayFill$steps,
     main ="total number of steps taken each day (filled missing values)",
     breaks=30, 
     ylim=c(0, 10),
     xlim=c(0,25000),
     col="cornflowerblue", border="blue",
     ylab="Density",
     xlab = "mean total number of steps taken per day")

abline(v = mx <- mean(stepPerDayFill$steps), col = "red", lwd = 2)
abline(v = mx <- median(stepPerDayFill$steps), col = "yellow", lwd = 2)

title=c("mean","median")
#liste of color
colors=c("red","yellow")
legend("topright",lty = "solid" , col =colors,bg="grey", legend = title,cex=0.7,)
```

![](PA1_template_files/figure-html/histograme_fill_missing_value-1.png) 



## Are there differences in activity patterns between weekdays and weekends?

```r
# convert date to number_day_of week (0 = sunday, 1= monday .. 6 = saturday)
myDataFillMissingValue$day.int<-strftime(myDataFillMissingValue$date.dateVal,'%w')
# define saturday and sunday = week end otherwise weekday 
myDataFillMissingValue$day.weekDay<-ifelse((myDataFillMissingValue$day.int==6)|(myDataFillMissingValue$day.int==0),0,1)
```


```r
# split originale dataframe into 2 dataframe: weekday and week end
stepPerDayFillWeekEnd<-subset(myDataFillMissingValue,
                              (myDataFillMissingValue$day.weekDay==0),
                              c( interval,steps.int))

stepPerDayFillWeek<-subset(myDataFillMissingValue,myDataFillMissingValue$day.weekDay==1,
                           c( interval,steps.int))


# compute the mean for each dataframe 
meanWeek<- aggregate(stepPerDayFillWeek$steps.int, 
                     by=list(date=stepPerDayFillWeek$interval), FUN=mean)
colnames(meanWeek) <- c( "interval","steps")

meanWeekEnd<- aggregate(stepPerDayFillWeekEnd$steps.int, 
                     by=list(date=stepPerDayFillWeekEnd$interval), FUN=mean)
colnames(meanWeekEnd) <- c( "interval","steps")
```


```r
# plot de canevas
plot(meanWeekEnd,
     type = "n",
     ylab = "average steps",
     xlab="",las=2,xaxt="n")

# plot curve weekend
points(meanWeekEnd$interval,
       meanWeekEnd$steps,
       type="l",col='black')

# plot curve weekday
points(meanWeek$interval,
       meanWeek$steps,
       type="l",col='red')

# plot title and  label

#liste au title
title=c("weekDay","WeekEnd")
#liste of color
colors=c("red","black")
legend("topright",lty = "solid" , col =colors, legend = title,cex=0.7,)

# redefine axis 1200 become 12:00
xtick<-seq(0, 2400, by=400)
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,offset=1 , par("usr")[3],labels =sprintf("%d:%.2d \n",xtick%/%100, xtick%%100), srt = 0, pos = 1, xpd = TRUE)
```

![](PA1_template_files/figure-html/plot_day_week-1.png) 
