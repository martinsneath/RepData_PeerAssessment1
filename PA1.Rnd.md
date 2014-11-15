# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
a=read.csv("activity.csv")
b=na.omit(a)
```
## What is mean total number of steps taken per day?

```r
steps=aggregate(b$steps,by=(list(b$date)),FUN=sum)
colnames(steps)=c("Date","Steps")
hist(steps$Steps,main="",xlab="Total steps per day")
```

![](PA1.Rnd_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean(steps$Steps)
```

```
## [1] 10766.19
```

```r
median(steps$Steps)
```

```
## [1] 10765
```

```r
## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
```
