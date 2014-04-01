# Reproducible Research: Peer Assessment 1

This assignment uses data from personal device to provide an analysis of activity patterns. The data is steps taken as collected every 5 minutes from 10/1/2012 through 11/30/2012.


## Loading and preprocessing the data

The original data is in a zip file. It is downloaded and read as a data frame. The date and the interval columns are converted to form a posix date time stamp column.

```r
#
# Download the data. Get filename after unzip
#
getFile<-function() {
    myurl<-'http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
    mytemp<-tempfile()
    download.file(myurl,mytemp)
    unzip(mytemp, list=T)
    myfilename<-unzip(mytemp)
    myfilename
}
#
# Pre-process the data given filename. Transform it for analysis
#
file2Data<-function(myfilename) {
    mydata<-read.csv(myfilename,sep=',')
    mydata$interval<-as.numeric(mydata$interval)
    mydata$hour<-trunc(mydata$interval/100) # hours
    mydata$min<-100*(mydata$interval/100 - mydata$hour) # minutes
    mydata$time<-sprintf('%s:%s:00', mydata$hour, mydata$min) # hh:mm:00
    mydata$posix<-as.POSIXct(paste(mydata$date, mydata$time)) # posix time of DateTime
    mydata$wd<-weekdays(mydata$posix) # used for calculating weekend
    mydata
}

myfile<-getFile() # myfile is 'activity.csv'
mydata<-file2Data('activity.csv')
```

Examine the data:

```r
summary(mydata$steps)
```
Result:
```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
   0.00    0.00    0.00   37.38   12.00  806.00    2304
```

## What is mean total number of steps taken per day?

We aggregate the steps of each interval of 5 minutes by date. The mean is about 10766 steps per day.
```r
# daily steps
#   mydaily<-getDaily(mydata)
#
getDaily<-function(myd) {
    mydaily<-aggregate(steps ~ date, myd, sum)
    mydaily    
}

```
Another way of getting daily data:

```r
library(plyr)
getDaily<-function(myd) {
    myd<-myd[!is.na(myd$steps),] 
    ddply(myd, .(date), summarize,
          mean=round(mean(steps),2),
          median=round(median(steps),2),
          sum=round(sum(steps),2)
          )
}
plotDaily2File<-function(mydaily,myfile='') {
    plotDaily(mydaily)
    ggsave(file=myfile)
    dev.off()
}

```

## What is the average daily activity pattern?

We draw a histogram, and show the mean and median.

```r
#
# plot daily
#   plotDaily(mydaily)
#
plotDaily <- function(mydaily, myfile='') {
    mymean<-mean(mydaily$steps)
    mymedian<-median(mydaily$steps)
    mylabels=c(paste('Mean:', mymean), paste('Median:', mymedian))
    cols = c('blue', 'orange')
    if (myfile!='') { png(myfile) }
    ggplot(mydaily, aes(x=steps)) +
        geom_histogram(fill='black') +      #,binwidth=1500) +
        geom_point(aes(x=mymean, y=0, color='blue'), size=4, shape=25) + 
        geom_point(aes(x=mymedian, y=0, color='orange'), size=4, shape=24) + 
        scale_color_manual(name=element_blank(), labels=mylabels, values=cols) + 
        labs(title='Histogram of daily steps taken',
             x='Number of steps', y='Count') + 
        theme(legend.position = 'bottom')
    if (myfile!='') { devoff() }
}

mydaily<-getDaily(mydata)
plotDaily(mydaily,'stepsDaily.png')

```


### Sum and plot of steps of 5 minute intervals.


```r
#
# 
#
intervalData<-function(myd) {
    # na.rm=T  otherwise you will get NA as result for the column values
    mydata<-ddply(myd, .(interval), summarize, 
                  mean=round(mean(steps,na.rm=T),2),
                  median=round(median(steps,na.rm=T),2),
                  sum=round(sum(steps,na.rm=T),2)
                  )    
    mydata$interval<-as.numeric(mydata$interval)
    mydata$hour<-trunc(mydata$interval/100) # hours
    mydata$min<-100*(mydata$interval/100 - mydata$hour) # minutes
    mydata$time<-sprintf('%s:%s:00', mydata$hour, mydata$min) # hh:mm:00
    #mydata$posix<-as.POSIXct(paste(mydata$date, mydata$time)) # posix time of DateTime
    mydata    
}
```

### Time series plot
```r
#
# Plot time series data:
#
#  tsplot( myseg$sum )
#
tsplot<-function(myseg, mytext='', myfile='') {
    if (mytext!='') { mymain=paste(mytext,'Average daily step pattern per interval' )}
    else{
        mymain='Average daily step pattern per interval'
    }
    if (myfile!='') { png(filename=myfile) }
    plot(myseg$sum ~ myseg$interval, type='l',
         xlab='Interval (max segment show in red)',
         ylab='Steps', main=mymain)
    mymax<-getIntervalMax(myseg)
    points( x=mymax, y=0, pch=20, cex=3, col='red')
    if (myfile!='') { dev.off() }
}
```
```r
myseg<-intervalData(mydata)
tsplot(myseg,'seg.png')  # interval plot
```
## Are there differences in activity patterns between weekdays and weekends?

We may filter by category whether it is of weekend:
```r
intervalDataWeekday<-function(myd) {
    mytemp<-subset( myd, ! myd$wd %in% c('Saturday','Sunday'))
    intervalData(mytemp)
}
intervalDataWeekend<-function(myd) {
    mytemp<-subset( myd, myd$wd %in% c('Saturday','Sunday'))
    intervalData(mytemp)
}

```
We will plot these side by side:
```r
tsplot2<-function(mydata,myfile='') {
    mysegwe = intervalDataWeekend(mydata)
    mysegwd = intervalDataWeekday(mydata)
    if (myfile!='') { png(filename=myfile) }
    par(mfrow=c(2,1))
    tsplot(mysegwe, 'weekend')
    tsplot(mysegwd, 'weekday')
    if (myfile!='') { dev.off() }    
}

```
```r
tsplot2(mydata,'ww.png')  # weekday vs weekend interval plot
```



