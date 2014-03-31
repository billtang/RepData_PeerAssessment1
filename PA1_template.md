# Reproducible Research: Peer Assessment 1

This assignment uses data from personal device to provide an analysis of activity patterns. The data is steps taken as collected every 5 minutes from 10/1/2012 through 11/30/2012.


## Loading and preprocessing the data

The original data is in a zip file. It is downloaded and read as a data frame. The date and the interval columns are converted to form a posix date time stamp column.

```r
#
# Loading the data
#
getData<-function() {
    myurl<-'http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
    mytemp<-tempfile()
    download.file(myurl,mytemp)
    unzip(mytemp, list=T)
    myfilename<-unzip(mytemp)
    myfilename
}
#
# Pre-process the data and transform it for analysis
#
file2Data<-function(myfilename) {
    mydata<-read.csv(myfilename,sep=',')
    mydata$interval<-as.numeric(mydata$interval)
    mydata$hour<-trunc(mydata$interval/100) # hours
    mydata$min<-100*(mydata$interval/100 - mydata$hour) # minutes
    mydata$time<-sprintf('%s:%s:00', mydata$hour, mydata$min) # hh:mm:00
    mydata$posix<-as.POSIXct(paste(mydata$date, mydata$time)) # posix time of DateTime
    mydata
}

myfile<-getData() # myfile is 'activity.csv'
mydata<-file2Data('activity.csv')
```

## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
