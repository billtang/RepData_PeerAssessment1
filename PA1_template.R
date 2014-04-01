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
    mydata$wd<-weekdays(mydata$posix)
    mydata
}
# daily steps
#   mydaily<-getDaily(mydata)
#
getDaily<-function(myd) {
    mydaily<-aggregate(steps ~ date, myd, sum)
    mydaily    
}
library(plyr)
getDaily2<-function(myd) {
    myd<-myd[!is.na(myd$steps),] 
    ddply(myd, .(date), summarize,
          mean=round(mean(steps),2),
          median=round(median(steps),2),
          sum=round(sum(steps),2)
          )
}
# plot daily
#   plotDaily(mydaily)
#
library(ggplot2)
plotDaily <- function(mydaily) {
    mymean<-mean(mydaily$steps)
    mymedian<-median(mydaily$steps)
    mylabels=c(paste('Mean:', mymean), paste('Median:', mymedian))
    cols = c('blue', 'orange')
    ggplot(mydaily, aes(x=steps)) +
        geom_histogram(fill='black') +      #,binwidth=1500) +
        geom_point(aes(x=mymean, y=0, color='blue'), size=4, shape=25) + 
        geom_point(aes(x=mymedian, y=0, color='orange'), size=4, shape=24) + 
        scale_color_manual(name=element_blank(), labels=mylabels, values=cols) + 
        labs(title='Histogram of daily steps taken',
             x='Number of steps', y='Count') + 
        theme(legend.position = 'bottom')
}
plotDaily2File<-function(mydaily,myfile='') {
    plotDaily(mydaily)
    ggsave(file=myfile)
    dev.off()
}

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

intervalDataWeekday<-function(myd) {
    mytemp<-subset( myd, ! myd$wd %in% c('Saturday','Sunday'))
    intervalData(mytemp)
}
intervalDataWeekend<-function(myd) {
    mytemp<-subset( myd, myd$wd %in% c('Saturday','Sunday'))
    intervalData(mytemp)
}
getIntervalMax<-function(myseg) {
    mymax<-myseg[ myseg$sum == max(myseg$sum), ]
    mymax$interval
}
#
# Plot time series data:
#
#  tsplot( myintv$sum )
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
tsplot2<-function(mydata,myfile='') {
    mysegwe = intervalDataWeekend(mydata)
    mysegwd = intervalDataWeekday(mydata)
    if (myfile!='') { png(filename=myfile) }
    par(mfrow=c(2,1))
    tsplot(mysegwe, 'weekend')
    tsplot(mysegwd, 'weekday')
    if (myfile!='') { dev.off() }    
}
#
#
#
runFile<-function(filename) {
    mydata<-file2Data(filename)
    summary(mydata$steps)
    mydaily<-getDaily(mydata)
    plotDaily(mydaily,'daily.png')
    myseg<-intervalData(mydata)
    tsplot(myseg,'seg.png')
}


#myfile<-getFile() # myfile is 'activity.csv'
#runFile('activity.csv')
