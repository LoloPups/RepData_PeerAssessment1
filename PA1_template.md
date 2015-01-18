# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data  

```r
DT1<-read.csv("activity.csv")
```

Creating directory for plots  

```r
dir.create("figure")
```

Removing rows with NA steps value.  

```r
DT2<-DT1[!is.na(DT1$steps),]
```

## Histogram of the total number of steps taken each day

```r
stepsSums<-aggregate(DT2$steps,by=list(DT2$date),FUN=sum)
names(stepsSums)<-c("date","steps")
hist(stepsSums$steps, col="blue", xlab = "Steps", main="Total number of steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
dev.copy(png, file = "figure//Plot1.png",width = 680, height = 480)
dev.off()
```

## Calculating mean total number of steps taken per day?

```r
library(plyr)
mm<-ddply(stepsSums,.(),summarize, mean=mean(steps), median=median(steps))
library(xtable)
mmtable<-xtable(mm[c("mean","median")])
print(mmtable, type = "html")
```

<!-- html table generated in R 3.1.1 by xtable 1.7-4 package -->
<!-- Sun Jan 18 22:53:51 2015 -->
<table border=1>
<tr> <th>  </th> <th> mean </th> <th> median </th>  </tr>
  <tr> <td align="right"> 1 </td> <td align="right"> 10766.19 </td> <td align="right"> 10765 </td> </tr>
   </table>

## The average daily activity pattern

```r
intervalsMeans<-with(DT2,aggregate(steps,by=list(interval),FUN=mean))
names(intervalsMeans)<-c("interval","steps")
with(intervalsMeans,plot(x=interval,y=steps,type="l",xlab="Interval",
                         ,ylab="Steps", main="Daily activity"))
```

![](./PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

```r
dev.copy(png, file = "figure//Plot2.png",width = 680, height = 480)
dev.off()
```

## Imputing missing values
1. Printing number of rows with NA value in 'steps' column

```r
sum(is.na(DT1$steps))
```

```
## [1] 2304
```

2. Creating list for replacing NA by means for inervals

```r
meansList<-intervalsMeans$steps
names(meansList)<-intervalsMeans$interval
```

3. Creating new table with filled NA

```r
DT3<-DT1
naIndices<-is.na(DT3)
DT3[naIndices,"steps"]<-meansList[as.character(DT3[naIndices,]$interval)]
```

4. Creating a hist and stats for new table...  

```r
stepsSums3<-aggregate(DT3$steps,by=list(DT3$date),FUN=sum)
names(stepsSums3)<-c("date","steps")
hist(stepsSums3$steps, col="blue", xlab = "Steps", main="Total number of steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

```r
dev.copy(png, file = "figure//Plot3.png",width = 680, height = 480)
dev.off()
```
As is easy to see the count of days which total number of steps varies between 10000 and 15000 has increased.  
  
5. Creating the table of new mean and new median.

```r
mm<-ddply(stepsSums3,.(),summarize, mean=mean(steps), median=median(steps))
mmtable<-xtable(mm[c("mean","median")])
print(mmtable, type = "html")
```

<!-- html table generated in R 3.1.1 by xtable 1.7-4 package -->
<!-- Sun Jan 18 22:53:51 2015 -->
<table border=1>
<tr> <th>  </th> <th> mean </th> <th> median </th>  </tr>
  <tr> <td align="right"> 1 </td> <td align="right"> 10766.19 </td> <td align="right"> 10766.19 </td> </tr>
   </table>


## Creating a new factor variable indicating whether a given date is a weekday or weekend day.

```r
library(chron)
library(ggplot2)
DT3$date<-as.Date(DT3$date)
DT3$daytype[is.weekend(DT3$date)]<-"weekend"
DT3$daytype[!is.weekend(DT3$date)]<-"weekday"
DT3$daytype<-as.factor(DT3$daytype)
qplot(interval, steps, data=DT3, main="Daily activity", facets=daytype~., 
      stat="summary",fun.y="mean", geom="line")
```

![](./PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

```r
dev.copy(png, file = "figure//Plot4.png", width = 680, height = 480)
dev.off()
```
