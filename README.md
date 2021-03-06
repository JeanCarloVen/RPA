# Introduction


It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about  dsd themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]

The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as \color{red}{\verb|NA|}NA)
* date: The date on which the measurement was taken in YYYY-MM-DD format
* interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Libraries:
```{r, echo=TRUE, warning=FALSE, message=FALSE}
library(lubridate)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(data.table)
```

# I. Loading and preprocessing the data
```{r}
tmp <- tempfile()
archivo <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
download.file(archivo, tmp)
```

## Reading Dataset

```{r}
Activity <- unz(tmp, 'activity.csv')
dFactivity <- read.csv(Activity)
str(dFactivity)
```

## Cleaning Data
### 1rst Method
```{r}
dFact <- dFactivity[complete.cases(dFactivity),]
```

### 2nd Method
```{r}
dFact<- na.omit(dFact)
str(dFact)
```

##  Fixing Data
```{r}
dFact$date <- ymd(dFact$date)
```

What is mean total number of steps taken per day?
Investigaremos como es la distribución de pasos, para ello, primero se separa en grupos por fecha

```{r}
sumStepsByDate<- dFact %>% group_by(date) %>% 
                            summarize(sumSteps = sum(steps))
summary(sumStepsByDate)
```
![](https://github.com/JeanCarloVen/RPA/blob/master/images/01.png)

We observe that the average of daily steps is 10,765 steps, with a maximum of 21,194 and a minimum of 41.

```{r, message=FALSE}
sumStepsByDate %>% 
    ggplot(aes(sumSteps))+
    geom_histogram(color= "blue", fill = "yellow")+
    geom_vline(aes(xintercept = mean(sumSteps)), 
               color = "blue", linetype = "dashed", size =2)+
    ggtitle("Histogram of total number of steps per day")
```

We observe a higher concentration between 25 and 75 percent, that is, above 8,000 steps per day.

![](https://github.com/JeanCarloVen/RPA/blob/master/images/02.png)

## II. Calculate and report the mean and median total number of steps taken per day?

### Summary

```{r, message=FALSE}
summary(sumStepsByDate)
```
* The Mean total number of steps taken per day is 10,766

* The Median total number of steps taken per day is 10,765

## III. What is the average daily activity pattern?

```{r, warning=FALSE, message=FALSE}
sumStepsByDate %>% 
    ggplot(aes(x= date, y = sumSteps))+
        geom_line( color="steelblue") +
        geom_point()+
        labs(title = "Time Series for Total Steps by day", caption = "Data: Activity Monitoring Data")+
        ylab("")+
        xlab("")+
        scale_x_date(date_labels = "%b %d")+
        theme_ipsum()+
        theme(axis.text.x=element_text(angle=60, hjust=1))
```

### a) Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Prepare data for ggplot

```{r}
meanFiveMinutes <- dFact %>% group_by(interval) %>% summarize(meanSteps=mean(steps))
meanFiveMinutes
plotStepInterval <- ggplot(meanFiveMinutes, aes(x=interval, y=meanSteps)) + 
    geom_line(color="red") +
    labs(title="Average Number of Steps Taken vs 5-min Interval", y="Average Number of Steps", x="5-min Interval Times Series")
                        
plotStepInterval
```
![](https://github.com/JeanCarloVen/RPA/blob/master/images/03.png)

### b. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
intervalMaxNumber <- which.max(meanFiveMinutes$meanSteps)
selectIntervalMaxNumber <- meanFiveMinutes$interval[intervalMaxNumber]
```


The maximum number of steps comes from: 
```{r}
selectIntervalMaxNumber
```



## IV. Code to describe and show a strategy for imputing missing data

### a) The strategy start with the total number of missing values in the dataset
```{r}
sum(is.na(dFactivity))
```

### b) The strategy for filling all the missing values in the dataset will be use the median for the day.
```{r}
imputeData <- dFactivity
imputeData$steps[is.na(imputeData$steps)] <- median(imputeData$steps, na.rm = TRUE)
imputeData$steps <- as.numeric(imputeData$steps)
imputeData$interval <- as.numeric(imputeData$interval)
colSums(is.na(imputeData))
```

### c) Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
summary(imputeData)
str(imputeData)
```



### d) Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```{r}
imputeDataCurrent <- summarize(group_by(imputeData, date),dailyStep=sum(steps))
meanDataCurrent <- as.integer(mean(imputeDataCurrent$dailyStep))
meanDataCurrent
plotStepDay <- ggplot(imputeDataCurrent, aes(x=dailyStep)) +
    geom_histogram(binwidth = 1000, aes(y=..count.., fill=..count..))+
    geom_vline(xintercept = meanDataCurrent, colour="red", linetype="dashed", size=1)+
    labs(title = "Histogram of Number of Steps taken each day (impute)", y="Frecuency", x="Daily Steps")
plotStepDay
```
![](https://github.com/JeanCarloVen/RPA/blob/master/images/04.png)
The mean and median total number of steps taken per day (after impute) is 10766.
