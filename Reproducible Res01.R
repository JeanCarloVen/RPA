setwd("C:/Users/Lenovo Y50/Downloads/PERSONAL/CURSOS/ESPECIALIZACION DATA SCIENCE JOHN HOPKINGS/Coursera/MODULO5_EXPLORATORY_DATA_ANALISYS/PROJECTS")

## It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, 
## or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves 
## regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both 
## because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

## Commit containing full submission

# 1) Code for reading in the dataset and/or processing the data
# 2) Histogram of the Total number of steps taken each day
# 3) Mean and median number of steps taken each day
# 4) Time series plot of the average number of steps taken
# 5) The 5-minute interval that, on average, contains the maximum number of steps
# 6) Code to describe and show a strategy for imputing missing data
# 7) Histogram of the total number of steps taken each day after missing values are imputed
# 8) Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
# 9) All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

# Libraries:
install.packages("hrbrthemes")
library(lubridate)
library(ggplot2)
library(dplyr)
library(hrbrthemes)

# Loading and preprocessing the data

## Loading Data

tmp <- tempfile()
archivo <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
download.file(archivo, tmp)

## Reading Dataset
Activity <- unz(tmp, 'activity.csv')

dFactivity <- read.csv(Activity, header=TRUE, na.strings = "NA")
str(dFactivity)

## Cleaning Data
# 1rst Method

dFact <- dFactivity[complete.cases(dFactivity),]

# 2nd Method
dFact<- na.omit(dFact)
str(dFact)

##  Fixing Data

dFact$date <- ymd(dFact$date)
head(dFact, 10)


# What is mean total number of steps taken per day?
# Investigaremos como es la distribución de pasos, para ello, primero se separa en grupos por fecha
sumStepsByDate<- dFact %>% 
    group_by(date) %>% 
    summarise(sumSteps = sum(steps))

# Realizamos un resumen de los datos:
sumStepsByDate
summary(sumStepsByDate)
# Observamos que el promedio de pasos díarios es de 10,765 pasos, con un máximo de 21194 y mínimo de 41.

sumStepsByDate %>% 
    ggplot(aes(sumSteps))+
    geom_histogram(color= "blue", fill = "yellow")+
    geom_vline(aes(xintercept = mean(sumSteps)), 
               color = "blue", linetype = "dashed", size =2)+
    ggtitle("Histogram of total number of steps per day")

# Observamos una mayor concentración entre el 25 y el 75 por ciento, es decir, por arriba de los 8,000 pasos por día.

# Time Series
# First for the total Steps

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

meanStepsByInterval <- dFact %>% 
    group_by(interval) %>% 
    summarize(meanSteps = mean(steps))
meanStepsByInterval

meanStepsByInterval %>% 
    ggplot(aes(x= interval, y= meanSteps))+
    geom_line()+
    geom_point()+
    labs(title = "Average Number of Steps Taken vs 5-min Interval", caption = "Data: Activity Monitoring Data")


# Second for the Average Steps

# Calculate the mean  for every day.

meanStepsByDate<- dFact %>% group_by(date) %>% 
    summarise(meanSteps = mean(steps))

meanStepsByDate %>% 
    ggplot(aes(x= date, y = meanSteps))+
    geom_line( color="steelblue") +
    geom_point()+
    labs(title = "Time Series for Mean Steps by day", caption = "Data: Activity Monitoring Data")+
    ylab("")+
    xlab("")+
    scale_x_date(date_labels = "%b %d")+
    theme_ipsum()+
    theme(axis.text.x=element_text(angle=60, hjust=1))

summary(meanStepsByDate)


# The 5-minute interval that, on average, contains the maximum number of steps

str(dFact)
sum(dFact$steps)
str(dFactivity)
head(dFact)
tail(dFact)
summary(dFact)

meanFiveMinutes <- dFact %>% group_by(interval) %>% summarize(meanStep=mean(steps))
meanFiveMinutes

plotStepInterval <- ggplot(meanFiveMinutes, aes(x=interval, y=meanStep)) + 
    geom_line(color="red") +
    labs(title="Average Number of Steps Taken vs 5-min Interval", y="Average Number of Steps", x="5-min Interval Times Series")

plotStepInterval

# This is that the inteval 835 has a main of 206.17 steps


# Code to describe and show a strategy for imputing missing data

# Se suman todos los valores que en la fila de pasos tengan algun NA y se reporta el número.

sum(is.na(dFactivity))

# The strategy for fill all the missing values in the dataset will be use the median for the day.

imputeData <- dFactivity
imputeData$steps[is.na(imputeData$steps)] <- mean(imputeData$steps, na.rm = TRUE)
imputeData$steps <- as.numeric(imputeData$steps)
imputeData$interval <- as.numeric(imputeData$interval)
colSums(is.na(imputeData))

# Create a new dataset that is equal to the original dataset but with the missing data filled in.

summary(imputeData)

str(imputeData)

imputeDataCurrent <- summarize(group_by(imputeData, date), dailyStep=sum(steps))

meanDataCurrent <- as.integer(mean(imputeDataCurrent$dailyStep))

meanDataCurrent

# Plot historigram
# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total 
# number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total daily number of steps?

plotStepDay <- ggplot(imputeDataCurrent, aes(x=dailyStep)) +
    geom_histogram(binwidth = 1000, aes(y=..count.., fill=..count..))+
    geom_vline(xintercept = meanDataCurrent, colour="red", linetype="dashed", size=1)+
    labs(title = "Histogram of Number of Steps taken each day (impute)", y="Frecuency", x="Daily Steps")

plotStepDay

str(imputeData)

imputeData$dateimputeData

imputeData$day <- ifelse(weekdays(imputeData$date) %in% c("Saturday", "Sunday"), "weekday", "weekend")


imputeDataFRAME <- imputeData %>% 
    group_by(interval, day) %>% 
    summarise(meanStep=mean(steps))
PlotWeekInterval <- ggplot(imputeDataFRAME, aes(x=interval, y=meanStep, color=day))+
    facet_grid(day~.)+
    geom_line()+
    labs(title="Average Number of Steps Taken vs 5-min Interval on Weekday/Weekend", y="Average Number of Steps", x="5-min Interval Times Series")
PlotWeekInterval


