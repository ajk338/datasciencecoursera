# Reproducible Research: Peer Assessment 1
#Aaron Kleyn



## Loading and preprocessing the data
```{r loaddata}
unzip(zipfile = "activity.zip")
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
```{r}
library(ggplot2)
aggregate.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(aggregate.steps, binwidth=1000, xlab="Aggregate steps taken each day")
```

![](https://cloud.githubusercontent.com/assets/11023062/6653359/cf6362e4-ca5c-11e4-85ea-dd17fe053cef.jpg)

```{r}
mean(aggregate.steps, na.rm=TRUE)
```
## [1] 9354


```{r}
median(aggregate.steps, na.rm=TRUE)
```
## [1] 10395


## What is the average daily activity pattern?
```{r}
library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")
```
![](https://cloud.githubusercontent.com/assets/11023062/6653360/d3667606-ca5c-11e4-9874-e6867237dc98.jpg)

On average across all the days in the dataset, the 5-minute interval contains
the maximum number of steps?
```{r}
averages[which.max(averages$steps),]
```

## Imputing missing values

There are many days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the data. Additionally, this will skew the data set and affect the median value.

```{r how_many_missing}
missing <- is.na(data$steps)
# How many missing
table(missing)
```
## missing
## FALSE  TRUE 
## 15264  2304


To improve the dataset, we will impute the mean value for a 5-minute interval for each missing value.

```{r}
# Replace each missing value with the mean value of its 5-minute interval
imputed.value <- function(steps, interval) {
    imputed<- NA
    if (!is.na(steps))
        imputed <- c(steps)
    else
        imputed <- (averages[averages$interval==interval, "steps"])
    return(imputed)
}
imputed.data <- data
imputed.data$steps <- mapply(imputed.value, imputed.data$steps, imputed.data$interval)
```
A histogram was created for the steps was created, with the mean and median number of steps per day calculated. The histogram appears to follow a t-distribution, since the values appear somewhat normally distributed with a high peak in the center of the distribution.
```{r}
total.steps <- tapply(imputed.data$steps, imputed.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")


mean(total.steps)
```


## [1] 10766

```{r}
median(total.steps)
```

## [1] 10766

After imputing the values, the median number of steps is equal to the mean number of steps. We can conclude that by imputing the mean value for each NA we have effectively eliminated the bias in our dataset. 

## Are there differences in activity patterns between weekdays and weekends?
We need to order our values in the dataset by the days of the week. 
This will be done using the dataset that includes the imputed values.

```{r}
weekdayWeekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
imputed.data$date <- as.Date(imputed.data$date)
imputed.data$day <- sapply(imputed.data$date, FUN=weekdayWeekend)
```

A plot will be created for the number of steps taken for weekdays and the weekend days. Our plot will show the steps taken in each 5 minute interval.

```{r}
mu <- aggregate(steps ~ interval + day, data=imputed.data, mean)
ggplot(mu, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
```

![](https://cloud.githubusercontent.com/assets/11023062/6653362/d9aac558-ca5c-11e4-81c9-881212fb4849.jpg)

The weekdays have the largest number of steps taken in any one 5 minute interval. However, the weekend has a larger number of 5 minute intervals with over 100 steps.
