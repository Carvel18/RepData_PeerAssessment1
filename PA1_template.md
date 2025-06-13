---
title: "Activity Monitoring Data Analysis"
author: "Carvel18"
date: "2025-06-13"
output:
  html_document: default
  word_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(dplyr)
```

# Submission

### Loading the dataset
```{r}
activity <- read.csv("activity.csv")
```

### Converting date to Date type
```{r}
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
```

# What is Mean Total Number of Steps Taken Per Day?

### Total steps per day (ignoring NA)
```{r}
daily_steps <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
```

### Histogram of total steps per day
```{r}
hist(daily_steps$steps, col = "lightblue", main = "Total Steps per Day", 
     xlab = "Steps", breaks = 20)
```
     
### Mean and median
```{r}
mean_steps <- mean(daily_steps$steps)
median_steps <- median(daily_steps$steps)
mean_steps
median_steps
```

# What is the Average Daily Activity Pattern?

### Mean steps per interval
```{r}
interval_avg <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
```

### Plot time series
```{r}
plot(interval_avg$interval, interval_avg$steps, type = "l", col = "darkgreen",
     main = "Average Daily Activity Pattern",
     xlab = "5-minute Interval", ylab = "Average Steps")
```
     
## Interval with max average steps
```{r}
interval_avg[which.max(interval_avg$steps), ]
```

# Imputing Missing Values

### Total missing values
```{r}
sum(is.na(activity$steps))
```

### Filling NAs with the mean for that 5-minute interval
```{r}
interval_mean <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
activity_imputed <- activity
na_indices <- is.na(activity_imputed$steps)
activity_imputed$steps[na_indices] <- interval_mean[as.character(activity_imputed$interval[na_indices])]
```

### Histogram with imputed data
```{r}
daily_steps_imputed <- aggregate(steps ~ date, data = activity_imputed, sum)
hist(daily_steps_imputed$steps, col = "orange", main = "Steps per Day (Imputed)", 
     xlab = "Steps", breaks = 20)
```

### Mean and median after imputation
```{r}
mean(daily_steps_imputed$steps)
median(daily_steps_imputed$steps)
```

# Are There Differences in Activity Patterns Between Weekdays and Weekends?

### Add a column for weekday/weekend
```{r}
activity_imputed$day_type <- ifelse(weekdays(activity_imputed$date) %in% 
                                      c("Saturday", "Sunday"), "weekend", "weekday")
activity_imputed$day_type <- factor(activity_imputed$day_type)
```

```{r}
library(lattice)
```
# Aggregate steps by interval and day type
```{r}
steps_by_daytype <- aggregate(steps ~ interval + day_type, data = activity_imputed, mean)
```

# Panel plot
```{r}
xyplot(steps ~ interval | day_type, data = steps_by_daytype, layout = c(1, 2),
       type = "l", col = "blue",
       xlab = "5-minute Interval", ylab = "Average Steps",
       main = "Weekday vs Weekend Activity Patterns")
```
