library(data.table)
library(ggplot2)

dataURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url = dataURL,destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'),
              method = "curl")
unzip("repdata%2Fdata%2Factivity.zip",exdir = "data")

activity <- data.table::fread(input = "data/activity.csv")

Total_Steps <- activity[, c(lapply(.SD, sum, na.rm = FALSE)),
                        .SDcols = c("steps"), 
                        by = .(date)] 

head(Total_Steps, 10)

ggplot(Total_Steps, aes(x = steps))+
    geom_histogram(fill = "magenta", binwidth = 800)+
    labs(title = "Daily Number of Steps", x = "steps", y = "frequency")

Total_Steps[, .(Mean_Steps = mean(steps, na.rm = TRUE),
                Median_Steps = median(steps, na.rm = TRUE))]


IntervalDT <- activity[, c(lapply(.SD, mean, na.rm = TRUE)),
                       .SDcols = c("steps"), 
                       by = .(interval)] 

ggplot(IntervalDT, aes(x = interval , y = steps)) + geom_line(color="magenta", size=1) +
    labs(title = "Avg. Daily Steps", x = "Interval", y = "Avg. Steps per day")


IntervalDT[steps == max(steps), .(max_interval = interval)]


activity[is.na(steps), .N ]

# Filling in missing values with median of dataset. 
activity[is.na(steps), "steps"] <- activity[, c(lapply(.SD, 
                                                       median, 
                                                       na.rm = TRUE)),
                                            .SDcols = c("steps")]

data.table::fwrite(x = activity, file = "data/tidyData.csv", quote = FALSE)


# total number of steps taken per day
Total_Steps <- activity[, c(lapply(.SD, sum)), .SDcols = c("steps"), by = .(date)] 

# mean and median total number of steps taken per day
Total_Steps[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]

ggplot(Total_Steps, aes(x = steps)) + geom_histogram(fill = "magenta", binwidth = 1000) + 
    labs(title = "Daily Steps", x = "Steps", y = "Frequency")


# Just recreating activity from scratch then making the new factor variable. (No need
# to, just want to be clear on what the entire process is.) 
activity <- data.table::fread(input = "data/activity.csv")
activity[, date := as.POSIXct(date, format = "%Y-%m-%d")]
activity[, `Day of Week`:= weekdays(x = date)]
activity[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", 
               x = `Day of Week`), "weekday or weekend"] <- "weekday"
activity[grepl(pattern = "Saturday|Sunday", 
               x = `Day of Week`),
         "weekday or weekend"] <- "weekend"
activity[, `weekday or weekend` := as.factor(`weekday or weekend`)]
head(activity, 10)


activity[is.na(steps), "steps"] <- activity[, c(lapply(.SD, 
                                                       median, 
                                                       na.rm = TRUE)), 
                                            .SDcols = c("steps")]
Interval <- activity[, c(lapply(.SD, mean, na.rm = TRUE)), 
                     .SDcols = c("steps"), 
                     by = .(interval, `weekday or weekend`)] 

ggplot(Interval , aes(x = interval , y = steps, color=`weekday or weekend`))+
    geom_line() + 
    labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps")+
    facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2)





