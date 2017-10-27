# First assignment in the Reproducible Research Course
library(ggplot2)
library(hashmap)
library(dplyr)

# A function to calculate average steps per interval
sum_steps <- function(input_df)
{
  output_df <- input_df[, c(1, 2)] %>% group_by(date) %>% summarise_all(funs(sum))
  colnames(output_df) = c("date", "number_of_steps")
  output_df
}

# A function to check whether a day is a weekday or in the weekend
day_classification <- function(date)
{
  day_number <- as.POSIXlt(date)$wday
  if(day_number <= 0 || day_number >= 6)
  {
    "weekend"
  }
  else
  {
    "weekday"
  }
}

# Read input data
df <- read.csv("activity.csv")

# Calculating the total number of steps per day (ignoring missing values)
total_steps_per_day <- sum_steps(df)
colnames(total_steps_per_day) = c("date", "number_of_steps")

# Plotting a histogram
g <- ggplot(total_steps_per_day, aes(total_steps_per_day$number_of_steps))
g + geom_histogram(binwidth = 2000) + labs(x = "Number of Steps", y = "Count")

# Calculation of mean and median steps per day
mean_steps_per_day <- mean(total_steps_per_day$number_of_steps, na.rm = TRUE)
median_steps_per_day <- median(total_steps_per_day$number_of_steps, na.rm = TRUE)
paste("Mean steps per day: ", mean_steps_per_day, ". Median steps per day: ", median_steps_per_day, ".")

# Preparing time series data, i.e. grouping by interval and computing the mean over all days.
average_per_interval <- df[, c(1, 3)] %>% group_by(interval) %>% summarise_all(funs(mean), na.rm = TRUE)
colnames(average_per_interval) = c("Interval", "Mean_Steps")

# Plotting the time series plot for the average number of steps in each interval
g <- ggplot(average_per_interval, aes(average_per_interval$Interval, average_per_interval$Mean_Steps))
g + geom_line() + labs(x = "Interval", y = "Average Number of Steps")

# Computing the interval the maximum number of steps on average
max_interval <- average_per_interval$Interval[which.max(average_per_interval$Mean_Steps)]

# Filling in missing data using the mean for the specific interval
#median_steps <- total_steps_per_day %>% group_by(interval) %>% summarise(median_steps_per_weekday = mean(number_of_steps, na.rm = TRUE))
#colnames(median_steps) = c("Weekday", "Median_Steps")

# Create a hashmap to store median values per weekday for easy access
mean_steps <- hashmap(average_per_interval$Interval, round(average_per_interval$Mean_Steps))
# Fill in missing values in a new data frame
df_no_missing <- cbind(df)
for (i in 1:dim(df_no_missing)[1])
{
  if(is.na(df_no_missing[i, "steps"]))
  {
    df_no_missing[i, "steps"] = mean_steps[[df_no_missing[i, "interval"]]]
  }
}

# Calculate total steps per day for the modified data frame
total_steps_per_day_no_missing <- sum_steps(df_no_missing)

mean_steps_per_day_no_missing <- mean(total_steps_per_day_no_missing$number_of_steps, na.rm = TRUE)
median_steps_per_day_no_missing <- median(total_steps_per_day_no_missing$number_of_steps, na.rm = TRUE)
paste("Mean steps per day: ", mean_steps_per_day_no_missing, ". Median steps per day: ", median_steps_per_day_no_missing, ".")

# Plot the new data
g <- ggplot(total_steps_per_day_no_missing, aes(total_steps_per_day_no_missing$number_of_steps))
g + geom_histogram(binwidth = 2000) + labs(x = "Number of Steps", y = "Count")

# Check for difference in mean and median across weekend or weekdays using a grouped plot
df_no_missing$day_class <- as.factor(sapply(X = df_no_missing$date, FUN = day_classification))
mean_per_interval_per_dayclass <- df_no_missing[, c(1, 3, 4)] %>% group_by(interval, day_class) %>% summarise_all(funs(mean), na.rm = TRUE)
qplot(interval, steps, data = mean_per_interval_per_dayclass, facets = ~day_class, geom = "line")
