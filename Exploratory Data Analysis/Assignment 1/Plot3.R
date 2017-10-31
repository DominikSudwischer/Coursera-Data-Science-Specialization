# Construction of Plot 3

# Load libraries
library(lubridate)
library(hashmap)

# Load data
Sys.setlocale("LC_TIME", "English")
unzip("exdata%2Fdata%2Fhousehold_power_consumption.zip")
df <- read.table("household_power_consumption.txt", sep = ";", header = TRUE,
                 colClasses = c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric",
                                "numeric", "numeric"), na.strings = c("?"))

# Process data
df <- df[as.character(df$Date) %in% c("1/2/2007", "2/2/2007"), ]
df$DT <- as.POSIXct(paste(df$Date, df$Time), format = "%d/%m/%Y %H:%M:%S")
df$Time <- as.POSIXct(df$Time, format = "%H:%M:%S")
df$Date <- as.Date.factor(df$Date, "%d/%m/%Y")
wd <- hashmap(c(0, 1, 2, 3, 4, 5, 6), c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
df$weekday <- wd[[wday(df$DT)]]

# Plot 3
png(filename = "Plot3.png", width = 480, height = 480)
plot(df$DT, df$Sub_metering_1, type = "l", col = "gray2", lwd = 2, xlab = "", ylab = "Energy sub metering")
lines(df$DT, df$Sub_metering_2, col = "red", lwd = 2)
lines(df$DT, df$Sub_metering_3, col = "blue", lwd = 2)
legend("topright", legend = c("sub_metering_1", "sub_metering_2", "sub_metering_3"),
       col = c("gray", "red", "blue"), lwd = 2)
dev.off()