# Construction of Plot 4

# Load libraries
library(lubridate)

# Load data
Sys.setlocale("LC_TIME", "English")
unzip("exdata%2Fdata%2Fhousehold_power_consumption.zip")
df <- read.table("household_power_consumption.txt", sep = ";", header = TRUE,
                 colClasses = c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric",
                                "numeric", "numeric"), na.strings = c("?"))

# Process data
df <- df[as.character(df$Date) %in% c("1/2/2007", "2/2/2007"), ]
df$DT <- as.POSIXct(paste(df$Date, df$Time), format = "%d/%m/%Y %H:%M:%S")

#Plot 4
png(filename = "Plot4.png", width = 480, height = 480)
par(mfrow = c(2,2))
plot(df$DT, df$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power")
plot(df$DT, df$Voltage, type = "l", xlab = "datetime", ylab = "Voltage")
plot(df$DT, df$Sub_metering_1, type = "l", col = "gray2", lwd = 2, xlab = "", ylab = "Energy sub metering")
lines(df$DT, df$Sub_metering_2, col = "red", lwd = 2)
lines(df$DT, df$Sub_metering_3, col = "blue", lwd = 2)
legend("topright", legend = c("sub_metering_1", "sub_metering_2", "sub_metering_3"),
       col = c("gray2", "red", "blue"), lwd = 2)
plot(df$DT, df$Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power", lwd = 1.5)
dev.off()