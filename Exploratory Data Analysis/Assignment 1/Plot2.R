# Construction of Plot 2

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

# Plot 2
png(filename = "Plot2.png", width = 480, height = 480)
plot(df$DT, df$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
dev.off()