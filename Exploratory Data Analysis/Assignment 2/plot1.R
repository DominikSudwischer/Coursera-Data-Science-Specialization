# Plot 1

# Load packages
library(dplyr)

# Load data
unzip("exdata%2Fdata%2FNEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
NEI$type <- as.factor(NEI$type)

# Aggregate data
total <- NEI %>% group_by(NEI$year) %>% summarise(total_tons_pm25 = sum(Emissions))
names(total)[1] <- "year"

# Create the plot
png(file = "plot1.png")
plot(total$year, total$total_tons_pm25, type = "o",
     main = "Total PM2.5 Emissions in the U.S.", xlab = "Year",
     ylab = "PM2.5 Emissions (tons)", xaxt = "n")
axis(side = 1, at = total$year, labels = total$year)
dev.off()