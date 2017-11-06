# Plot 2

# Load packages
library(dplyr)

# Load data
unzip("exdata%2Fdata%2FNEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
NEI$type <- as.factor(NEI$type)

# Filter and aggregate data
NEI <- NEI[NEI$fips == "24510", ]
total <- NEI %>% group_by(NEI$year) %>% summarise(total_tons_pm25 = sum(Emissions))
names(total)[1] <- "year"

# Create the plot
png(file = "plot2.png")
barplot(height = total$total_tons_pm25,
     main = "Total PM2.5 Emissions in Baltimore City, Maryland", xlab = "Year",
     names.arg = total$year,
     ylab = "PM2.5 Emissions (tons)")
dev.off()
