# Plot 6

# Load packages
library(dplyr)

# Load data
unzip("exdata%2Fdata%2FNEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
NEI$type <- as.factor(NEI$type)
SCC$EI.Sector <- tolower(SCC$EI.Sector)

# Filter and aggregate data
# First, we filter for Baltimore City and Los Angeles and then look for emission sources in SCC with "vehicle"
# in it and finally merge (inner join) the filtered NEI with the filtered SCC to only keep relevant entries
NEI <- NEI[NEI$fips %in% c("24510", "06037"), ]
NEI$fips <- as.factor(NEI$fips)
SCC <- SCC[grepl("vehicle", SCC$EI.Sector), ]
NEI <- merge(NEI, SCC, by.x = "SCC", by.y = "SCC")
total <- NEI %>% group_by(NEI$year, NEI$fips) %>% summarise(total_tons_pm25 = sum(Emissions))
names(total)[c(1, 2)] <- c("year", "city")

# Subset data by city
los_angeles <- subset(total, city == "06037")
baltimore <- subset(total, city == "24510")

# Create the plot
#png(file = "plot6.png")
plot(los_angeles$year, los_angeles$total_tons_pm25, type = "o",
     main = "Total PM2.5 Emissions from Vehicles in Baltimore City vs. Los Angeles", xlab = "Year",
     ylab = "PM2.5 Emissions (tons)", xaxt = "n", col = "red",
     ylim = c(0, max(los_angeles$total_tons_pm25)))
lines(baltimore$year, baltimore$total_tons_pm25, col = "green")
axis(side = 1, at = total$year, labels = total$year)
#dev.off()

fips == "06037"
