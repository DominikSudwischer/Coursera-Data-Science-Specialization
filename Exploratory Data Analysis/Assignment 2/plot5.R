# Plot 5

# Load packages
library(dplyr)

# Load data
unzip("exdata%2Fdata%2FNEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
NEI$type <- as.factor(NEI$type)
SCC$EI.Sector <- tolower(SCC$EI.Sector)

# Filter and aggregate data
# First, we filter for Baltimore City and then look for emission sources in SCC with "vehicle"
# in it and finally merge (inner join) the filtered NEI with the filtered SCC to only keep relevant entries
NEI <- NEI[NEI$fips == "24510", ]
SCC <- SCC[grepl("vehicle", SCC$EI.Sector), ]
NEI <- merge(NEI, SCC, by.x = "SCC", by.y = "SCC")
total <- NEI %>% group_by(NEI$year) %>% summarise(total_tons_pm25 = sum(Emissions))
names(total)[1] <- "year"

# Create the plot
png(file = "plot5.png")
plot(total$year, total$total_tons_pm25, type = "o",
     main = "Total PM2.5 Emissions from Vehicles in Baltimore City, Maryland", xlab = "Year",
     ylab = "PM2.5 Emissions (tons)", xaxt = "n")
axis(side = 1, at = total$year, labels = total$year)
dev.off()
