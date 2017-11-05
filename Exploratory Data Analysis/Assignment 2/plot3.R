# Plot 3

# Load packages
library(dplyr)
library(ggplot2)

# Load data
unzip("exdata%2Fdata%2FNEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
NEI$type <- as.factor(NEI$type)

# Filter and aggregate data
# Only keep Baltimore City
NEI <- NEI[NEI$fips == "24510", ]
total <- NEI %>% group_by(NEI$year, NEI$type) %>% summarise(total_tons_pm25 = sum(Emissions))
names(total)[c(1, 2)] <- c("year", "type")

# Create the plot
png(file = "plot3.png")
qplot(year, total_tons_pm25, data = total, facets = ~type,
      geom = "line", ylab = "PM2.5 Emissions (tons)", xlab = "Year",
      main = "PM2.5 Emissions in Baltimore City by Type") + 
  scale_x_continuous(breaks = unique(total$year))
dev.off()
