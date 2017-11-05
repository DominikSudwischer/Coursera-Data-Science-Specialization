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

# Normalize data with respect to the base year 1999 and calculate percentage change
base_data <- total %>%
  group_by(city) %>%
  summarise(base = first(total_tons_pm25, order_by = year)) %>%
  select(city, base)
total <- merge(total, base_data, by = "city")
total <- total %>%
  mutate(pct_change = 100 * (total_tons_pm25 / base -1 )) %>%
  filter(year > 1999) %>%
  mutate(city_name = ifelse(city == "24510", "Baltimore", "Los Angeles"))

# Note: Since I am only allowed to include exactly one plot, I chose percentage change as measurement.
# Under normal circumstances, I would have added a second plot to the figure to also show absolute changes.

# Create the plot
png(file = "plot6.png")
ggplot(data = total, aes(x = factor(year), y = pct_change, fill = factor(city_name))) +
  geom_col(position = "dodge") +
  ggtitle("Change of PM 2.5 Emission Compared to the Base Year 1999") +
  labs(x = "Year", y = "Emission Change (%)", fill = "City")
dev.off()
