# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all 
# sources for each of the years 1999, 2002, 2005, and 2008.

plot1 <- function() {
    print("Please wait, reading data...")
    NEI <- readRDS(file.path("exdata-data-NEI_data", "summarySCC_PM25.rds"))
    SCC <- readRDS(file.path("exdata-data-NEI_data", "Source_Classification_Code.rds"))
    
    # total emissions per each year
    total.emissions <- tapply(NEI$Emissions, NEI$year, sum)
    years <- as.integer(dimnames(total.emissions)[[1]])

    png(filename = "plot1.png", width = 480, height = 480, units = "px")
    plot(years, 
         total.emissions, 
         type = "l", 
         lwd = 4, 
         xlab = "Year", 
         ylab = "Total emissions (tons)",
         main = "Total emissions of PM2.5 in the United States")
    dev.off()
    print("Done!")
}