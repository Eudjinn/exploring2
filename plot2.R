# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.

plot2 <- function() {
    print("Please wait, reading data...")
    NEI <- readRDS(file.path("exdata-data-NEI_data", "summarySCC_PM25.rds"))
    SCC <- readRDS(file.path("exdata-data-NEI_data", "Source_Classification_Code.rds"))
    
    # total emissions in Baltimore City fips == "24510" per each year
    
    NEI.BC <- subset(NEI, fips == "24510")
    
    total.emissions.BC <- tapply(NEI.BC$Emissions, NEI.BC$year, sum)
    years <- as.integer(dimnames(total.emissions.BC)[[1]])

    png(filename = "plot2.png", width = 480, height = 480, units = "px")
    plot(years, 
         total.emissions.BC, 
         type = "l", 
         lwd = 4, 
         xlab = "Year", 
         ylab = "Total emissions (tons)",
         main = "Total emissions from PM2.5 in Baltimore City")
    dev.off()
    print("Done!")
}