# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999 to 2008 for Baltimore City? 
# Which have seen increases in emissions from 1999 to 2008? Use the ggplot2 plotting system to make a 
# plot answer this question.

plot3 <- function() {
    print("Please wait, reading data...")
    NEI <- readRDS(file.path("exdata-data-NEI_data", "summarySCC_PM25.rds"))
    SCC <- readRDS(file.path("exdata-data-NEI_data", "Source_Classification_Code.rds"))
    
    # subset only Baltimore City fips == "24510"
    NEI.BC <- subset(NEI, fips == "24510")
    
    # total emissions in Baltimore City fips == "24510" per each year by type
    total.emissions.BC.bytype <- tapply(NEI.BC$Emissions, list(NEI.BC$year, NEI.BC$type), sum)
    # transform the array to a more suitable for qplot format
    total.emissions.BC.bytype <- melt(total.emissions.BC.bytype, 
                                      varnames = c("year", "type"), 
                                      value.name = "Emissions")
    png(filename = "plot3.png", width = 480, height = 480, units = "px")
    print(qplot(year, 
          Emissions, 
          data = total.emissions.BC.bytype, 
          color = type, 
          geom = "line",
          main = "Emissions in Baltomore City per type"))
    dev.off()    
    print("Done!")
}