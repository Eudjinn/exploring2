# How have emissions from motor vehicle sources changed from 1999 to 2008 in Baltimore City?

plot5 <- function() {
    print("Please wait, reading data...")
    NEI <- readRDS(file.path("exdata-data-NEI_data", "summarySCC_PM25.rds"))
    SCC <- readRDS(file.path("exdata-data-NEI_data", "Source_Classification_Code.rds"))
    
    # select data from Baltimore City
    NEI.BC <- subset(NEI, fips == "24510")

    # select motor vehicles in Baltimore City.
    # looking for the words "motor" and "vehicles" probably does not gather all the sources but
    # it should be pretty close and sufficient to complete the task
    mv <- grep("vehicle", SCC$EI.Sector, ignore.case = TRUE)
    mv.SCC <- SCC$SCC[mv]
    NEI.BC.mv <- subset(NEI.BC, SCC %in% mv.SCC)    
    
    ## total emissions from coal combustion-related sources per year
    # total.emissions.BC.mv <- tapply(NEI.BC.mv$Emissions, NEI.BC.mv$year, sum)
    # total.emissions.BC.mv <- melt(total.emissions.BC.mv, varnames = "year", value.name = "Emissions")
    
    
    # instead of showing total Emissions, this time lets look at boxplot of emissions all the selected
    # sources on a log10 scale, divided into types of sources
    # in order to avoid warnings from log10, get rid of values <= 0 first
    NEI.BC.mv <- subset(NEI.BC.mv, Emissions > 0)    
    
    png(filename = "plot5.png", width = 480, height = 480, units = "px")
    print(qplot(as.factor(year), 
          log10(Emissions), 
          data = NEI.BC.mv, 
          geom = "boxplot", 
          main = "Emissions from motor vehicles in Baltimore City",
          xlab = "year",
          ylab = "Emissions (log10(tons))"))
    dev.off()
    print("Done!")
}