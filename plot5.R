# How have emissions from motor vehicle sources changed from 1999 to 2008 in Baltimore City?

plot5 <- function() {
    print("Please wait, reading data...")
    NEI <- readRDS(file.path("exdata-data-NEI_data", "summarySCC_PM25.rds"))
    SCC <- readRDS(file.path("exdata-data-NEI_data", "Source_Classification_Code.rds"))
    
    # select data from Baltimore City
    NEI.BC <- subset(NEI, fips == "24510")

    # looking for the word "vehicle" probably does not gather all the sources but
    # it should be pretty close and sufficient to complete the task
    mv <- grep("vehicle", SCC$EI.Sector, ignore.case = TRUE)
    mv.SCC <- SCC$SCC[mv]

    # select motor vehicles in Baltimore City.
    NEI.BC.mv <- subset(NEI.BC, SCC %in% mv.SCC)    
    
    # total emissions from motor vehicle sources per year
    total.emissions.BC.mv <- tapply(NEI.BC.mv$Emissions, NEI.BC.mv$year, sum)
    total.emissions.BC.mv <- melt(total.emissions.BC.mv, varnames = "year", value.name = "Emissions")
    
    png(filename = "plot5.png", width = 480, height = 480, units = "px")
    print(qplot(year, 
          Emissions, 
          data = total.emissions.BC.mv, 
          geom = c("point","line"), 
          main = "Emissions of PM2.5 from motor vehicles in Baltimore City",
          xlab = "Year",
          ylab = "Emissions (tons)"))
    dev.off()
    print("Done!")
}