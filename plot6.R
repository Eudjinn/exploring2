# Compare emissions from motor vehicle sources in Baltimore City with emissions from 
# motor vehicle sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?

plot6 <- function() {
    print("Please wait, reading data...")
    NEI <- readRDS(file.path("exdata-data-NEI_data", "summarySCC_PM25.rds"))
    SCC <- readRDS(file.path("exdata-data-NEI_data", "Source_Classification_Code.rds"))
    
    # select data from Baltimore City
    NEI.BC <- subset(NEI, fips == "24510")
    # select data from Los Angeles County, California
    NEI.LA <- subset(NEI, fips == "06037")

    # looking for the word "vehicle" probably does not gather all the sources but
    # it should be pretty close and sufficient to complete the task
    mv <- grep("vehicle", SCC$EI.Sector, ignore.case = TRUE)
    mv.SCC <- SCC$SCC[mv]

    # select motor vehicles in Baltimore City.
    NEI.BC.mv <- subset(NEI.BC, SCC %in% mv.SCC)    

    # select motor vehicles in Los Angeles County.
    NEI.LA.mv <- subset(NEI.LA, SCC %in% mv.SCC)    
    
        
    # total emissions from motor vehicle sources per year in BC
    total.emissions.BC.mv <- tapply(NEI.BC.mv$Emissions, NEI.BC.mv$year, sum)
    total.emissions.BC.mv <- melt(total.emissions.BC.mv, varnames = "year", value.name = "Emissions")
    total.emissions.BC.mv$county <- "BC"
    
    # total emissions from motor vehicle sources per year in LA
    total.emissions.LA.mv <- tapply(NEI.LA.mv$Emissions, NEI.LA.mv$year, sum)
    total.emissions.LA.mv <- melt(total.emissions.LA.mv, varnames = "year", value.name = "Emissions")
    total.emissions.LA.mv$county <- "LA"
    
    # add two datasets in one data frame for plotting
    total.emissions.mv <- rbind(total.emissions.BC.mv, total.emissions.LA.mv)
    
    # make a bar plot this time
    png(filename = "plot6.png", width = 480, height = 480, units = "px")
    print(qplot(as.factor(year), 
                Emissions, 
                data = total.emissions.mv, 
                geom = "bar", 
                stat = "identity",
                fill = county,
                main = "Emissions from motor vehicles in BC and LA",
                xlab = "year",
                ylab = "Emissions (tons)"))
    dev.off()
    print("Done!")
}