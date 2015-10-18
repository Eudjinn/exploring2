# Across the United States, how have emissions from coal combustion-related sources changed from 1999 to 2008?

plot4 <- function() {
    print("Please wait, reading data...")
    NEI <- readRDS(file.path("exdata-data-NEI_data", "summarySCC_PM25.rds"))
    SCC <- readRDS(file.path("exdata-data-NEI_data", "Source_Classification_Code.rds"))
    
    combustion <- grep("[Cc]ombustion", SCC$SCC.Level.One)
    coal <- grep("[Cc]oal", SCC$SCC.Level.Three)
    combustion.coal <- intersect(combustion, coal)
    
    SCC.cc.SCC <- SCC[combustion.coal, "SCC"]
    NEI.cc <- subset(NEI, SCC %in% SCC.cc.SCC)
    
    # total emissions from coal combustion-related sources per year
    total.emissions.cc <- tapply(NEI.cc$Emissions, NEI.cc$year, sum)
    total.emissions.cc <- melt(total.emissions.cc, varnames = "year", value.name = "Emissions")
    
    ## total emissions from coal combustion-related sources per year per type
    ## total.emissions.cc.bytype <- tapply(NEI.cc$Emissions, list(NEI.cc$year, NEI.cc$type), sum)
    ## total.emissions.cc.bytype <- melt(total.emissions.cc.bytype, varnames = c("year", "type"), value.name = "Emissions")
    
    png(filename = "plot4.png", width = 480, height = 480, units = "px")
    print(qplot(year, 
          Emissions, 
          data = total.emissions.cc, 
          geom = "line",
          main = "Emissions from coal combustion-related sources"))
    dev.off()
    print("Done!")
}