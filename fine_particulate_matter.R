


# Reading in the data frame and the classification code

setwd("/Users/skyrit17/Desktop/Personal/DataScientistCertification/4_Exploratory_Data_Analysis/fine_particulate_matter")
# This code assumes that the NEI and SCC files are already in your working directory. Set directory accordingly.

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(plyr)
library(ggplot2)
library(gridExtra)

#1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, 
#make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

EmissionTotal = ddply(NEI, ~year, summarise, sum=sum(Emissions, na.rm = TRUE))
png(filename = "plot1.png",width = 480, height = 480)
barplot(EmissionTotal$sum, names.arg = EmissionTotal$year,xlab = "Year", main = "Total Emissions from 1999 - 2008")
dev.off()

#2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use 
#the base plotting system to make a plot answering this question.

MarylandPM2.5 = aggregate(Emissions~year,data=subset(NEI,fips == "24510"),sum)
png(filename = "plot2.png",width = 480, height = 480)
barplot(MarylandPM2.5$Emissions, names.arg = MarylandPM2.5$year,xlab = "Year", main = "Total Emissions in Maryland from 1999 - 2008")
dev.off()

#3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four 
#sources have seen decreases in emissions from 1999-2008 for Baltimore City? Which have seen increases in emissions from 
#1999-2008? Use the ggplot2 plotting system to make a plot answer this question.

BaltimoreType = aggregate(Emissions~year+type,data=subset(NEI,fips == "24510"),sum)
png(filename = "plot3.png",width = 680, height = 680)
qplot(year, Emissions, data=BaltimoreType, color = type, facets = .~type, geom = c("point","smooth"), method = "lm", 
      main = "Baltimore Emissions by type and source")
dev.off()


#4. Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?

NEI_SCC = merge(NEI,SCC, by="SCC")
coalSN = grepl("coal", NEI_SCC$Short.Name, ignore.case=TRUE)
coalEI = grepl("coal", NEI_SCC$EI.Sector, ignore.case=TRUE)
coalL3 = grepl("coal", NEI_SCC$SCC.Level.Three, ignore.case=TRUE)
coalL4 = grepl("coal", NEI_SCC$SCC.Level.Four, ignore.case=TRUE)
BaltimoreCoalSN = aggregate(Emissions~year,data=subset(NEI_SCC, coalSN),sum)
BaltimoreCoalEI = aggregate(Emissions~year,data=subset(NEI_SCC, coalEI),sum)
BaltimoreCoalL3 = aggregate(Emissions~year,data=subset(NEI_SCC, coalL3),sum)
BaltimoreCoalL4 = aggregate(Emissions~year,data=subset(NEI_SCC, coalL4),sum)

png(filename = "plot4.png",width = 680, height = 680)
p1 = qplot(year, Emissions, data=BaltimoreCoalSN, geom = c("point","smooth"), method = "lm", 
           main ="Emission for Short Name coal combustion")
p2 = qplot(year, Emissions, data=BaltimoreCoalEI, geom = c("point","smooth"), method = "lm",
           main ="Emission for EI.Sector coal combustion")
p3 = qplot(year, Emissions, data=BaltimoreCoalL3, geom = c("point","smooth"), method = "lm", 
           main ="Emission for SCC Level 3 coal combustion")
p4 = qplot(year, Emissions, data=BaltimoreCoalL4, geom = c("point","smooth"), method = "lm", 
           main ="Emission for SCC Level 4 coal combustion")
grid.arrange(p1,p2,p3,p4)
dev.off()

#5.	How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

BaltimoreMotorVehicle = aggregate(Emissions~year,data=subset(NEI,fips == "24510" & type == "ON-ROAD"),sum)
png(filename = "plot5.png",width = 680, height = 680)
qplot(year, Emissions, data=BaltimoreMotorVehicle, geom = c("point","smooth"), method = "lm", 
      main = "Change in emissions form Motor vehicle sources from 1999-2008")
dev.off()

#6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions? 

MotorVehicleLA_Bal = aggregate(Emissions~year+fips,data=subset(NEI,(fips == "24510"|fips=="06037") & type == "ON-ROAD"),sum)
png(filename = "plot6.png",width = 480, height = 480)
ggplot(MotorVehicleLA_Bal,aes(x=factor(year),y=Emissions,fill=factor(fips)), color=factor(fips), main ="Change") +  
  stat_summary(fun.y=mean, geom="bar") + stat_smooth(aes(group=1),method="lm", se=FALSE, size=1.0)+
  facet_wrap(~fips) + ggtitle("Emissions of motor vehicles in Baltimore City vs. Los Angeles County")+ labs(x="Year", y = "Emissions")
dev.off()




