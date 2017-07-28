# Code to plot Timeseries Temperature Depth Data

source("R/ExtractBuoyData.R") #GetBuoyData
source("R/PlotBuoyTemp.R") # TempHeatTS
source('R/ImageScale.R')

library(lubridate)

# What data do you want (all characters)
# This is for downloading buoy data from lter data portal
# startdate<-"2016-01-01"
# enddate<-"2016-12-31"
# type="Daily"

# #Download Buoy Data from LTER server and crop as needed
# BuoyData<-GetBuoyTemp(startdate=startdate, enddate=enddate, type=type)
# BuoyData<-BuoyData[order(BuoyData$sampledate),]
# BuoyData$wtemp[BuoyData$wtemp<0]<-NA

# Load 2016 Data
BuoyData2016<-read.csv(file='Data/MendotaDaily2016.csv', header=T, stringsAsFactors = F)
BuoyData2016$sampledate<-as.Date(BuoyData2016$sampledate)
BuoyData2016<-BuoyData2016[BuoyData2016$year4==2016,]
BuoyData2016<-BuoyData2016[!is.na(BuoyData2016$wtemp),]
head(BuoyData2016)
str(BuoyData2016)


#Plot Lake Temperature Profile Time Series
# TempHeatTS(BuoyData)

png('Figures/TempBathyGram2016v2.png', width=7.15, height=2.5, units='in', res=200, bg='white')
par(pch=16)
par(ps=10)
par(mfrow=c(1,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)

xticks<-seq(ceiling_date(min(BuoyData2016$sampledate), "months"),floor_date(max(BuoyData2016$sampledate), "months"), by='months')
xlabels<-paste(month(xticks, label=TRUE, abbr=T), " 1", sep="")

TempHeatTS(BuoyData2016, xticks, xlabels)
mtext(expression(paste("Temperature (", degree, "C)", sep="")), 4, -6)

dev.off()




colors<-colorRampPalette(c("violet", "blue", "cyan", "green3", "yellow", "orange", "red"), bias = 1, space = "rgb")

breaks<-seq(min(BuoyData2016$wtemp), max(BuoyData2016$wtemp), length.out=100)

colors(99)

png('Figures/TempBathyGram2016v2ColorBar.png', width=1.3, height=.2, units='in', res=200, bg='white')
par(pch=16)
par(ps=10)
par(mgp=c(1.5,-1,0),tck=.2)

par(mar=c(0,0,0,0), bg=NA)
image.scale(BuoyData2016$wtemp, col=colors(99), breaks=breaks,axis.pos=3)
mtext(expression(paste('Temperature (', degree, 'C)')), 1, 0)
#abline(v=levs)
box(lwd=2)

dev.off()



