# Code to plot Timeseries Temperature Depth Data


source("R/ExtractBuoyData.R") #GetBuoyData
source("R/PlotBuoyTemp.R") # TempHeatTS

#What data do you want (all characters)
startdate<-"2016-01-01"
enddate<-"2016-12-31"
type="Daily"

# #Download Buoy Data from LTER server and crop as needed
# BuoyData<-GetBuoyTemp(startdate=startdate, enddate=enddate, type=type)
# BuoyData<-BuoyData[order(BuoyData$sampledate),]
# BuoyData$wtemp[BuoyData$wtemp<0]<-NA

BuoyData2016<-read.csv(file='Data/MendotaDaily2016.csv', header=T, stringsAsFactors = F)
BuoyData2016$sampledate<-as.Date(BuoyData2016$sampledate)
BuoyData2016<-BuoyData2016[BuoyData2016$year4==2016,]
head(BuoyData2016)
str(BuoyData2016)



#Plot Lake Temperature Profile Time Series
TempHeatTS(BuoyData)
TempHeatTS(BuoyData2016)
