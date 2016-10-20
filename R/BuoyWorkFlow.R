# Code to plot Timeseries Temperature Depth Data


source("R/ExtractBuoyData.R") #GetBuoyData
source("R/PlotBuoyTemp.R") # TempHeatTS

#What data do you want (all characters)
startdate<-"2012-01-31"
enddate<-"2015-12-31"
type="Daily"

#Download Buoy Data from LTER server and crop as needed
BuoyData<-GetBuoyTemp(startdate=startdate, enddate=enddate, type=type)
BuoyData<-BuoyData[order(BuoyData$sampledate),]
BuoyData$wtemp[BuoyData$wtemp<0]<-NA

#Plot Lake Temperature Profile Time Series
TempHeatTS(BuoyData)
