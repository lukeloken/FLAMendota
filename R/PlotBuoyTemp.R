# Code to plot Timeseries Temperature Depth Data


source("R/ExtractBuoyData.R")

startdate<-"2015-01-31"
enddate<-"2015-12-31"
type="Daily"


BuoyData<-GetBuoyTemp(startdate=startdate, enddate=enddate, type=type)
BuoyData<-BuoyData[order(BuoyData$sampledate),]
BuoyData$wtemp[BuoyData$wtemp<0]<-NA

# LTERdf<-BuoyData

source("R/PlotBuoyTemp.R")

TempHeatTS<-function (LTERdf){
  

  depths = unique(LTERdf$depth)
  dates = unique(LTERdf$sampledate)
  
  wrt<-matrix(ncol=length(depths), nrow=length(dates))
  
  for (i in 1:length(dates)){
    for (j in 1:length(depths)){
      wrt[i,j]<-LTERdf$wtemp[LTERdf$depth==depths[j] & LTERdf$sampledate==dates[i]]
    }
  }
  
  # n = length(dates)
  filled.contour(x=dates, y=depths, z=wrt, ylim=c(max(depths), 0), nlevels = 100, color.palette = colorRampPalette(c("violet", "blue", "cyan", "green3", "yellow", "orange", "red"), bias = 1, space = "rgb"), ylab="Depths (m)")
  
}