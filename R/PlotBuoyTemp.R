# Code to plot Timeseries Temperature Depth Data
# Input is a dataframe consisting of "sampledate", "depth", and "wtemp" as columns. 

library(tidyr)
library(gtools)

TempHeatTS<-function (LTERdf, ticks, labels){
  
  depths = unique(LTERdf$depth)
  dates = seq(min(LTERdf$sampledate), max(LTERdf$sampledate), by='days')
  dates.df<-data.frame(sampledate = c(dates))
  
  #Make Matrix of temperature (x=depth, y=date, z=temp)
  wrt1<-spread(LTERdf[c('sampledate', 'depth', 'wtemp')], key=depth, value=wtemp, fill=NA )
  wrt2<-merge(wrt1, dates.df, by='sampledate', all=T)
  wrt<-as.matrix(wrt2[,-1])
  
  #Plot
  # filled.contour(x=dates, y=depths, z=wrt, ylim=c(max(depths), 0), nlevels = 100, color.palette = colorRampPalette(c("violet", "blue", "cyan", "green3", "yellow", "orange", "red"), bias = 1, space = "rgb"), ylab="Depth (m)")
  
  filled.contour(x=dates, y=depths, z=wrt, ylim=c(max(depths), 0), nlevels = 100, color.palette = colorRampPalette(c("violet", "blue", "cyan", "green3", "yellow", "orange", "red"), bias = 1, space = "rgb"), ylab="Depth (m)", plot.axes = { axis(1, at=ticks, labels=labels); axis(2) })
  
}