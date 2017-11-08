# Code to plot Timeseries Temperature Depth Data
# Input is a dataframe consisting of "sampledate", "depth", and "wtemp" as columns. 

library(tidyr)
library(gtools)
library(viridis)
library(RcppRoll)

TempHeatTS<-function (LTERdf, ticks, labels, td=FALSE){
  
  depths = unique(LTERdf$depth)
  dates = seq(min(LTERdf$sampledate), max(LTERdf$sampledate), by='days')
  dates.df<-data.frame(sampledate = c(dates))
  
  #Make Matrix of temperature (x=depth, y=date, z=temp)
  wrt1<-spread(LTERdf[c('sampledate', 'depth', 'wtemp')], key=depth, value=wtemp, fill=NA )
  wrt2<-merge(wrt1, dates.df, by='sampledate', all=T)
  wrt<-as.matrix(wrt2[,-1])
  
  if (td==T){
  t.d<-apply(wrt1[,-1], 1, function (x) thermo.depth(x, depths=depths, Smin=.05)[1])
  t.d.roll<-roll_mean(t.d, n=3, align='center', fill=NA)
  } else { 
    t.d.roll<-rep(NA, nrow(wrt1))
    }
  
  #Plot
  # filled.contour(x=dates, y=depths, z=wrt, ylim=c(max(depths), 0), nlevels = 100, color.palette = colorRampPalette(c("violet", "blue", "cyan", "green3", "yellow", "orange", "red"), bias = 1, space = "rgb"), ylab="Depth (m)")
#   
#   filled.contour(x=dates, y=depths, z=wrt, ylim=c(max(depths), 0), nlevels = 100, color.palette = colorRampPalette(c("navy", "blue", "cyan", "green3", "yellow", "orange", "red"), bias = 1, space = "rgb"), ylab="Depth (m)", plot.axes = { axis(1, at=ticks, labels=labels); axis(2) })
  
#   filled.contour(x=dates, y=depths, z=wrt, ylim=c(max(depths), 0), nlevels = 100, color.palette = colorRampPalette(c(viridis(14, begin=.00, end=.99), rev(magma(10, begin=.4, end=.85))), bias=1), ylab="Depth (m)", plot.axes = { axis(1, at=ticks, labels=labels); axis(2) })
#   
  filled.contour(x=dates, y=depths, z=wrt, ylim=c(max(depths), 0), nlevels = 100, color.palette = colorRampPalette(c(viridis(8, begin=.00, end=.99), c('#fed976',  '#feb24c',  '#fd8d3c',  '#f03b20',  '#bd0026', '#800026')), bias=1), ylab="Depth (m)", plot.axes = { axis(1, at=ticks, labels=labels); axis(2); lines(wrt1[,1],t.d.roll, lwd=1, col='black') })
  

  
  # filled.contour(x=dates, y=depths, z=wrt, ylim=c(max(depths), 0), nlevels = 100, color.palette = colorRampPalette(rev(c('#a50026', '#d73027', '#f46d43', '#fdae61', '#fee090', '#e0f3f8', '#abd9e9', '#74add1', '#4575b4', '#313695')), bias = 1, space = "rgb"), ylab="Depth (m)", plot.axes = { axis(1, at=ticks, labels=labels); axis(2) })
#   
#   filled.contour(x=dates, y=depths, z=wrt, ylim=c(max(depths), 0), nlevels = 100, color.palette = colorRampPalette(rev(c('#67001f', '#9e0142', '#d53e4f', '#f46d43', '#fdae61', '#fee08b', '#e6f598', '#abdda4', '#66c2a5', '#3288bd', '#5e4fa2', '#40004b')), bias = 1, space = "rgb"), ylab="Depth (m)", plot.axes = { axis(1, at=ticks, labels=labels); axis(2) })
  
  
  
  
}