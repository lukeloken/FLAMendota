# Code to plot Timeseries Temperature Depth Data
# Input is a dataframe consisting of "sampledate", "depth", and "wtemp" as columns. 


TempHeatTS<-function (LTERdf){
  
  depths = unique(LTERdf$depth)
  dates = unique(LTERdf$sampledate)
  
  #Make Matrix of temperature (x=depth, y=date, z=temp)
  wrt<-matrix(ncol=length(depths), nrow=length(dates))
  for (i in 1:length(dates)){
    for (j in 1:length(depths)){
      wrt[i,j]<-LTERdf$wtemp[LTERdf$depth==depths[j] & LTERdf$sampledate==dates[i]]
    }
  }
  
  filled.contour(x=dates, y=depths, z=wrt, ylim=c(max(depths), 0), nlevels = 100, color.palette = colorRampPalette(c("violet", "blue", "cyan", "green3", "yellow", "orange", "red"), bias = 1, space = "rgb"), ylab="Depths (m)")
  
}