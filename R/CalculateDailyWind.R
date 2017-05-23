# #####################################
# Code takes hourly wind data from Lake Mendota 
# Aggregates to a daily mean summary
# input is Lake Mendota Buoy Wind data
# output is a dataframe
# columns are (date, speed, direction, direction_volatility)
# ########################################################

library(stringr)
library(openair)
library(circular)

SurfaceBuoyData2016<-read.csv(file='Data/MendotaBuoySurfaceHourly2016.csv', header=T, stringsAsFactors = F)

SurfaceBuoyData2016$sampledate<-as.Date(SurfaceBuoyData2016$sampledate)
SurfaceBuoyData2016<-SurfaceBuoyData2016[SurfaceBuoyData2016$year4==2016,]
# Convert hour to four digit character (pad with zeros)
SurfaceBuoyData2016$hour<-str_pad(SurfaceBuoyData2016$hour, 4, pad = "0")
SurfaceBuoyData2016$datetime<-as.POSIXct(paste(SurfaceBuoyData2016$sampledate, SurfaceBuoyData2016$hour, sep=" "), format='%Y-%m-%d %H%M')

# Check out data
head(SurfaceBuoyData2016)
str(SurfaceBuoyData2016)
plot(SurfaceBuoyData2016$datetime, SurfaceBuoyData2016$avg_wind_dir, type="l")

# Loop through dates and aggregate at the daily

daynumbers<-unique(SurfaceBuoyData2016$daynum)
daynumbers<-daynumbers[order(daynumbers)]
dailywind<-data.frame(daynumbers)
dailywind$sampledate<-SurfaceBuoyData2016$sampledate[match(dailywind$daynumbers,SurfaceBuoyData2016$daynum)]
dailywind$speed<-NA
dailywind$speedvariance<-NA
dailywind$dir<-NA
dailywind$dirvariance<-NA
dailywind$dirdeviation<-NA

# daynum=daynumbers[3]
for (daynum in daynumbers){
  DailyData<-SurfaceBuoyData2016[SurfaceBuoyData2016$daynum==daynum,]

  if(length(which(!is.na(DailyData$avg_wind_speed)))>0) {
    #wind spped
    avg_windspeed<-mean(DailyData$avg_wind_speed, na.rm=T)
    variance_windspeed<-var(DailyData$avg_wind_speed, na.rm=T)
    #wind direction
    avg_winddirection<-mean(circular(DailyData$avg_wind_dir, units = "degrees"), na.rm=T)
    if (avg_winddirection<0){
      avg_winddirection=360+avg_winddirection}
    variance_winddirection <- angular.variance(circular(DailyData$avg_wind_dir, units = "degrees"), na.rm = T)
    deviation_winddirection <- angular.deviation(circular(DailyData$avg_wind_dir, units = "degrees"), na.rm = T)
    
    
    # plot data
    png(paste('Figures/DailyWindRose2016/daynum', daynum, '.png', sep=""), width=4, height=5, units='in', res=200, bg='white')
    par(mar = c(0,0.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
    windRose(DailyData, ws="avg_wind_speed", wd="avg_wind_dir", cols='hue', paddle=F, auto.text=F, border='black', grid.line=list(value=10, lty=5, col="gray"), offset=4, main=paste('2016 day #', daynum, sep=""))
    dev.off()
    
    # Outputs
    dailywind$speed[which(dailywind$daynum==daynum)]<-avg_windspeed
    dailywind$speedvariance[which(dailywind$daynum==daynum)]<-variance_windspeed
    dailywind$dir[which(dailywind$daynum==daynum)]<-as.numeric(avg_winddirection)
    dailywind$dirvariance[which(dailywind$daynum==daynum)]<- variance_winddirection
    dailywind$dirdeviation[which(dailywind$daynum==daynum)]<-deviation_winddirection
  }
}

# Look at the output
# dailywind                                                  
# plot(dailywind$sampledate, dailywind$speed, type="l")
# plot(dailywind$sampledate, dailywind$speedvariance, type="l")
# plot(dailywind$sampledate, dailywind$dir, type="l")
# plot(dailywind$sampledate, dailywind$dirvariance, type="l")
# plot(dailywind$sampledate, dailywind$dirdeviation, type="l")

# Save to Git folder
saveRDS(dailywind , file='Data/Dailywind.rds')
write.table(dailywind , file='Data/Dailywind.csv')
