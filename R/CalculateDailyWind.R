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

SurfaceBuoyData2016<-read.csv(file='Data/me_met_2016.csv', header=T, stringsAsFactors = F)
SurfaceBuoyData2016$sampledate<-as.Date(SurfaceBuoyData2016$sampledate, format = '%m/%d/%Y')
SurfaceBuoyData2016<-SurfaceBuoyData2016[SurfaceBuoyData2016$year4==2016,]

# Convert hour to four digit character (pad with zeros)
SurfaceBuoyData2016$sample_time<-str_pad(SurfaceBuoyData2016$sample_time, 4, pad = "0")
SurfaceBuoyData2016$datetime<-as.POSIXct(paste(SurfaceBuoyData2016$sampledate, SurfaceBuoyData2016$sample_time, sep=" "), format='%Y-%m-%d %H%M')
# 

##############################################################
# Old Data This was previuosly aggreagated by LTER incorrectly
##############################################################

# SurfaceBuoyData2016<-read.csv(file='Data/MendotaBuoySurfaceHourly2016.csv', header=T, stringsAsFactors = F)
# 
# SurfaceBuoyData2016$sampledate<-as.Date(SurfaceBuoyData2016$sampledate)
# SurfaceBuoyData2016<-SurfaceBuoyData2016[SurfaceBuoyData2016$year4==2016,]
# # Convert hour to four digit character (pad with zeros)
# SurfaceBuoyData2016$hour<-str_pad(SurfaceBuoyData2016$hour, 4, pad = "0")
# SurfaceBuoyData2016$datetime<-as.POSIXct(paste(SurfaceBuoyData2016$sampledate, SurfaceBuoyData2016$hour, sep=" "), format='%Y-%m-%d %H%M')
# 
# # Check out data
# head(SurfaceBuoyData2016)
# str(SurfaceBuoyData2016)
# plot(SurfaceBuoyData2016$datetime, SurfaceBuoyData2016$avg_wind_dir, type="l")

# Loop through dates and aggregate at the daily

daynumbers<-unique(SurfaceBuoyData2016$daynum)
daynumbers<-daynumbers[order(daynumbers)]
dailywind<-data.frame(daynumbers)
dailywind$sampledate<-SurfaceBuoyData2016$sampledate[match(dailywind$daynumbers,SurfaceBuoyData2016$daynum)]
dailywind$speed<-NA
dailywind$speedvariance<-NA
dailywind$speeddeviation<-NA
dailywind$dir<-NA
dailywind$dirvariance<-NA
dailywind$dirdeviation<-NA
dailywind$dir_Mitsuta<-NA
dailywind$dir_sd_Mitsuta<-NA
dailywind$wind_observations<-NA

daynum=daynumbers[1]
for (daynum in daynumbers){
  DailyData<-SurfaceBuoyData2016[SurfaceBuoyData2016$daynum==daynum,]
  N<-length(which(!is.na(DailyData$wind_speed) & !is.na(DailyData$wind_dir))) 

  if(N>0) {
    #wind spped
    avg_windspeed<-mean(DailyData$wind_speed, na.rm=T)
    variance_windspeed<-var(DailyData$wind_speed, na.rm=T)
    deviation_windspeed<-sd(DailyData$wind_speed, na.rm=T)
    #wind direction
    avg_winddirection<-mean(circular(DailyData$wind_dir, units = "degrees"), na.rm=T)
    if (avg_winddirection<0){
      avg_winddirection=360+avg_winddirection}
    #angular variance and standard deviation (both in radians)
    variance_winddirection <- angular.variance(circular(DailyData$wind_dir, units = "degrees"), na.rm = T)
    deviation_winddirection <- angular.deviation(circular(DailyData$wind_dir, units = "degrees"), na.rm = T)
    
    #Abelian.org bearing calculation
    # Mitsuta method
    
    b<-DailyData$wind_dir[is.finite(DailyData$wind_dir)]
    N=length(b)
    sum = D = b[1]
    sumsq = D * D
    
    i=2
    for( i in 2:N){
      delta = b[i] - D
      if( delta < (-180)){
        D = D + delta + 360}
      else if (delta < 180){
        D = D + delta}
      else {
        D = D + delta - 360}
      sum = sum + D
      sumsq = sumsq + D * D
    }
    
    avg_winddirection_Mitsuta = sum/N
    std_dev_winddirection_Mitsuta = sqrt( sumsq/N - avg_winddirection_Mitsuta * avg_winddirection_Mitsuta)
    if (avg_winddirection_Mitsuta<0){
      avg_winddirection_Mitsuta=360+avg_winddirection_Mitsuta}
    else if (avg_winddirection_Mitsuta>=360){
      avg_winddirection_Mitsuta=avg_winddirection_Mitsuta-360}

    # plot data
    png(paste('Figures/DailyWindRose2016/daynum', daynum, '.png', sep=""), width=4, height=5, units='in', res=200, bg='white')
    par(mar = c(0,0.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
    windRose(DailyData, ws="wind_speed", wd="wind_dir", cols='hue', paddle=F, auto.text=F, border='black', grid.line=list(value=10, lty=5, col="gray"), offset=4, main=paste('2016 day #', daynum, sep=""))
    dev.off()
    
    # Outputs
    dailywind$speed[which(dailywind$daynum==daynum)]<-avg_windspeed
    dailywind$speedvariance[which(dailywind$daynum==daynum)]<-variance_windspeed
    dailywind$speeddeviation[which(dailywind$daynum==daynum)]<-deviation_windspeed
    dailywind$dir[which(dailywind$daynum==daynum)]<-as.numeric(avg_winddirection)
    dailywind$dirvariance[which(dailywind$daynum==daynum)]<- variance_winddirection
    dailywind$dirdeviation[which(dailywind$daynum==daynum)]<-deviation_winddirection
    dailywind$dir_Mitsuta[which(dailywind$daynum==daynum)]<-avg_winddirection_Mitsuta
    dailywind$dir_sd_Mitsuta[which(dailywind$daynum==daynum)]<-std_dev_winddirection_Mitsuta
    dailywind$wind_observations[which(dailywind$daynum==daynum)]<-N
  }
}

# Look at the output
dailywind
par(mar=c(3,3,1,1))
plot(dailywind$sampledate, dailywind$speed, type="l")
plot(dailywind$sampledate, dailywind$speedvariance, type="l")
plot(dailywind$sampledate, dailywind$dir, type="l")
plot(dailywind$sampledate, dailywind$dirvariance, type="l")
plot(dailywind$sampledate, dailywind$dirdeviation, type="l")

breakpoint<-55
plot(dailywind$dir, dailywind$dir_Mitsuta)
points(dailywind$dir[dailywind$dir_sd_Mitsuta<breakpoint], dailywind$dir_Mitsuta[dailywind$dir_sd_Mitsuta<breakpoint], col="red", pch=16)
points(dailywind$dir[dailywind$dir_sd_Mitsuta>=breakpoint], dailywind$dir_Mitsuta[dailywind$dir_sd_Mitsuta>=breakpoint], col="blue", pch=16)
abline(0,1)

png(paste('Figures/SummaryWindRose2016.png', sep=""), width=4, height=5, units='in', res=200, bg='white')
par(mar = c(0,0.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
windRose(dailywind, ws="speed", wd="dir", cols='hue', paddle=F, auto.text=F, border='black', grid.line=list(value=10, lty=5, col="gray"), offset=4, main=paste('2016 daily averages', sep=""), dig.lab=3)
dev.off()

# Save to Git folder
saveRDS(dailywind , file='Data/Dailywind.rds')
write.table(dailywind , file='Data/Dailywind.csv')
