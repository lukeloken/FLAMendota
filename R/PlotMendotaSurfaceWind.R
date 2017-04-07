

library(stringr)
library(openair)

#Load all hourly surface data
SurfaceBuoyData<-read.csv(file='Data/MendotaBuoySurfaceHourly.csv', header=T, stringsAsFactors = F)
SurfaceBuoyData$sampledate<-as.Date(SurfaceBuoyData$sampledate)
# Convert hour to four digit character (pad with zeros)
SurfaceBuoyData$hour<-str_pad(SurfaceBuoyData$hour, 4, pad = "0")
SurfaceBuoyData$datetime<-as.POSIXct(paste(SurfaceBuoyData$sampledate, SurfaceBuoyData$hour, sep=" "), format='%Y-%m-%d %H%M')
head(SurfaceBuoyData)
str(SurfaceBuoyData)

plot(SurfaceBuoyData$datetime, SurfaceBuoyData$avg_wind_dir, type="l")

windRose(SurfaceBuoyData, ws="avg_wind_speed", wd="avg_wind_dir", cols='hue', paddle=F, auto.text=F, border='black', grid.line=list(value=5, lty=5, col="gray"), offset=4)

# Load 2016 Surface Buoy Data
SurfaceBuoyData2016<-read.csv(file='Data/MendotaBuoySurfaceHourly2016.csv', header=T, stringsAsFactors = F)

SurfaceBuoyData2016$sampledate<-as.Date(SurfaceBuoyData2016$sampledate)
SurfaceBuoyData2016<-SurfaceBuoyData2016[SurfaceBuoyData2016$year4==2016,]
# Convert hour to four digit character (pad with zeros)
SurfaceBuoyData2016$hour<-str_pad(SurfaceBuoyData2016$hour, 4, pad = "0")
SurfaceBuoyData2016$datetime<-as.POSIXct(paste(SurfaceBuoyData2016$sampledate, SurfaceBuoyData2016$hour, sep=" "), format='%Y-%m-%d %H%M')
head(SurfaceBuoyData2016)
str(SurfaceBuoyData2016)

plot(SurfaceBuoyData2016$datetime, SurfaceBuoyData2016$avg_wind_dir, type="l")



#Plot Wind rose for entire summer

png('Figures/WindRose2016.png', width=4, height=5, units='in', res=200, bg='white')

par(mar = c(0,0.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)

windRose(SurfaceBuoyData2016, ws="avg_wind_speed", wd="avg_wind_dir", cols='hue', paddle=F, auto.text=F, border='black', grid.line=list(value=5, lty=5, col="gray"), offset=4)

dev.off()



split_dates<-as.Date(c('2016-09-20', '2016-09-20'))
split_data<-subset(SurfaceBuoyData2016, sampledate>=split_dates[1] & sampledate<=split_dates[2])


png(paste('Figures/WindRose', split_dates[2], '.png', sep=""), width=4, height=5, units='in', res=200, bg='white')

par(mar = c(0,0.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)

windRose(split_data, ws="avg_wind_speed", wd="avg_wind_dir", cols='hue', paddle=F, auto.text=F, border='black', grid.line=list(value=10, lty=5, col="gray"), offset=4)

dev.off()



