

library(stringr)
library(openair)

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

split_dates<-as.Date(c('2016-10-28', '2016-11-01'))
split_data<-subset(SurfaceBuoyData2016, sampledate>=split_dates[1] & sampledate<=split_dates[2])



windRose(split_data, ws="avg_wind_speed", wd="avg_wind_dir", cols='jet', paddle=F, auto.text=F)



