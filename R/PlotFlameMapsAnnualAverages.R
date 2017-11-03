



library(rgdal)
library(animation)
library(magick)
library(lubridate)

imagedates<-c('16940', '17031', '17059', '17114')


#Load list of maps
maps_list<-readRDS(file='Data/All2016Maps.rds')
names_list<-names(maps_list)
dates_list1<-sub('LakeMendota', '', names_list)
names(maps_list)<-dates_list<-ymd(sub('cleaned', '', dates_list1))

# Load flame array
ConcArray<-readRDS(file='Data/DailyFlamebyPixel.rds')
dailydates<-dimnames(ConcArray)[[1]]
date_names<-seq.Date(as.Date('2016-03-29'), as.Date('2016-12-02'), by='day')

PixelSummary$CH4Conc<-colMeans(ConcArray[,,c('CH4uM_t')])
PixelSummary$CO2Conc<-colMeans(ConcArray[,,c('CO2uM_t')])
PixelSummary$CH4Conc<-colMeans(ConcArray[,,c('CH4uM_t')])
PixelSummary$CO2Conc<-colMeans(ConcArray[,,c('CO2uM_t')])


# Load pixelIDs
coordinates<-readRDS(file='Data/PixelIDs.rds')

# Set UTM projection (Zone 15 for Wisconsin)
projection <- "+proj=utm +zone=15 ellps=WGS84"

# Outline of Lake Mendota
basemap_dir<-"E:/Dropbox/FLAME/basemaps"
Mendota_Shoreline<-readOGR(basemap_dir, "Mendota_shape")
# Project into UTM's. This way distance is in meters (m)
shoreline <- spTransform(Mendota_Shoreline, CRS(projection))



time=imagedates[1]
dates<-dailydates[]

CO2Range2<-range(c(ConcArray[imagedates,,c('CO2uM_t')]))
CO2breaks2<-seq(CO2Range2[1],CO2Range2[2], length.out=99)

CH4Range2<-range(c(ConcArray[imagedates,,c('CH4uM_t')]))
CH4breaks2<-seq(CH4Range2[1],CH4Range2[2], length.out=99)


SPCRange2<-range(c(ConcArray[imagedates,,c('SPCuScm')]))
SPCbreaks2<-seq(SPCRange2[1],SPCRange2[2], length.out=99)

ChlRange2<-range(c(ConcArray[imagedates,,c('ChlAugL')]))
Chlbreaks2<-seq(ChlRange2[1],ChlRange2[2], length.out=99)

BGARange2<-range(c(ConcArray[imagedates,,c('BGAPCgL')]))
BGAbreaks2<-seq(BGARange2[1],BGARange2[2], length.out=99)

ODORange2<-range(c(ConcArray[imagedates,,c('ODOsat')]))
ODObreaks2<-seq(ODORange2[1],ODORange2[2], length.out=99)

fDOMRange2<-range(c(ConcArray[imagedates,,c('fDOMRFU')]))
fDOMbreaks2<-seq(fDOMRange2[1],fDOMRange2[2], length.out=99)



for (time in imagedates){
  daynumber<-which(time==dailydates)
  date_name<-date_names[daynumber]
  
  maps_nu<-which(names(maps_list)==as.character(date_name))
  map<-maps_list[[maps_nu]]
  gridded(map)<-TRUE
  
  #non carbon plots
  if(time ==imagedates[1]){
    png(paste('Figures/MultiMapPanelForPub/SPC/vertical', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
    print(spplot(map, zcol='SPCuScm', at=SPCbreaks2, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F)))
    dev.off()
    
    png(paste('Figures/MultiMapPanelForPub/ODO/vertical', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
    print(spplot(map, zcol='ODOsat', at=ODObreaks2, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F)))
    dev.off()
    
    png(paste('Figures/MultiMapPanelForPub/fDOM/vertical', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
    print(spplot(map, zcol='fDOMRFU', at=fDOMbreaks2, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F)))
    dev.off()
    
    png(paste('Figures/MultiMapPanelForPub/BGA/vertical', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
    print(spplot(map, zcol='BGAPCgL', at=BGAbreaks2, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F)))
    dev.off()
    
    png(paste('Figures/MultiMapPanelForPub/ChlA/vertical', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
    print(spplot(map, zcol='ChlAugL', at=Chlbreaks2, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F)))
    dev.off()
    
  }
  
  
  png(paste('Figures/MultiMapPanelForPub/SPC/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
  print(spplot(map, zcol='SPCuScm', at=SPCbreaks2, colorkey=F, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F)))
  dev.off()
  
  png(paste('Figures/MultiMapPanelForPub/ODO/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
  print(spplot(map, zcol='ODOsat', at=ODObreaks2, colorkey=F, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F)))
  dev.off()
  
  png(paste('Figures/MultiMapPanelForPub/fDOM/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
  print(spplot(map, zcol='fDOMRFU', at=fDOMbreaks2, colorkey=F, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F)))
  dev.off()
  
  png(paste('Figures/MultiMapPanelForPub/BGA/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
  print(spplot(map, zcol='BGAPCgL', at=BGAbreaks2, colorkey=F, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F)))
  dev.off()
  
  png(paste('Figures/MultiMapPanelForPub/ChlA/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
  print(spplot(map, zcol='ChlAugL', at=Chlbreaks2, colorkey=F, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F)))
  dev.off()
  
}
