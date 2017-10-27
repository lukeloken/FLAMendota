# ################################################################
# Code to Merge Concentration and K data to generate flux estimates
# ################################################################

library(rgdal)
library(animation)
library(magick)



#load atmosphere data
Atm<-readRDS('Data/FlameBuoyAtmMeasurements.rds')
atmCO2<-median(Atm$XCO2Dppm)
atmCH4<-median(Atm$XCH4Dppm)

# Load concentration array
ConcArray<-readRDS(file='Data/DailyFlamebyPixel.rds')
dailydates<-dimnames(ConcArray)[[1]]

# Load pixelIDs
coordinates<-readRDS(file='Data/PixelIDs.rds')

# Set UTM projection (Zone 15 for Wisconsin)
projection <- "+proj=utm +zone=15 ellps=WGS84"

# Outline of Lake Mendota
basemap_dir<-"E:/Dropbox/FLAME/basemaps"
Mendota_Shoreline<-readOGR(basemap_dir, "Mendota_shape")
# Project into UTM's. This way distance is in meters (m)
shoreline <- spTransform(Mendota_Shoreline, CRS(projection))


# Load K list of maps
Klist<-readRDS(file='Data/DailyFetchK600maps.rds')
str(Klist[[1]])

k600_dataframe<-readRDS('Data/DailyK600Stats.rds')

# Wind
dailywind <-readRDS('Data/Dailywind.rds')
numericdates<-as.numeric(dailywind$sampledate)
gooddates<-which(numericdates %in% dailydates)
subsetKlist<-Klist[gooddates]

date_names<-dailywind$sampledate[gooddates]

#convert k maps into array
# dimensions (date, pixelID)
Kmatrix<-array(data=NA, dim=c(length(dailydates), nrow(coordinates)), dimnames=list(dailydates, coordinates$pixelID))

Kmap<-1
for (Kmap in 1:length(dailydates)){
  dailydata<-subsetKlist[[Kmap]]
  Kmatrix[Kmap,]<-dailydata$k600
}

# Save k matrix
saveRDS(Kmatrix, file='Data/Kmatrix.rds')


# Atmospheric equilibrium array
Atmmatrix<-array(data=NA, dim=c(length(dailydates), nrow(coordinates), 2), dimnames=list(dailydates, coordinates$pixelID, c('CO2', 'CH4')))

#  Flux matrix
Fluxmatrix<-Atmmatrix

time=dailydates[1]
for (time in dailydates){
  daynumber<-which(time==dailydates)
  date_name<-date_names[daynumber]
  # DailyArray<-ConcArray[time,,]
  DailyCO2Sat<-ConcArray[time,,c('CO2St_t')]
  DailyCO2uM<-ConcArray[time,,c('CO2uM_t')]
  
  DailyCO2Diff<-DailyCO2uM-100*(DailyCO2uM/DailyCO2Sat)
  
  DailyCH4Sat<-ConcArray[time,,c('CH4St_t')]
  DailyCH4uM<-ConcArray[time,,c('CH4uM_t')]
  
  DailyCH4Diff<-DailyCH4uM-100*(DailyCH4uM/DailyCH4Sat)
  
  Atmmatrix[time,,'CO2']<-DailyCO2Diff
  Atmmatrix[time,,'CH4']<-DailyCH4Diff
  
  #Calculate flux (multiple by .24 to convert to mmol/m2/day)
  # Concentration is in (umol per liter)
  # k is in (cm/hr)
  # Double check all units
  Fluxmatrix[time,,'CO2']<-Atmmatrix[time,,'CO2']*Kmatrix[time,]*.24
  Fluxmatrix[time,,'CH4']<-Atmmatrix[time,,'CH4']*Kmatrix[time,]*.24
  
  subsetKlist[[daynumber]]$CO2Conc<-DailyCO2uM
  subsetKlist[[daynumber]]$CH4Conc<-DailyCH4uM
  
  subsetKlist[[daynumber]]$CO2Flux<-Fluxmatrix[time,,'CO2']
  subsetKlist[[daynumber]]$CH4Flux<-Fluxmatrix[time,,'CH4']
  
  
  #Concentration maps
#   png(paste('Figures/DailyCO2ConcMap2016/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
#   print(spplot(subsetKlist[[daynumber]], zcol='CO2Conc', cuts=20,  colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(CO[2], " (", mu, "M)")), xlab=paste(date_name)))
#   dev.off()
#   
#   png(paste('Figures/DailyCH4ConcMap2016/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
#   print(spplot(subsetKlist[[daynumber]], zcol='CH4Conc', cuts=20,  colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(CH[4], " (", mu, "M)")), xlab=paste(date_name)))
#   dev.off()
#   
#   
#   png(paste('Figures/DailyCO2FluxMap2016/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
#   print(spplot(subsetKlist[[daynumber]], zcol='CO2Flux', cuts=20,  colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(CO[2], " efflux (mmol/m2/d)  ")), xlab=paste(date_name)))
#   dev.off()
#   
#   png(paste('Figures/DailyCH4FluxMap2016/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
#   print(spplot(subsetKlist[[daynumber]], zcol='CH4Flux', cuts=20,  colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(CH[4], " efflux (mmol/m2/d)  ")), xlab=paste(date_name)))
#   dev.off()
  
}


#Set concentration and flux ranges
# This way images are constant color ramp and more suitable for timeseries comparison. 
CO2Range<-range(c(ConcArray[,,c('CO2uM_t')]))
CO2breaks<-seq(CO2Range[1],CO2Range[2], length.out=99)

CH4Range<-range(c(ConcArray[,,c('CH4uM_t')]))
CH4breaks<-seq(CH4Range[1],CH4Range[2], length.out=99)

CO2FluxRange<-range(Fluxmatrix[,,'CO2'])
CO2Fluxbreaks<-seq(CO2FluxRange[1],CO2FluxRange[2], length.out=99)

CH4FluxRange<-range(Fluxmatrix[,,'CH4'])
CH4Fluxbreaks<-seq(CH4FluxRange[1],CH4FluxRange[2], length.out=99)

#Empty lists to fill with images
plot.co2.conc<-list()
plot.ch4.conc<-list()
plot.co2.flux<-list()
plot.ch4.flux<-list()


time=dailydates[1]
for (time in dailydates){
  daynumber<-which(time==dailydates)
  date_name<-date_names[daynumber]
  
  png(paste('Figures/DailyCO2Movie2016/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
  plot.co2.conc[[daynumber]]<-spplot(subsetKlist[[daynumber]], zcol='CO2Conc', at=CO2breaks, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(CO[2], " (", mu, "M)")), xlab=paste(date_name))
  print(plot.co2.conc[[daynumber]])
  dev.off()
  
  png(paste('Figures/DailyCH4Movie2016/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
  plot.ch4.conc[[daynumber]]<-spplot(subsetKlist[[daynumber]], zcol='CH4Conc', at=CH4breaks,  colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(CH[4], " (", mu, "M)")), xlab=paste(date_name))
  print(plot.co2.conc[[daynumber]])
  dev.off()

  png(paste('Figures/DailyCO2FluxMovie2016/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
  plot.co2.flux[[daynumber]]<-(spplot(subsetKlist[[daynumber]], zcol='CO2Flux', at=CO2Fluxbreaks, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(CO[2], " efflux (mmol/m2/d)  ")), xlab=paste(date_name)))
  print(plot.co2.flux[[daynumber]])
  dev.off()
  
  png(paste('Figures/DailyCH4FluxMovie2016/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
  plot.ch4.flux[[daynumber]]<-(spplot(subsetKlist[[daynumber]], zcol='CH4Flux', at=CH4Fluxbreaks, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(CH[4], " efflux (mmol/m2/d)  ")), xlab=paste(date_name)))
  print(plot.ch4.flux[[daynumber]])
  dev.off()

}

img <- image_graph(480, 400, res = 48)
rate<-4
#CO2 Concentration Annimation
img1 <- img
p <- for (i in seq(1, length(plot.co2.conc), 1)) {
# p <- for (i in 1:49) {
  print(plot.co2.conc[[i]])}
out<-print(p)
dev.off()

img1 <- image_background(image_trim(img1), 'white')
animation <- image_animate(img1, fps = rate)
# print(animation)
image_write(animation, "Figures/Animations/CO2ConcentrationDaily.gif")
rm(list=c('img1', 'animation', 'out', 'p'))
dev.off()

#CH4 Concentration Annimation
img1 <- img
p <- for (i in seq(1, length(plot.ch4.conc), 1)) {
  # p <- for (i in 1:49) {
  print(plot.ch4.conc[[i]])}
out<-print(p)
dev.off()

img1 <- image_background(image_trim(img1), 'white')
animation <- image_animate(img1, fps = rate)
# print(animation)
image_write(animation, "Figures/Animations/CH4ConcentrationDaily.gif")
rm(list=c('img1', 'animation', 'out', 'p'))
dev.off()

#CO2 Flux Annimation
img1 <- img
p <- for (i in seq(1, length(plot.co2.flux), 1)) {
  # p <- for (i in 1:49) {
  print(plot.co2.flux[[i]])}
out<-print(p)
dev.off()

img1 <- image_background(image_trim(img1), 'white')
animation <- image_animate(img1, fps = 4)
# print(animation)
image_write(animation, "Figures/Animations/CO2FluxDaily.gif")
rm(list=c('img1', 'animation', 'out', 'p'))
dev.off()

#CH4 Flux Annimation
img1 <- img
p <- for (i in seq(1, length(plot.ch4.flux), 1)) {
  # p <- for (i in 1:49) {
  print(plot.ch4.flux[[i]])}
out<-print(p)
dev.off()

img1 <- image_background(image_trim(img1), 'white')
animation <- image_animate(img1, fps = 4)
# print(animation)
image_write(animation, "Figures/Animations/CH4FluxDaily.gif")
rm(list=c('img1', 'animation', 'out', 'p'))
dev.off()

#End Animations




imagedates<-c('16940', '17031', '17059', '17114')
time=imagedates[1]

#Set concentration and flux ranges
# This way images are constant color ramp and more suitable for timeseries comparison. 
CO2Range2<-range(c(ConcArray[imagedates,,c('CO2uM_t')]))
CO2breaks2<-seq(CO2Range2[1],CO2Range2[2], length.out=99)

CH4Range2<-range(c(ConcArray[imagedates,,c('CH4uM_t')]))
CH4breaks2<-seq(CH4Range2[1],CH4Range2[2], length.out=99)

CO2FluxRange2<-range(Fluxmatrix[imagedates,,'CO2'])
CO2Fluxbreaks2<-seq(CO2FluxRange2[1],CO2FluxRange2[2], length.out=99)

CH4FluxRange2<-range(Fluxmatrix[imagedates,,'CH4'])
CH4Fluxbreaks2<-seq(CH4FluxRange2[1],CH4FluxRange2[2], length.out=99)



for (time in imagedates){
  daynumber<-which(time==dailydates)
  date_name<-date_names[daynumber]
  
  if (time ==imagedates[1]){
    png(paste('Figures/MultiMapPanelForPub/CO2Conc/horizontal', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
    print(spplot(subsetKlist[[daynumber]], zcol='CO2Conc', at=CO2breaks2, colorkey = list(space = "bottom", height = 0.8, width=2), sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F)))
    dev.off()
    
    png(paste('Figures/MultiMapPanelForPub/CH4Conc/horizontal', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
    print(spplot(subsetKlist[[daynumber]], zcol='CH4Conc', at=CH4breaks2, colorkey = list(space = "bottom", height = 0.8, width=2), sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F)))
    dev.off()
    
    png(paste('Figures/MultiMapPanelForPub/CO2Conc/vertical', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
    print(spplot(subsetKlist[[daynumber]], zcol='CO2Conc', at=CO2breaks2, colorkey = list(space = "right", height = 0.9, width=2), sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F)))
    dev.off()
    
    png(paste('Figures/MultiMapPanelForPub/CH4Conc/vertical', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
    print(spplot(subsetKlist[[daynumber]], zcol='CH4Conc', at=CH4breaks2, colorkey = list(space = "right", height = 0.9, width=2), sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F)))
    dev.off()
    
  }
  
  png(paste('Figures/MultiMapPanelForPub/CO2Conc/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
  print(spplot(subsetKlist[[daynumber]], zcol='CO2Conc', at=CO2breaks2, colorkey=F, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F)))
  
  dev.off()
  
  png(paste('Figures/MultiMapPanelForPub/CH4Conc/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
  print(spplot(subsetKlist[[daynumber]], zcol='CH4Conc', at=CH4breaks2,  colorkey=F, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F)))
  dev.off()
  
  png(paste('Figures/MultiMapPanelForPub/CO2Flux/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
  print(spplot(subsetKlist[[daynumber]], zcol='CO2Flux', at=CO2Fluxbreaks2, colorkey=F, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F)))
  dev.off()
  
  png(paste('Figures/MultiMapPanelForPub/CH4Flux/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
  print(spplot(subsetKlist[[daynumber]], zcol='CH4Flux', at=CH4Fluxbreaks2, colorkey=F, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F)))
  dev.off()
  
}


PixelSummary<-subsetKlist[[daynumber]]
PixelSummary$CH4Conc<-colMeans(ConcArray[,,c('CH4uM_t')])
PixelSummary$CO2Conc<-colMeans(ConcArray[,,c('CO2uM_t')])
PixelSummary$CH4Flux<-colMeans(Fluxmatrix[,,'CH4'])
PixelSummary$CO2Flux<-colMeans(Fluxmatrix[,,'CO2'])
PixelSummary$k600<-colMeans(Kmatrix[,], na.rm=T)

#Save Average pixel images
cuts<-40

png(paste('Figures/K6002016Average.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
print(spplot(PixelSummary, zcol='k600', cuts=cuts, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), xlab=expression(paste(k[600], " (cm hr"^"-1", ")")), main='2016 average k'))
dev.off()

png(paste('Figures/CH4Conc2016Average.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
print(spplot(PixelSummary, zcol='CH4Conc', cuts=cuts, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(CH[4], " (", mu, "M)")), xlab='2016 Average'))
dev.off()

png(paste('Figures/CO2Conc2016Average.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
print(spplot(PixelSummary, zcol='CO2Conc', cuts=cuts, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(CO[2], " (", mu, "M)")), xlab='2016 Average'))
dev.off()

png(paste('Figures/CO2Flux2016Average.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
print(spplot(PixelSummary, zcol='CO2Flux', cuts=cuts, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(CO[2], " efflux (mmol/m2/d)  ")), xlab='2016 Average'))
dev.off()

png(paste('Figures/CH4Flux2016Average.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
print(spplot(PixelSummary, zcol='CH4Flux', cuts=cuts, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(CH[4], " efflux (mmol/m2/d)  ")), xlab='2016 Average'))
dev.off()

png(paste('Figures/Historgram_2016Average.png', sep=""), width=3, height=10, units='in', res=200, bg='white')
par(pch=16)
par(ps=12)
par(mfrow=c(5,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)

panel<-5
for (panel in c(5,6,8,7,9)){
  vector<-PixelSummary@data[,panel]
  name<-names(PixelSummary@data[panel])
  range<-range(vector, na.rm=T)
  diff<-range[2]-range[1]
  
  breaks<-seq(range[1], range[2], length.out=cuts)
 
  hist(vector, breaks=breaks, col=colors, main='', yaxs="i", xlab="", las=1, ylab="")
  mtext('Frequency', 2,-1.5, outer=T )
  mtext(name, 1,2)
  abline(h=0)

}
dev.off()

#Aggregate across space

CO2_Conc_Mean<-rowMeans(ConcArray[,,c('CO2uM_t')])
CH4_Conc_Mean<-rowMeans(ConcArray[,,c('CH4uM_t')])
CH4_Flux_Mean<-rowMeans(Fluxmatrix[,,'CH4'])
CO2_Flux_Mean<-rowMeans(Fluxmatrix[,,'CO2'])

DailyCO2quantiles<-as.data.frame(t(apply(ConcArray[,,c('CO2uM_t')], 1, quantile, probs = c(0.05, .1, .25, .5, .75, .95, 0.9),  na.rm = TRUE)))
names(DailyCO2quantiles)<-paste('CO2_Conc_', names(DailyCO2quantiles), sep="")

DailyCH4quantiles<-as.data.frame(t(apply(ConcArray[,,c('CH4uM_t')], 1, quantile, probs = c(0.05, .1, .25, .5, .75, .95, 0.9),  na.rm = TRUE)))
names(DailyCH4quantiles)<-paste('CH4_Conc_', names(DailyCH4quantiles), sep="")


DailyCO2Fluxquantiles<-as.data.frame(t(apply(Fluxmatrix[,,'CO2'], 1, quantile, probs = c(0.05, .1, .25, .5, .75, .95, 0.9),  na.rm = TRUE)))
names(DailyCO2Fluxquantiles)<-paste('CO2_Flux_', names(DailyCO2Fluxquantiles), sep="")


DailyCH4Fluxquantiles<-as.data.frame(t(apply(Fluxmatrix[,,'CH4'], 1, quantile, probs = c(0.05, .1, .25, .5, .75, .95, 0.9),  na.rm = TRUE)))
names(DailyCH4Fluxquantiles)<-paste('CH4_Flux_', names(DailyCH4Fluxquantiles), sep="")



DailyStats<-data.frame(date_names, CO2_Conc_Mean, CH4_Conc_Mean, CO2_Flux_Mean, CH4_Flux_Mean, DailyCO2quantiles, DailyCH4quantiles, DailyCO2Fluxquantiles, DailyCH4Fluxquantiles)

# Save daily summary table
saveRDS(DailyStats , file='Data/DailyConcFluxStats.rds')
write.table(DailyStats , file='Data/DailyConcFluxStats.csv')


# Generate 'Buoy' Flux samples
# pixel number's are buoy...
BuoyPixels<-c(358:360, 404:406, 450:452)

CO2Buoy_Conc_Mean<-rowMeans(ConcArray[,BuoyPixels,c('CO2uM_t')])
CH4Buoy_Conc_Mean<-rowMeans(ConcArray[,BuoyPixels,c('CH4uM_t')])
CH4Buoy_Flux_Mean<-rowMeans(Fluxmatrix[,BuoyPixels,'CH4'])
CO2Buoy_Flux_Mean<-rowMeans(Fluxmatrix[,BuoyPixels,'CO2'])

BuoyDailyStats<-data.frame(date_names, CO2Buoy_Conc_Mean, CH4Buoy_Conc_Mean, CO2Buoy_Flux_Mean, CH4Buoy_Flux_Mean)
saveRDS(BuoyDailyStats , file='Data/DailyBuoyConcFluxStats.rds')
write.table(BuoyDailyStats , file='Data/DailyBuoyConcFluxStats.csv')


hist(Fluxmatrix[,,'CO2'], breaks=40)
hist(Fluxmatrix[,,'CH4'], breaks=40)
hist(Atmmatrix[,,'CO2'], breaks=40)
hist(Atmmatrix[,,'CH4'], breaks=40)

summary(Fluxmatrix[,,'CO2'])
mean((Fluxmatrix[,,'CO2']))
median((Fluxmatrix[,,'CO2']))

summary(Fluxmatrix[,,'CH4'])
mean((Fluxmatrix[,,'CH4']))
median((Fluxmatrix[,,'CH4']))


# Save Flux matrix and summaries

saveRDS(PixelSummary , file='Data/PixelSummaryFlux.rds')
saveRDS(Fluxmatrix , file='Data/DailyFluxperPixel.rds')

#Save Flux and concentration maps per day as individual shapefiles

K<-1
for (K in 1:length(subsetKlist)){
  dayKlist<-subsetKlist[[K]]
  name<-date_names[K]
  writeOGR(dayKlist, dsn='Data/Shapefiles', layer=paste(name, '_C_Conc_Flux', sep=""), driver="ESRI Shapefile")
}


k600_dataframe$Mean
k600_dataframe$K600.VachonLA


