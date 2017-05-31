# ################################################################
# Code to Merge Concentration and K data to generate flux estimates
# ################################################################

#load atmosphere data
Atm<-readRDS('Data/FlameBuoyAtmMeasurements.rds')
atmCO2<-median(Atm$XCO2Dppm)
atmCH4<-median(Atm$XCH4Dppm)

# Load concentration array
ConcArray<-readRDS(file='Data/DailyFlamebyPixel.rds')
dailydates<-dimnames(ConcArray)[[1]]

# Load pixelIDs
coordinates<-readRDS(file='Data/PixelIDs.rds')


# Load K list of maps
Klist<-readRDS(file='Data/DailyFetchK600maps.rds')
str(Klist[[1]])

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
  
  Klist[[daynumber]]$CO2Conc<-DailyCO2uM
  Klist[[daynumber]]$CH4Conc<-DailyCH4uM
  
  Klist[[daynumber]]$CO2Flux<-Fluxmatrix[time,,'CO2']
  Klist[[daynumber]]$CH4Flux<-Fluxmatrix[time,,'CH4']
  
  
  #Concentration maps
  png(paste('Figures/DailyCO2ConcMap2016/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
  print(spplot(Klist[[daynumber]], zcol='CO2Conc', cuts=20,  colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(CO[2], " (", mu, "M)")), xlab=paste(date_name)))
  dev.off()
  
  png(paste('Figures/DailyCH4ConcMap2016/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
  print(spplot(Klist[[daynumber]], zcol='CH4Conc', cuts=20,  colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(CH[4], " (", mu, "M)")), xlab=paste(date_name)))
  dev.off()
  
  
  png(paste('Figures/DailyCO2FluxMap2016/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
  print(spplot(Klist[[daynumber]], zcol='CO2Flux', cuts=20,  colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(CO[2], " efflux (mmol/m2/d)  ")), xlab=paste(date_name)))
  dev.off()
  
  png(paste('Figures/DailyCH4FluxMap2016/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
  print(spplot(Klist[[daynumber]], zcol='CH4Flux', cuts=20,  colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(CH[4], " efflux (mmol/m2/d)  ")), xlab=paste(date_name)))
  dev.off()
  
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



time=dailydates[1]
for (time in dailydates){
  daynumber<-which(time==dailydates)
  date_name<-date_names[daynumber]
  
  png(paste('Figures/DailyCO2Movie2016/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
  print(spplot(Klist[[daynumber]], zcol='CO2Conc', at=CO2breaks, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(CO[2], " (", mu, "M)")), xlab=paste(date_name)))
  dev.off()
  
  png(paste('Figures/DailyCH4Movie2016/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
  print(spplot(Klist[[daynumber]], zcol='CH4Conc', at=CH4breaks,  colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(CH[4], " (", mu, "M)")), xlab=paste(date_name)))
  dev.off()

  png(paste('Figures/DailyCO2FluxMovie2016/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
  print(spplot(Klist[[daynumber]], zcol='CO2Flux', at=CO2Fluxbreaks, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(CO[2], " efflux (mmol/m2/d)  ")), xlab=paste(date_name)))
  dev.off()
  
  png(paste('Figures/DailyCH4FluxMovie2016/', date_name, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
  print(spplot(Klist[[daynumber]], zcol='CH4Flux', at=CH4Fluxbreaks, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(CH[4], " efflux (mmol/m2/d)  ")), xlab=paste(date_name)))
  dev.off()

}

PixelSummary<-Klist[[daynumber]]
PixelSummary$CH4Conc<-colMeans(ConcArray[,,c('CH4uM_t')])
PixelSummary$CO2Conc<-colMeans(ConcArray[,,c('CO2uM_t')])
PixelSummary$CH4Flux<-colMeans(Fluxmatrix[,,'CH4'])
PixelSummary$CO2Flux<-colMeans(Fluxmatrix[,,'CO2'])

png(paste('Figures/CH4Conc2016Average.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
print(spplot(PixelSummary, zcol='CH4Conc', cuts=40, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(CH[4], " (", mu, "M)")), xlab='2016 Average'))
dev.off()

png(paste('Figures/CO2Conc2016Average.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
print(spplot(PixelSummary, zcol='CO2Conc', cuts=40, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(CO[2], " (", mu, "M)")), xlab='2016 Average'))
dev.off()

png(paste('Figures/CO2Flux2016Average.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
print(spplot(PixelSummary, zcol='CO2Flux', cuts=40, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(CO[2], " efflux (mmol/m2/d)  ")), xlab='2016 Average'))
dev.off()

png(paste('Figures/CH4Flux2016Average.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
print(spplot(PixelSummary, zcol='CH4Flux', cuts=40, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(CH[4], " efflux (mmol/m2/d)  ")), xlab='2016 Average'))
dev.off()



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

