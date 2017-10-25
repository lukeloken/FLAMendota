
library(seacarb)

dt1<-readRDS(file='Data/Hard_dt1.rds') #gases
dt2<-readRDS(file='Data/Hard_dt2.rds') #sonde casts
dt3<-readRDS(file='Data/Hard_dt3.rds') #ebullition
dt4<-readRDS(file='Data/Hard_dt4.rds') #inlets

# dt2$sampledate<-as.Date(dt2$sampledate)

summary(dt1)
summary(dt2)
# summary(dt3)
# summary(dt4)

dt2$sampletime<-as.POSIXct(paste(dt2$sampledate, dt2$sampletime, sep=' '), format='%Y-%m-%d %H:%M:%S', tz='America/Chicago')

bottomgas<-dt1[dt1$water_depth>=15 & dt1$sample_site=='Deep Hole',]

surfacegas<-dt1[dt1$water_depth<5 & dt1$water_depth>1 & dt1$sample_site=='Deep Hole',]
surfacegasall<-dt1[dt1$water_depth<5 & dt1$water_depth>1,]
surfacegasmean<-aggregate(surfacegas, by=list(surfacegas$sampledate), FUN=mean)[-1]


surfacesonde<-dt2[dt2$water_depth<5,]

head(surfacegas)
head(surfacesonde)

surfacesondemean<-aggregate(surfacesonde, by=list(surfacesonde$sampledate), FUN=median)[2:ncol(surfacesonde)]

surfaceall<-merge(surfacegasmean, surfacesondemean, by='sampledate', all=T)

surfaceallsites<-merge(surfacegasall, surfacesondemean, by='sampledate', all=T)


plot(surfaceall$ph, surfaceall$co2)
plot(surfaceall$sampledate, surfaceall$ph)
plot(surfaceall$sampledate, surfaceall$co2)
plot(surfaceall$sampledate, surfaceall$ch4)


png('Figures/Hart_CO2TimeSeries.png', width=6, height=5, units='in', res=400, bg='white')
par(pch=16)
par(ps=12)
par(mfrow=c(2,1))
par(mar = c(3,5.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)

surfaceall$CO2guessDIC<-carb(flag=9, var1=surfaceall$ph, var2=surfaceall$dic/12.01/1000, S=surfaceall$salinity, T=surfaceall$temp, warn='n', k1k2="m06", Patm=0.93)$CO2*1000000

plot(surfaceall$sampledate, surfaceall$CO2guessDIC, xlab='', ylab='CO2 (uM)', ylim=range(c(surfaceall$CO2guessDIC, surfaceall$co2), na.rm=T), col='red', pch=16, type="o", xlim=range(surfaceall$sampledate))
points(surfaceall$sampledate, surfaceall$co2, col='black', pch=16, type="o")

legend('top', c('Observed', 'Calculated'), pch=16, lty=1, col=c('black', 'red'), bty="n")

plot(surfaceall$sampledate, surfaceall$ph, xlim=range(surfaceall$sampledate), type="o", ylab="pH", xlab="date", col="blue", pch=16, las=1)

par(new=T)
plot(surfaceall$sampledate, surfaceall$dic, xlim=range(surfaceall$sampledate), type="o", ylab="", xlab="date", col="green", pch=16, xaxt="n", yaxt="n")

axis(2, line=3, las=1)
mtext('DIC (mgC/L)', 2, 4.5)

legend('bottomleft', c('pH', 'DIC'), pch=16, lty=1, col=c('blue', 'green'), bty="n")

dev.off()


plot(surfaceall$co2, surfaceall$CO2guessDIC)
abline(0,1)

plot(surfacesonde$sampledate, surfacesonde$ph)


#add other four sites across Lake Mendota.
#Use YSI data at buoy and measured DIC to calculate theortetical CO2
surfaceallsites$CO2guessDIC<-carb(flag=9, var1=surfaceallsites$ph, var2=surfaceallsites$dic/12.01/1000, S=surfaceallsites$salinity, T=surfaceallsites$temp, warn='n', k1k2="m06", Patm=0.93)$CO2*1000000

png('Figures/Hart_CO2ObsVsCO2CalcpHDIC.png', width=5, height=5, units='in', res=400, bg='white')
par(pch=16)
par(ps=12)
par(mfrow=c(1,1))
par(mar = c(3,3,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)

HartRange<-range(c(surfaceallsites$co2, surfaceallsites$CO2guessDIC), na.rm=T)

plot(surfaceallsites$co2, surfaceallsites$CO2guessDIC, xlab="CO2 measured (uM)", ylab="CO2 calculated pH/DIC (uM)", xlim=HartRange, ylim=HartRange)
points(surfaceall$co2, surfaceall$CO2guessDIC, col="red")
abline(0,1)

dev.off()


plot(surfaceallsites$sampledate, surfaceallsites$co2)
points(surfaceallsites$sampledate, surfaceallsites$CO2guessDIC, col="red")

png('Figures/Hart_CO2TimeSeries_allsites.png', width=6, height=5, units='in', res=400, bg='white')
par(pch=16)
par(ps=12)
par(mfrow=c(2,1))
par(mar = c(3,5.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)


plot(surfaceallsites$sampledate, surfaceallsites$CO2guessDIC, xlab='', ylab='CO2 (uM)', ylim=range(c(surfaceallsites$CO2guessDIC, surfaceallsites$co2), na.rm=T), col='red', pch=16, type="n", xlim=range(surfaceallsites$sampledate))
points(surfaceallsites$sampledate, surfaceallsites$co2, col='black', pch=16, type="p", cex=1)
points(surfaceallsites$sampledate, surfaceallsites$CO2guessDIC, col='red', pch=16, type="p", cex=0.7)

legend('topleft', c('Observed', 'Calculated'), pch=16, lty=1, col=c('black', 'red'), bty="n")

plot(surfaceallsites$sampledate, surfaceallsites$ph, xlim=range(surfaceallsites$sampledate), type="p", ylab="pH", xlab="date", col="blue", pch=16, las=1)


par(new=T)
plot(surfaceallsites$sampledate, surfaceallsites$dic, xlim=range(surfaceallsites$sampledate), type="p", ylab="", xlab="date", col="green", pch=16, xaxt="n", yaxt="n")

axis(2, line=3, las=1)
mtext('DIC (mgC/L)', 2, 4.5)

legend('bottomleft', c('pH', 'DIC'), pch=16, lty=1, col=c('blue', 'green'), bty="n")

dev.off()



