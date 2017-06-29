
# Plot Daily Wind

library(lubridate)
library(caTools)
library(circular)
source('R/AddAlpha.R')
source('R/CalculateSpatialFetch.R')
source('R/CalculateSpatialk600.R')


dailywind<-readRDS('Data/dailywind.rds')

k=4
k2<-14

dailywind$runningdirection<-runmean(circular(dailywind$dir, units = "degrees"), k=k, endrule='NA')
dailywind$runningdirection2<-runmean(circular(dailywind$dir, units = "degrees"), k=k2, endrule='NA')
wind_dir_annualmean<-mean(circular(dailywind$dir, units = "degrees"), na.rm=T)
if (wind_dir_annualmean<0){wind_dir_annualmean=360+wind_dir_annualmean}
wind_speed_annualmean<-mean(dailywind$speed,na.rm=T)


# Set UTM projection (Zone 15 for Wisconsin)
projection <- "+proj=utm +zone=15 ellps=WGS84"

# Outline of Lake Mendota
basemap_dir<-"E:/Dropbox/FLAME/basemaps"
Mendota_Shoreline<-readOGR(basemap_dir, "Mendota_shape")
# Project into UTM's. This way distance is in meters (m)
shoreline <- spTransform(Mendota_Shoreline, CRS(projection))

# get the 200m resolution Lake Mendota grid
grid_dir<-"E:/Dropbox/FLAME/basemaps/shapefiles"
Mendota_grid<-readOGR(grid_dir, "MendotaPredictGrid2016")
Mendota_grid@proj4string <- CRS(projection)
gridded(Mendota_grid)<-T


# Calculate fetch distance (m and km)
Mendota_grid$fetch<-SpatialFetch(Mendota_grid, wind_dir=as.numeric(wind_dir_annualmean), shoreline=shoreline, projected=T, dmax=10000)
Mendota_grid$fetch_km<-Mendota_grid$fetch/1000

#Calculate k600 (cm/hr)
Mendota_grid$k600<-Spatialk600(Mendota_grid, wind_speed=wind_speed_annualmean, wind_height=2.2, fetch_name='fetch')
gridded(Mendota_grid)<-T
# Plot daily fetch and k600

png(paste('Figures/FetchAnnualMean2016.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
print(spplot(Mendota_grid, zcol='fetch_km', cuts=99, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F),  main=expression(paste("Fetch distance (km)"))))
dev.off()

png(paste('Figures/K600AnnualMean2016.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
print(spplot(Mendota_grid, zcol='k600', cuts=20,  colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(k[600], " (cm/hr)")) ))
dev.off()


png('Figures/DailyWind2016.png', width=4.5, height=4, units='in', res=200, bg='white')
par(pch=16)
par(ps=8)
par(mfrow=c(2,1))
par(mar = c(.5,2.5,0.5,0.5),mgp=c(2.5,0.4,0),tck=-0.02)
par(oma=c(1,0,0,0))
par(lend=2)

xticks<-seq(ceiling_date(min(dailywind$sampledate), "months"),floor_date(max(dailywind$sampledate), "months"), by='months')
xlabels<-paste(month(xticks, label=TRUE, abbr=T), " 1", sep="")

color_raw<-c('black', 'red', 'blue')
colors<-add.alpha(col=color_raw, alpha=.6)


plot(dailywind$sampledate, dailywind$speed, type="o", xlab="", ylab="", xaxt="n", las=1, cex=.3, lwd=.5, col=colors[1])
lines(dailywind$sampledate, runmean(dailywind$speed, k=k, endrule='NA'), col=colors[2], lwd=2)
lines(dailywind$sampledate, runmean(dailywind$speed, k=k2, endrule='NA'), col=colors[3], lwd=2)
axis(1, at=xticks, labels=NA, mgp=c(2.5,0.2,0))
mtext(expression(paste('Wind speed (m s'^'-1', ')')), 2,1.5)

legend('top', inset=0.00, c('1 day mean', '9 day mean', '29 day mean'), bty="n", text.col=color_raw, y.intersp=.7)

plot(dailywind$sampledate, dailywind$dir, type="o", xlab="", ylab="", xaxt="n", yaxt="n", cex=.3, lwd=.5, col=colors[1], ylim=c(-5,365))
lines(dailywind$sampledate, dailywind$runningdirection, col=colors[2], lwd=2)
lines(dailywind$sampledate, dailywind$runningdirection2, col=colors[3], lwd=2)
axis(1, at=xticks, labels=xlabels)
axis(2, at=seq(0,360,90), las=1)
axis(2, at=seq(0,360,90), las=1, labels=c('N', 'E', 'S', 'W', 'N'), line=-0.5, tick=F, hadj=0 )
mtext(expression(paste('Wind direction (', degree, ')')), 2,1.5)

dev.off()
