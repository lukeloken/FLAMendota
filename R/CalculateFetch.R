# ###########################################################################
# Code to calculate k at each pixel for each pixel of Lake Mendota
# model from Vachon and Praire 2013
# k600 = 2.13 (±1.06) + 2.18 (±0.30) · U10 + 0.82 (±0.24) · U10 · log10 fetch
# u10 == wind speed at m height (m s-1)
# fetch == length (km)
# ###########################################################################

# library(geosphere)
library(rgdal)
# library(fetchR)
library(rgeos)
# library(lakemorpho)
library(waver)
library(maps)

# Get outline of Lake Mendota
basemap_dir<-"E:/Dropbox/FLAME/basemaps"
Mendota_Base<-readOGR(basemap_dir, "Mendota_shape")

# Set UTM projection (Zone 15 for Wisconsin)
projection <- "+proj=utm +zone=15 ellps=WGS84"
# Transform lakebase data into UTM's. This way distance is in meters (m)
Mendota_Base_UTM <- spTransform(Mendota_Base, CRS(projection))


# Get outline of Wisconsin
state_dir<-"E:/Dropbox/ArcGis"
State<-readOGR(state_dir, 'cb_2014_us_state_20m')
WI<-State[State$NAME=='Wisconsin',]
# Transform WI into UTM's. This way distance is in meters (m)
WI_UTM <- spTransform(WI, CRS(projection))


# get a sample idw surface
sample_dir<-"E:/Dropbox/FLAME_YaharaLakes/Data/2016-10-25_LakeMendota/shapefiles_idw"
Mendota_surface<-readOGR(sample_dir, "LakeMendota2016-10-25cleaned")

# Transform surface data into UTM's. This way distance is in meters (m)
Mendota_surface_UTM <- spTransform(Mendota_surface, CRS(projection))

#plot to visualize data
par(mfrow=c(1,1))
plot(Mendota_Base_UTM, axes=T)
plot(Mendota_surface_UTM, add=T, cex=0.5, col="deeppink2", pch=4, lwd=2)



negativeLake<-gDifference(WI_UTM, Mendota_Base_UTM)
plot(negativeLake, add=T, col='green')

testpoint<-Mendota_surface_UTM[1143,]
plot(testpoint, col='blue', pch=15, add=T)
fetch_len(testpoint, bearings=0, shoreline= negativeLake, projected=T, dmax=5000)
fetch_len(testpoint, bearings=90, shoreline= negativeLake, projected=T, dmax=5000)
fetch_len(testpoint, bearings=180, shoreline= negativeLake, projected=T, dmax=5000)
fetch_len(testpoint, bearings=270, shoreline= negativeLake, projected=T, dmax=5000)



# Loop through all points and calculate fetch
# need to specify bearing
# bearing = 210 is the most common for Lake Mendota

bearing=210 #degs
speed=4.118 #m per s

#Convert wind speed to U10 using Crusius and Wanningkhof 2003
U10<- speed*(1+ 0.0013^0.5*log(10/2)/.41)

# Create vectors to fill
fetch_vector<-rep(NA, nrow(Mendota_surface_UTM))

for (point in 1:nrow(Mendota_surface_UTM)){

fetch_vector[point]<-fetch_len(Mendota_surface_UTM[point,], bearings=bearing, shoreline= negativeLake, projected=T, dmax=20000)

}

Mendota_surface_UTM$fetch<-fetch_vector
Mendota_surface_UTM$k600<- 2.13  + 2.18 *U10 + 0.82*U10*log10(Mendota_surface_UTM$fetch/1000)
Mendota_surface_UTM$k600_max<- 2.13+1.06  + (2.18+.3) *U10 + (0.82+.24)*U10*log10(Mendota_surface_UTM$fetch/1000)
Mendota_surface_UTM$k600_min<- 2.13-1.06  + (2.18-.3) *U10 + (0.82-.24)*U10*log10(Mendota_surface_UTM$fetch/1000)
Mendota_surface_UTM$k600LA<- 2.51 + 1.48*U10 + 0.39*U10*log10(39.85)
#Replace negaative k with zero
Mendota_surface_UTM$k600[which(Mendota_surface_UTM$k600<0)]<-0
Mendota_surface_UTM$k600_min[which(Mendota_surface_UTM$k600_min<0)]<-0
Mendota_surface_UTM$k600_max[which(Mendota_surface_UTM$k600_max<0)]<-0


col1<-add.alpha('azure2', alpha=1)
col2<-add.alpha('deeppink', alpha=1)
colfunc<-colorRampPalette(c(col1, col2))

# spplot(Mendota_surface_UTM, zcol='fetch', cuts=100, colorkey=TRUE, sp.layout = list(Mendota_Base_UTM), cex=1.6, pch=15)

png('Figures/FetchBearing210.png', width=6, height=6, units='in', res=200, bg='white')

par(mfrow=c(1,1))
par(mar=c(4,4,4,4), oma=c(1,1,1,1))

print(spplot(Mendota_surface_UTM, zcol='fetch', cuts=100, col.regions=colfunc(100), colorkey=TRUE, sp.layout=list(Mendota_Base_UTM, col=1, fill=0, lwd=3, lty=1, first=F), cex=1.6, pch=15, main="Fetch distance (m)"))

dev.off()
closeAllConnections()


# col1<-add.alpha('azure2', alpha=1)
# col2<-add.alpha('cyan4', alpha=1)
# colfunc<-colorRampPalette(c(col1, col2))


colfunc<-colorRampPalette(c('black','darkblue', 'skyblue4', 'cadetblue4', 'lavenderblush3', 'gold', 'orangered', 'deeppink3', 'magenta4'))

png('Figures/k600Bearing210.png', width=6, height=6, units='in', res=200, bg='white')

par(mfrow=c(1,1))
par(mar=c(4,4,4,4), oma=c(1,1,1,1))

print(spplot(Mendota_surface_UTM, zcol='k600', cuts=100, col.regions=colfunc(100), colorkey=TRUE, sp.layout=list(Mendota_Base_UTM, col=1, fill=0, lwd=3, lty=1, first=F), cex=1.6, pch=15, main=expression(paste(k[600]))))

dev.off()
closeAllConnections()

png('Figures/k600MinBearing210.png', width=6, height=6, units='in', res=200, bg='white')

par(mfrow=c(1,1))
par(mar=c(4,4,4,4), oma=c(1,1,1,1))

print(spplot(Mendota_surface_UTM, zcol='k600_min', cuts=100, col.regions=colfunc(100), colorkey=TRUE, sp.layout=list(Mendota_Base_UTM, col=1, fill=0, lwd=3, lty=1, first=F), cex=1.6, pch=15, main=expression(paste('Min ', k[600]))))

dev.off()
closeAllConnections()

png('Figures/k600MaxBearing210.png', width=6, height=6, units='in', res=200, bg='white')

par(mfrow=c(1,1))
par(mar=c(4,4,4,4), oma=c(1,1,1,1))

print(spplot(Mendota_surface_UTM, zcol='k600_max', cuts=100, col.regions=colfunc(100), colorkey=TRUE, sp.layout=list(Mendota_Base_UTM, col=1, fill=0, lwd=3, lty=1, first=F), cex=1.6, pch=15, main=expression(paste('Max ', k[600]))))

dev.off()
closeAllConnections()



png('Figures/HistogramKBearing210.png', width=3, height=3, units='in', res=200, bg='white')
par(pch=16)
par(ps=12)
par(mfrow=c(1,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)


hist(Mendota_surface_UTM$k600[Mendota_surface_UTM$k600>=0], main="", xlab=expression(paste(k[600], " (cm hr"^"-1", ")")), col=colfunc(16), breaks=seq(0,16,1))
box(which='plot')

dev.off()

png('Figures/HistogramKBearing210_minmax.png', width=3, height=8, units='in', res=200, bg='white')
par(pch=16)
par(ps=12)
par(mfrow=c(3,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)

hist(Mendota_surface_UTM$k600_min[Mendota_surface_UTM$k600_min>=0], main="", xlab=expression(paste(k[600], " (cm hr"^"-1", ")")), col=colfunc(20), breaks=seq(0,20,1))
box(which='plot')
legend('topleft', 'Min', bty="n")

hist(Mendota_surface_UTM$k600[Mendota_surface_UTM$k600>=0], main="", xlab=expression(paste(k[600], " (cm hr"^"-1", ")")), col=colfunc(20), breaks=seq(0,20,1))
box(which='plot')
legend('topleft', 'Middle', bty="n")

hist(Mendota_surface_UTM$k600_max[Mendota_surface_UTM$k600_max>=0], main="", xlab=expression(paste(k[600], " (cm hr"^"-1", ")")), col=colfunc(20), breaks=seq(0,20,1))
box(which='plot')
legend('topleft', 'Max', bty="n")

dev.off()


  