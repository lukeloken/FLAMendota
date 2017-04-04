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

bearing=210
fetch_vector<-rep(NA, nrow(Mendota_surface_UTM))
for (point in 1:nrow(Mendota_surface_UTM)){

fetch_vector[point]<-fetch_len(Mendota_surface_UTM[point,], bearings=bearing, shoreline= negativeLake, projected=T, dmax=20000)

}


Mendota_surface_UTM$fetch<-fetch_vector
spplot(Mendota_surface_UTM, zcol='fetch', cuts=99, colorkey=TRUE, sp.layout = list(Mendota_Base_UTM), cex=1.6, pch=15)

  