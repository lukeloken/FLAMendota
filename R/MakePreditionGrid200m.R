



rm(list = ls())

library(spatstat)
library(gstat)
library(sp)
library(rgdal)
library(geoR)
library(raster)
library(rgeos)

# Set working directory to basemap location
setwd("E:/Dropbox/FLAME/basemaps")
lakes_Base_UTM<-readOGR(getwd(), "Mendota_shape")


# Set UTM projection (Zone 15 for Northern Wisconsin Regional Lakes)
projection <- "+proj=utm +zone=15 ellps=WGS84"

# Transform lakebase data into UTM's. This way distance is in meters (m)
lake_polygon <- spTransform(lakes_Base_UTM, CRS(projection))

# Set lake polygon
plot(lake_polygon)
brdydim<-lake_polygon@bbox

pixelsize<-200


# Generate 'data cloud' based on observations
# bdry<-ripras(coordinates(data))

# Convert data cloud to spatial polygon
# bdry_df<-data.frame(bdry[[4]][[1]]$x, bdry[[4]][[1]]$y)
# bdry_df[nrow(bdry_df)+1,]<-bdry_df[1,]
# bdry_poly<-Polygon(bdry_df)
# bdry_poly2 = Polygons(list(bdry_poly), "s1")
# bdry_poly_sp<-SpatialPolygons(list(bdry_poly2), proj4string=CRS(as.character(projection)))

# Make Buffer around data cloud polygon 
# width = distance in meters; currently using two pixel distances
# buffered<-gBuffer(bdry_poly_sp, width=400)
# buffered<-gBuffer(bdry_poly_sp, width=pixelsize*1)

# Make prediction area as intersection of buffered area and lake polygon
# Area<-gIntersection(buffered, lake_polygon)
#     
#     # Check Area and Confirm
#     plot(Area)
#     plot(data, add=TRUE)
#     plot(bdry_poly_sp, add=TRUE)
#     plot(buffered, add=TRUE, lwd=4)
#     plot(lakes_Base, add=TRUE)

#Code above can subset the polygon

# Make polygrid - This is each location to make predictions
data1.grid<-polygrid(seq(brdydim[1,1], brdydim[1,2], pixelsize), seq(brdydim[2,1], brdydim[2,2], pixelsize), borders=lake_polygon@polygons[[1]]@Polygons[[1]]@coords)
# Set names of coordinates and match projection
coordinates(data1.grid)<-~x+y
proj4string(data1.grid) <- CRS(projection)
# Convert to gridded (pixels) rather than points
#     gridded(data1.grid) <- TRUE
#     
# Remove pixels from islands and other polygon 'holes'
# pts_in=over(SpatialPoints(data1.grid), SpatialPolygons(lake_polygon@polygons), fn=NULL)
# data2.grid<-data1.grid[!is.na(pts_in)]
data2.grid<-data1.grid[-c(991,990,973)] #Manually remove 3 points from Lake Mendota grid
plot(data2.grid, add=T, col="blue")


gridded(data2.grid) <- TRUE

# plot data grid, boundary, and observations
plot(data2.grid, col="grey", pch=16)
plot(lake_polygon, add=TRUE, col=NA, lwd=3)

saveRDS(data2.grid, file='MendotaPredictGrid2016.rds')

data2.grid@coords
data2.spdf<-SpatialPointsDataFrame(coords=data2.grid@coords, data=as.data.frame(data2.grid@coords))

writeOGR(data2.spdf, dsn=paste(getwd(), "/shapefiles", sep="") ,layer='MendotaPredictGrid2016', driver="ESRI Shapefile",  verbose=T, overwrite=T)





# Make spatial object to save surface predictions
grid_withData<-SpatialPixelsDataFrame(data2.grid, data=data.frame(matrix(ncol=length(variables), nrow=length(data2.grid))))
names(grid_withData@data)<-variables


