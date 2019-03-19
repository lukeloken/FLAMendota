#Flux Footsprint creation

library(readxl)
library(sp)
library(geoR)
library(sf)
library(geosphere)
library(rgdal)
library(RODBC)
library(RgoogleMaps)
library(ggmap)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(ggpubr)

df1<-read_excel('Data/PicnicPointfootprint.xlsx')
PP<-data.frame(y=43.0896, x=(-89.415827))

coordinates(PP) <- ~x+y
proj4string(PP)<- CRS("+init=epsg:4326")

head(df1)

boundary<-data.frame(lon=rep(NA, nrow(df1)), lat=rep(NA, nrow(df1)))
boundaryMax<-boundary
boundary20<-boundary
boundary40<-boundary
boundary60<-boundary
boundary80<-boundary

for (row in 1:nrow(df1)){
  boundaryMax[row,]<-destPoint(PP, b=df1[row,1], d=df1[row,2])
  boundary20[row,]<-destPoint(PP, b=df1[row,1], d=df1[row,3])
  boundary40[row,]<-destPoint(PP, b=df1[row,1], d=df1[row,4])
  boundary60[row,]<-destPoint(PP, b=df1[row,1], d=df1[row,5])
  boundary80[row,]<-destPoint(PP, b=df1[row,1], d=df1[row,6])
  
}
coordinates(boundaryMax) <- ~lon+lat
proj4string(boundaryMax)<- CRS("+init=epsg:4326")

coordinates(boundary20) <- ~lon+lat
proj4string(boundary20)<- CRS("+init=epsg:4326")

coordinates(boundary40) <- ~lon+lat
proj4string(boundary40)<- CRS("+init=epsg:4326")

coordinates(boundary60) <- ~lon+lat
proj4string(boundary60)<- CRS("+init=epsg:4326")

coordinates(boundary80) <- ~lon+lat
proj4string(boundary80)<- CRS("+init=epsg:4326")


PointsToPoly<-function(point_df){
p = Polygon(point_df)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
return(sps)
}

Poly80<-PointsToPoly(boundary80)
Poly60<-PointsToPoly(boundary60)
Poly40<-PointsToPoly(boundary40)
Poly20<-PointsToPoly(boundary20)
PolyMax<-PointsToPoly(boundaryMax)


# plot(Poly80, col='grey90', border='black')
# plot(Poly60, add=T, col='grey70', border='black')
# plot(Poly40, add=T, col='grey50', border='black')
# plot(Poly20, add=T, col='grey30', border='black')
# plot(PolyMax, add=T, col='grey10', border='black')
# plot(PP, add=T, col="cyan", pch=16, cex=1)


#GoogleKey
GoogleAPIkey<-unlist(read.delim("C:/Users/lcloken/Documents/Google/LokenAPIKey2.txt", stringsAsFactor=F, check.names = FALSE, header=F))


#Download ggmap
gmap_Mendota<-get_googlemap(center=c(-89.41583, 43.0896), size=c(640, 640), zoom = 15, maptype = "satellite", key=GoogleAPIkey )

colors<-adjustcolor(brewer.pal(5, 'RdPu'), alpha=0.3)

png("Figures/PicnicPointFootprint.png", res=600, width=6,height=6, units="in")

ggmap(gmap_Mendota) + 
  labs(x=expression(paste('Longitude (', degree, ')', sep='')) , y=expression(paste('Latitude (', degree, ')', sep=''))) + 
  geom_polygon(aes(x=long, y=lat), data=Poly80, fill=colors[1], col='black') + 
  geom_polygon(aes(x=long, y=lat), data=Poly60, fill=colors[2], col='black') + 
  geom_polygon(aes(x=long, y=lat), data=Poly40, fill=colors[3], col='black') + 
  geom_polygon(aes(x=long, y=lat), data=Poly20, fill=colors[4], col='black') + 
  geom_polygon(aes(x=long, y=lat), data=PolyMax, fill=colors[5], col='black') + 
  geom_point(aes(x=x, y=y), data=data.frame(PP), col='cyan', pch=8)

dev.off()
