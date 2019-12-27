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
library(ggsn)

# devtools::install_github('oswaldosantos/ggsn')
library(ggsn)


#Footprint outputs from Matlab
df1<-read_excel('Data/PicnicPointfootprint.xlsx')
df2<-read_excel('Data/PicnicPointfootprint_5deg.xlsx')

#Picnic Point location according to David Reed
PP<-data.frame(y=43.0896, x=(-89.415827))
#Picnic Point location according to Google Earth
# PP<-data.frame(y=43.089705, x=(-89.415740)) 


coordinates(PP) <- ~x+y
proj4string(PP)<- CRS("+init=epsg:4326")

#Mendota Shoreline
basemap_dir<-"C:/Dropbox/FLAME/basemaps"
Mendota_Shoreline<-readOGR(basemap_dir, "Mendota_shape")


head(df1)

boundary<-data.frame(lon=rep(NA, nrow(df1)), lat=rep(NA, nrow(df1)))
boundaryMax<-boundary
boundary20<-boundary
boundary40<-boundary
boundary60<-boundary
boundary80<-boundary

for (row in 1:nrow(df1)){
  boundaryMax[row,1:2]<-destPoint(PP, b=df1[row,1], d=df1[row,2])
  boundary20[row,1:2]<-destPoint(PP, b=df1[row,1], d=df1[row,3])
  boundary40[row,1:2]<-destPoint(PP, b=df1[row,1], d=df1[row,4])
  boundary60[row,1:2]<-destPoint(PP, b=df1[row,1], d=df1[row,5])
  boundary80[row,1:2]<-destPoint(PP, b=df1[row,1], d=df1[row,6])
  
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

# poly<-Poly80
# hole<-Poly60
AddHoleToPolygon <-function(poly,hole){
  # invert the coordinates for Polygons to flag it as a hole
  coordsHole <-  hole@polygons[[1]]@Polygons[[1]]@coords
  newHole <- Polygon(coordsHole,hole=TRUE)
  
  # punch the hole in the main poly
  listPol <- poly@polygons[[1]]@Polygons
  listPol[[length(listPol)+1]] <- newHole
  punch <- Polygons(listPol,poly@polygons[[1]]@ID)
  
  # make the polygon a SpatialPolygonsDataFrame as the entry
  new <- SpatialPolygons(list(punch),proj4string=poly@proj4string)
  
  if (class(poly)=='SpatialPolygonsDataFrame'){
  new <- SpatialPolygonsDataFrame(new,data=as(poly,"data.frame"))
  }
  return(new)
}

Poly80<-PointsToPoly(boundary80)
Poly60<-PointsToPoly(boundary60)
Poly40<-PointsToPoly(boundary40)
Poly20<-PointsToPoly(boundary20)
PolyMax<-PointsToPoly(boundaryMax)

Poly80_h<-AddHoleToPolygon(Poly80, Poly60)
Poly60_h<-AddHoleToPolygon(Poly60, Poly40)
Poly40_h<-AddHoleToPolygon(Poly40, Poly20)
Poly20_h<-AddHoleToPolygon(Poly20, PolyMax)


plot(Poly80, col='grey90', border='black')
plot(Poly60, add=T, col='grey70', border='black')
plot(Poly40, add=T, col='grey50', border='black')
plot(Poly20, add=T, col='grey30', border='black')
plot(PolyMax, add=T, col='grey10', border='black')
plot(PP, add=T, col="cyan", pch=16, cex=1)


#GoogleKey
GoogleAPIkey<-unlist(read.delim("C:/Users/lcloken/Documents/Google/LokenAPIKey2.txt", stringsAsFactor=F, check.names = FALSE, header=F))

register_google(key = as.character(GoogleAPIkey))

#Download ggmap
# gmap_Mendota<-get_googlemap(center=c(-89.41583, 43.0896), size=c(640, 640), zoom = 15, maptype = "satellite", key=GoogleAPIkey )

colors<-adjustcolor(brewer.pal(5, 'OrRd'), alpha=c(0.5))
legend.x<-c(-89.4045, -89.406, -89.406, -89.4045)
legend.y<-c(43.093, 43.093, 43.094, 43.094)
legend.df<-data.frame(x=rep(legend.x, 5), y=(rep(legend.y, 5)+rep(seq(0,0.004,0.001), each=4)), group=rep(letters[1:5], each=4))

png("Figures/PicnicPointFootprint.png", res=600, width=6,height=6, units="in")

ggmap(gmap_Mendota) + 
  labs(x=expression(paste('Longitude (', degree, ')', sep='')) , y=expression(paste('Latitude (', degree, ')', sep=''))) +
  geom_polygon(aes(x=long, y=lat), data=Poly80_h, fill=colors[1], col=NA) +
  geom_polygon(aes(x=long, y=lat), data=Poly60_h, fill=colors[2], col=NA) +
  geom_polygon(aes(x=long, y=lat), data=Poly40_h, fill=colors[3], col=NA) +
  geom_polygon(aes(x=long, y=lat), data=Poly20_h, fill=colors[4], col=NA) +
  geom_polygon(aes(x=long, y=lat), data=PolyMax, fill=colors[5], col='black', size=0.2) +
  geom_polygon(aes(x=long, y=lat), data=Poly80, fill=NA, col='black', size=0.2) +
  geom_polygon(aes(x=long, y=lat), data=Poly60, fill=NA, col='black', size=0.2) +
  geom_polygon(aes(x=long, y=lat), data=Poly40, fill=NA, col='black', size=0.2) +
  geom_polygon(aes(x=long, y=lat), data=Poly20, fill=NA, col='black', size=0.2) +
  geom_point(aes(x=x, y=y), data=data.frame(PP), col='black', bg='orange', pch=21, size=4, stroke=1)  +
  geom_point(aes(x=x, y=y), data=data.frame(PP), col='black', bg='orange', pch=21, size=2, stroke=1)  + 
  geom_point(aes(x=x, y=y), data=data.frame(PP), col='black', pch=20, size=0.3, stroke=1)  + 
  scale_fill_manual(values=colors) + 
  geom_polygon(aes(x=x, y=y, fill=group), data=legend.df, colour='black') + 
  annotate(geom="text", x=rep(mean(legend.x[1:2]), 5), y=(mean(legend.y[2:3])+seq(0,0.004, 0.001)), label=rev(c('Max', '20%', '40%', '60%', '80%')), size=3) + 
  theme(legend.position='none') + 

  
  # geom_polygon(aes(x=long, y=lat), data=Mendota_Shoreline, fill=NA, col='black', lwd=3) + 

#   scalebar(x.min=(-89.43), x.max=(-89.403), y.min=(43.098), y.max=(43.099), dist=0.5, dist_unit="km", transform=T, model="WGS84", height=0.5, st.size=4, st.dist=0.5, st.bottom=F, st.color='white')


  scalebar(x.min=(-89.43), x.max=(-89.403), y.min=(43.081), y.max=(43.082), dist=0.5, dist_unit="km", transform=T, model="WGS84", height=0.5, st.size=3, st.dist=0.4, st.bottom=T, st.color='black', border.size=0.5)


dev.off()






legend.df_2panel <- legend.df %>%
  filter(group %in% c('d', 'e'))
colors<-adjustcolor(brewer.pal(5, 'OrRd'), alpha=c(0.3))


png("Figures/PicnicPointFootprint_80Max.png", res=600, width=6,height=6, units="in")

ggmap(gmap_Mendota) + 
  labs(x=expression(paste('Longitude (', degree, ')', sep='')) , y=expression(paste('Latitude (', degree, ')', sep=''))) +
  geom_polygon(aes(x=long, y=lat), data=Poly80, fill=colors[2], col=NA) +
  # geom_polygon(aes(x=long, y=lat), data=Poly60_h, fill=colors[2], col=NA) +
  # geom_polygon(aes(x=long, y=lat), data=Poly40_h, fill=colors[3], col=NA) +
  # geom_polygon(aes(x=long, y=lat), data=Poly20_h, fill=colors[4], col=NA) +
  geom_polygon(aes(x=long, y=lat), data=PolyMax, fill=colors[5], col='black', size=0.2) +
  geom_polygon(aes(x=long, y=lat), data=Poly80, fill=NA, col='black', size=0.2) +
  # geom_polygon(aes(x=long, y=lat), data=Poly60, fill=NA, col='black', size=0.2) +
  # geom_polygon(aes(x=long, y=lat), data=Poly40, fill=NA, col='black', size=0.2) +
  # geom_polygon(aes(x=long, y=lat), data=Poly20, fill=NA, col='black', size=0.2) +
  geom_point(aes(x=x, y=y), data=data.frame(PP), col='black', bg='orange', pch=21, size=4, stroke=1)  +
  geom_point(aes(x=x, y=y), data=data.frame(PP), col='black', bg='orange', pch=21, size=2, stroke=1)  + 
  geom_point(aes(x=x, y=y), data=data.frame(PP), col='black', pch=20, size=0.3, stroke=1)  + 
  scale_fill_manual(values=colors[c(2,5)]) + 
  geom_polygon(aes(x=x, y=y, fill=group), data=legend.df_2panel, colour='black') + 
  annotate(geom="text", x=rep(mean(legend.x[1:2]), 2), y=(mean(legend.y[2:3])+seq(0.003,0.004, 0.001)), label=rev(c('Max', '80%')), size=3) +
  # annotate(geom="text", x= -89.42, y=43.0935, label='80%', size=3) +
  # annotate(geom="text", x= -89.42, y=43.0935, label='Max', size=3) +
  theme(legend.position='none') + 
  
  
  # geom_polygon(aes(x=long, y=lat), data=Mendota_Shoreline, fill=NA, col='black', lwd=3) + 
  
  #   scalebar(x.min=(-89.43), x.max=(-89.403), y.min=(43.098), y.max=(43.099), dist=0.5, dist_unit="km", transform=T, model="WGS84", height=0.5, st.size=4, st.dist=0.5, st.bottom=F, st.color='white')
  
  
  scalebar(x.min=(-89.43), x.max=(-89.403), y.min=(43.081), y.max=(43.082), dist=0.5, dist_unit="km", transform=T, model="WGS84", height=0.5, st.size=3, st.dist=0.4, st.bottom=T, st.color='black', border.size=0.5)


dev.off()


