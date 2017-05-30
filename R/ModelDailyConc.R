# ####################################################
# Workflow to predict concentration daily maps
# linear interpolation between days at each pixel
# ###################################################

# Inputs
# Concentration spdfs

# Outputs
# list of spdfs for each day starting on first flame run ending on last flame run
# Note this concentration list does not include the 2-4 week period approaching ice-on and after ice-off
rm(list = ls())

library(rgdal)
library(gtools)

# Set UTM projection (Zone 15 for Wisconsin)
projection <- "+proj=utm +zone=15 ellps=WGS84"


# ##################
# Load data
# ##################

# #####
# idw lake surfaces
# #####

maps_list<-readRDS('Data/All2016Maps.rds')
filenames<-names(maps_list)
dates<-sub('cleaned', '', filenames)
dates<-as.Date(sub('LakeMendota', '', dates))

#Create vector of dates to create estimates for each variable
dailydates<-seq(range(dates)[1], range(dates)[2], by='days')



# Create a flat data frame and a list of dataframes
data_list<-list()
for (map in 1:length(maps_list)){
  
  data_list[[map]]<-maps_list[[map]]@data
  data_list[[map]]$lat<-maps_list[[map]]@coords[,1]
  data_list[[map]]$long<-maps_list[[map]]@coords[,2]
  data_list[[map]]$date<-dates[map]
  data_list[[map]]$pixelID<- seq.int(nrow(data_list[[map]]))
  if (map==1){
    data_df<-data_list[[map]]}
  else if (map>1){
    data_df<-smartbind(data_df, data_list[[map]])}
}
data_df$date<-as.Date(data_df$date)

colnames<-names(data_df)
approxnames<-colnames[2:63]

approxlist<-list()

approxarray<-array(data=NA, dim=c(length(dailydates),length(unique(data_df$pixelID)),length(approxnames)), dimnames=list(dailydates, unique(data_df$pixelID), approxnames))



locationID<-1
for (locationID in unique(data_df$pixelID)){
  subsetdata<-data_df[data_df$pixelID==locationID,]
  
  approxdata<-as.data.frame(matrix(ncol=length(approxnames)+1, nrow=length(dailydates)))
  names(approxdata)<-c('date', approxnames)
  approxdata$date<-dailydates
  
  variable<-approxnames[1]
  for (variable in approxnames){
    
    approxtable<-approx(x=subsetdata$date, y=subsetdata[,variable], xout=dailydates)
    approxdata[variable]<-approxtable$y
    
    approxarray[,locationID,variable]<-approxtable$y
    
  }
  # approxlist[[locationID]]<-approxdata

}

str(approxarray)



#convert list of data into an array
data_array<-array(unlist(data_list), dim = c(nrow(data_list[[1]]), ncol(data_list[[1]]), length(data_list)))

data_array1<-as.array(data_list[[1]], nrow(data_list[[1]]), ncol(data_list[[1]]))

head(data_array[3,,])


# make a list of 'data by pixels'
coordinates<-maps_list[[1]]@coords
pixels<-seq(1:nrow(coordinates))

listbypixel<-list()
basedf<-as.data.frame(matrix(ncol=ncol(maps_list[[1]]@data)+1, nrow=length(dailydates)))
basedf[,1]<-dailydates
names(basedf)[-1]<-names(maps_list[[1]]@data)

for (point in pixels){
  pixeldf<-basedf
  
  
  ...do stuff to interpolate through time at each pixeldf...
  
  listbypixel[[point]]<-pixeldf
  names(listbypixel)[[point]]<-point
}

# Old code (probably delete below)




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


#New loop
# Go through days and calculate fetch and k based on daily mean wind speed/direction

daynumbers<-dailywind$daynumbers
Fetch_list<-list()
day=2
for (day in 1:length(daynumbers)){
  daynum<-dailywind$daynumbers[day]
  wind_speed<-dailywind$speed[day]
  wind_dir<-dailywind$dir[day]
  
  if(!is.na(wind_speed)){
    # Calculate fetch distance (m and km)
    Mendota_grid$fetch<-SpatialFetch(Mendota_grid, wind_dir=wind_dir, shoreline=shoreline, projected=T, dmax=10000)
    Mendota_grid$fetch_km<-Mendota_grid$fetch/1000
    
    #Calculate k600 (cm/hr)
    Mendota_grid$k600<-Spatialk600(Mendota_grid, wind_speed=wind_speed, wind_height=wind_height, fetch_name='fetch')
    gridded(Mendota_grid)<-T
    # Plot daily fetch and k600
    
    png(paste('Figures/DailyFetchMap2016/daynum', daynum, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
    print(spplot(Mendota_grid, zcol='fetch_km', cuts=99, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F),  main=expression(paste("Fetch distance (km)")), xlab=paste('Day #', daynum, "     Wind direction ", round(wind_dir), ' deg', sep="")))
    dev.off()
    
    png(paste('Figures/DailyK600Map2016/daynum', daynum, '.png', sep=""), width=6, height=5, units='in', res=200, bg='white')
    print(spplot(Mendota_grid, zcol='k600', cuts=20,  colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(k[600], " (cm/hr)")), xlab=paste('Day #', daynum, "     Wind direction ", round(wind_dir), ' deg     Speed ', round(wind_speed, digits=1), " m/s", sep="")))
    dev.off()
  }
  
  Fetch_list[[day]]<-Mendota_grid
  
}

str(Fetch_list)

saveRDS(Fetch_list , file='Data/DailyFetchK600maps.rds')

# Test plots. Looks at last 'day'
spplot(Fetch_list[[day]], zcol='fetch_km', cuts=99, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste("Fetch distance (km)")))

spplot(Fetch_list[[day]], zcol='k600', cuts=20,  colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(k[600], " (cm/hr)")))


# Loop through daily maps and generate summaries
probs<-c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)

#Build dataframes to populate
fetch_dataframe<-as.data.frame(matrix(nrow=length(daynumbers), ncol=12))
names(fetch_dataframe)<-c('Daynum', 'Sampledate', 'Min', 'Mean', 'Max', 'Q05', 'Q10', 'Q25', 'Q50', 'Q75', 'Q90', 'Q95')
fetch_dataframe$Daynum<-daynumbers
fetch_dataframe$Sampledate<-dailywind$sampledate[match(dailywind$daynumbers,fetch_dataframe$Daynum)]
k600_dataframe<-fetch_dataframe

fetch_map<-2
for (fetch_map in 1:length(Fetch_list)){
  daynum<-daynumbers[fetch_map]
  map<-Fetch_list[[fetch_map]]
  
  #fetch stats
  summary_fetch<-summary(map$fetch_km)[c(1,4,6)]
  quantiles_fetch<-quantile(map$fetch_km, probs=probs, na.rm=T)
  stats_fetch<-c(summary_fetch, quantiles_fetch)
  
  #k600 stats
  summary_k600<-summary(map$k600)[c(1,4,6)]
  quantiles_k600<-quantile(map$k600, probs=probs, na.rm=T)
  stats_k600<-c(summary_k600, quantiles_k600)
  
  # Output to daily data frames
  fetch_dataframe[fetch_map,3:12]<-stats_fetch
  k600_dataframe[fetch_map,3:12]<-stats_k600

  # Look at data
  # Comment out for processing speed
#   hist(map$fetch_km)
#   summary(map)
#   spplot(map, zcol='fetch_km', cuts=99, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste("Fetch distance (km)")))
#   
#   spplot(map, zcol='k600', cuts=20,  colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(k[600], " (cm/hr)")))
  
  
}

# Save fetch stats to Git folder
saveRDS(fetch_dataframe , file='Data/DailyFetchStats.rds')
write.table(fetch_dataframe , file='Data/DailyFetchStats.csv')

# ##############################################
# K model using Lake Area and Wind speed only. 
# Model B from Vachon and Praire 2013
# I should put this into the Daily K600 data frame above...
# Also can use lake metabolizer (k.vachon.base)
# ##############################################


U10<-wind.scale.base(dailywind$speed, wind_height)
LA<-39850000 #Lake Mendota area square meters

k600_dataframe

# Calculate K600 using Lake Metablizer (Vachon Lake area model, Cole model, and Crusius Model)
# These K600 values are in cm/hr. 
k600_dataframe$K600.VachonLA<-k.vachon.base(U10, lake.area=LA, params=c(2.51,1.48,0.39))*100/24
k600_dataframe$K600.Cole<-k.cole.base(U10)*100/24
k600_dataframe$K600.Crusius<-k.crusius.base(U10)*100/24

#Compare spatially explict k models to Vachon's Lake Area based model
plot(k600_dataframe$Mean, k600_dataframe$K600.VachonLA)
points(k600_dataframe$Q50, k600_dataframe$K600.VachonLA, col="red")
abline(0,1)

#Compare spatially explict k models to Cole's model
plot(k600_dataframe$Mean, K600.Cole)
points(k600_dataframe$Q50, K600.Cole, col="red")
abline(0,1)

#Compare spatially explict k models to Vachon's Lake Area based model
plot(k600_dataframe$Mean, K600.Crusius)
points(k600_dataframe$Q50, K600.Crusius, col="red")
abline(0,1)

plot(U10, k600_dataframe$Mean, pch=16)
points(U10, k600_dataframe$K600.VachonLA, pch=16, col="red")

# Save K table
saveRDS(k600_dataframe , file='Data/DailyK600Stats.rds')
write.table(k600_dataframe , file='Data/DailyK600Stats.csv')


png('Figures/K600boxplotAmongModels.png', width=5, height=3, units='in', res=200, bg='white')
par(mar=c(2,3,1,1))
par(mgp=c(1.5,0.6,0),tck=-0.02)
par(ps=10)

boxplot(c(k600_dataframe[c('Mean', 'K600.VachonLA', 'K600.Cole',  'K600.Crusius')]), names=c('Vachon Fetch', 'Vachon LA', 'Cole', 'Crusius'),ylab=expression(paste(k[600], ' (cm hr'^'-1', ')')), col='grey60', las=1, boxwex=.5, cex=.7)

dev.off()

