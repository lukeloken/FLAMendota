# ####################################################
# Workflow to compute flux of gas across lake surface
# Spatially explicit
# ###################################################

# inputs
# spdf - spatial points data frame
# data frame must include temperature (C) and gas concentration (uM)
# shoreline = lake outline (spatial lines or spatial polygons)
# spatial objects need to be same projection, prefereably UTM
# wind data (speed and direction) (single value for each, may integrate speed/direction range)
# elevation (m) or atm pressure (atms) for calculating saturation concentrations
# Without pressure or elevation, default is sea-level)

# Code amends original spatial points data frame and adds the following data
# fetch distance (m)
# k600 (cm/hr)
# efflux of CO2 and CH4 (mmol/m2/d)

# Load packages and spatial functions
library(LakeMetabolizer)
library(rgdal)
source('R/CalculateSpatialFetch.R')
source('R/CalculateSpatialk600.R')


# ##################
# Load data
# ##################

# #####
# Wind
# #####
dailywind <-readRDS('Data/Dailywind.rds')

wind_height=2 # height of anamometer above lake surface

# ############
# Spatial data
# ############

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


#New loop
# Go through days and calculate fetch and k based on daily mean wind speed/direction

daynumbers<-dailywind$daynumbers
Fetch_list<-list()
day=1
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


# Loop through daily maps and generate summaries
probs<-c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)

#Build dataframes to populate
fetch_dataframe<-as.data.frame(matrix(nrow=length(daynumbers), ncol=12))
names(fetch_dataframe)<-c('Daynum', 'Sampledate', 'Min', 'Mean', 'Max', 'Q05', 'Q10', 'Q25', 'Q50', 'Q75', 'Q90', 'Q95')
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
  
  
  #Look at data
  hist(map$fetch_km)
  summary(map)
  spplot(map, zcol='fetch_km', cuts=99, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste("Fetch distance (km)")))
  
  spplot(map, zcol='k600', cuts=20,  colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(k[600], " (cm/hr)")))
  
  
}





# Test plots. Looks at last 'day'
spplot(Fetch_list[[day]], zcol='fetch_km', cuts=99, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste("Fetch distance (km)")))

spplot(Fetch_list[[day]], zcol='k600', cuts=20,  colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(k[600], " (cm/hr)")))


# Old Code Below

# Test names
# spdf<-Mendota_surface_UTM
spdf<-Mendota_grid
gridded(spdf) <- TRUE
shoreline<-Mendota_Shoreline_UTM
fetch_name<-'fetch'
temp_name<-"TempC"
conc_name<-'CO2uM_t'
K600_name<-'k600'
elevation=257 #Mendota
wind_speed=4 #test variables
wind_height=2
wind_dir<-210

#Calculate Fetch (m)
spdf$fetch<-SpatialFetch(spdf, wind_dir=wind_dir, shoreline=shoreline, projected=T, dmax=10000)
spdf$fetch_km<-spdf$fetch/1000

#Calculate k600 (cm/hr)
spdf$k600<-Spatialk600(spdf, wind_speed=wind_speed, wind_height=wind_height, fetch_name='fetch')

#Calculate Efflux (mmol/m2/d)
spdf$CH4efflux<-SpatailFlux(spdf=spdf, conc_name='CH4uM_t', temp_name='TempC', K600_name='k600', gas='CH4', elevation = 257)
spdf$CO2efflux<-SpatailFlux(spdf=spdf, conc_name='CO2uM_t', temp_name='TempC', K600_name='k600', gas='CO2', elevation = 257)

summary(spdf$fetch)
summary(spdf$k600)
summary(spdf$CH4efflux)
summary(spdf$CO2efflux)


# Example Figure

#fetch and k600 only
spplot(spdf, zcol='fetch_km', cuts=99, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste("Fetch distance (km)")))

spplot(spdf, zcol='k600', cuts=20,  colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), main=expression(paste(k[600], " (cm/hr)")))



p1<-spplot(spdf, zcol='fetch_km', cuts=100, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), cex=0.7, pch=15, main=expression(paste("Fetch distance (km)")))

p2<-spplot(spdf, zcol='k600', cuts=20,  colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), cex=.7, pch=15, main=expression(paste(k[600], " (cm/hr)")))

p3<-spplot(spdf, zcol='CO2uM_t', cuts=100, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), cex=.7, pch=15, main=expression(paste(CO[2], " (", mu, "M)")))

p4<-spplot(spdf, zcol='CO2efflux', cuts=100,  colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), cex=.7, pch=15, main=expression(paste(CO[2], " efflux (mmol m"^"-2", " d"^"-1", ")")))

p5<-spplot(spdf, zcol='CH4uM_t', cuts=100, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), cex=.7, pch=15, main=expression(paste(CH[4], " (", mu, "M)")))

p6<-spplot(spdf, zcol='CH4efflux', cuts=100, colorkey=TRUE, sp.layout=list(shoreline, col=1, fill=0, lwd=3, lty=1, first=F), cex=.7, pch=15, main=expression(paste(CH[4], " efflux (mmol m"^"-2", " d"^"-1", ")")))


png('Figures/ExampleFourPanel.png', width=8, height=10, units='in', res=200, bg='white')

print(p1, position=c(0,.6666,.5,1), more=T, justify="left")
print(p2, position=c(.5,.6666,1,1), more=T, justify="left")
print(p3, position=c(0,.3333,.5,.6666), more=T, justify="left")
print(p4, position=c(.5,.3333,1,.6666), more=T, justify="left")
print(p5, position=c(0,0,.5,.3333), more=T, justify="left")
print(p6, position=c(.5,0,1,.3333), more=F, justify="left")

dev.off()

