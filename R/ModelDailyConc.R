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

# make a table of coordinates 
coordinates<-as.data.frame(maps_list[[1]]@coords)
names(coordinates)<-c('long', 'lat')
coordinates$pixelID<-seq(1:nrow(coordinates))

# make a table of dates
datestable<-data.frame(dailydates, as.numeric(dailydates))

# Create a flat data frame and a list of dataframes
data_list<-list()
for (map in 1:length(maps_list)){
  
  data_list[[map]]<-maps_list[[map]]@data
  data_list[[map]]$long<-maps_list[[map]]@coords[,1]
  data_list[[map]]$lat<-maps_list[[map]]@coords[,2]
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

# approxlist<-list()

#create empty array to fill with predicted values
# dimensions are (date, pixelID, variable)
approxarray<-array(data=NA, dim=c(length(dailydates),length(unique(data_df$pixelID)),length(approxnames)), dimnames=list(dailydates, unique(data_df$pixelID), approxnames))

# Loop through all pixels (and by variable) and interpolate through time. 
locationID<-1
for (locationID in unique(data_df$pixelID)){
  subsetdata<-data_df[data_df$pixelID==locationID,]
  
#   approxdata<-as.data.frame(matrix(ncol=length(approxnames)+1, nrow=length(dailydates)))
#   names(approxdata)<-c('date', approxnames)
#   approxdata$date<-dailydates
  
  variable<-approxnames[1]
  for (variable in approxnames){
    
    approxtable<-approx(x=subsetdata$date, y=subsetdata[,variable], xout=dailydates)
    # approxdata[variable]<-approxtable$y
    
    approxarray[,locationID,variable]<-approxtable$y
    
  }
  # approxlist[[locationID]]<-approxdata

}

str(approxarray)
# dimension tables for array
datestable
coordinates
approxnames


# Save array
saveRDS(approxarray, file='Data/DailyFlamebyPixel.rds')
