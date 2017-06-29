# #######################################################################################
# Code to loop through 'Samples.csv' files and extract all Lake Mendota Buoy Flame Values
# Directory is dependent on Loken-Desktop
# Output single file of all Buoy Samples (not aggregated)
# #######################################################################################

# Load packages
library(gtools)

sample_dir<-"E:/Dropbox/FLAME_YaharaLakes/Data/"

directories<-list.files(sample_dir)
directories_ME<-directories[grep('LakeMendota', directories)]
directories_ME2016<-directories_ME[grep('2016', directories_ME)]

ME_data<-as.data.frame(matrix(nrow=0, ncol=0))

dir<-directories_ME2016[21]
for (dir in directories_ME2016){
  subdir<-paste(sample_dir, dir, sep="")
  subdir_files<-list.files(subdir)
  
  file<-subdir_files[grep('_Samples.csv', subdir_files)]
  
  if (length(file)==1){
    data1<-read.csv(paste(subdir, file, sep="/"), header=T, stringsAsFactors = F)
    
    if (nrow(ME_data)==0){
      ME_data<-data1}
    else {
      ME_data<-smartbind(ME_data, data1, fill=NA)}
  }
}

# Only include Buoy samples
sites<-unique(ME_data$Sample.Notes )
buoy_names<-c(sites[grep('Buoy', sites, ignore.case=T)], 'Deep Hole')

Buoy_data<-ME_data[ME_data$Sample.Notes %in% buoy_names,]
str(Buoy_data)

Buoy_data$DateTime<-as.POSIXct(Buoy_data$DateTime, tz="UTC")
Buoy_data$Date<-as.Date(Buoy_data$DateTime, tz="UTC")


# Save to Git folder
saveRDS(Buoy_data, file='Data/FlameBuoyMeasurements.rds')
write.table(Buoy_data, file='Data/FlameBuoyMeasurements.csv')

#Get average Buoy Location
Buoy_medianLat<-median(Buoy_data$Latitude, na.rm=T)
Buoy_medianLong<-median(Buoy_data$Longitude, na.rm=T)
coords<-data.frame(Buoy_medianLong, Buoy_medianLat)
BuoyLocation<-SpatialPointsDataFrame(coords, data=coords)
proj4string(BuoyLocation)=CRS("+init=epsg:4326")
writeOGR(BuoyLocation, dsn='Data/Shapefiles' ,layer="MendotaBuoyLocation", driver="ESRI Shapefile",  verbose=T, overwrite=T)
