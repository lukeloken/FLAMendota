# #######################################################################################
# Code to loop through 'AtmSamples.csv' files and extract all Lake Mendota Buoy Atmosphere Values
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

dir<-directories_ME2016[1]
for (dir in directories_ME2016){
  subdir<-paste(sample_dir, dir, sep="")
  subdir_files<-list.files(subdir)
  
  file<-subdir_files[grep('_AtmSamples.csv', subdir_files)]
  
  if (length(file)==1){
    data1<-read.csv(paste(subdir, file, sep="/"), header=T, stringsAsFactors = F)
    
    if (nrow(ME_data)==0){
      ME_data<-data1}
    else {
      ME_data<-smartbind(ME_data, data1, fill=NA)}
  }
}

ME_data$DateTime<-as.POSIXct(ME_data$DateTime, tz="UTC")
ME_data$Date<-as.Date(ME_data$DateTime, tz="UTC")

plot(ME_data$DateTime, ME_data$XCO2Dppm, type='o')
plot(ME_data$DateTime, ME_data$XCH4Dppm, type='o')

# Save to Git folder
saveRDS(ME_data, file='Data/FlameBuoyAtmMeasurements.rds')
write.table(ME_data, file='Data/FlameBuoyAtmMeasurements.csv')
