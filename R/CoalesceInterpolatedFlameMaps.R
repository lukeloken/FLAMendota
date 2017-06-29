# =============================================
# Code to Extract Flame Data from Loken Desktop
# Bring all interpolated maps into a single R object (list)
# Inputs = individual idw surface
# outputs = list of surfaces
# =============================================

# Load packages
library(gtools)

#### Merge all spatial summaries ####

data_dir<-"E:/Dropbox/FLAME_YaharaLakes/Data"

dates<-list.files(data_dir)
dates2016<-dates[grep('2016', dates)]
dates2016<-dates2016[grep('LakeMendota', dates2016)]

maps_list<-list()
points_list<-list()

dir<-1
for (dir in 1:length(dates2016)){
  file_dir<-paste(data_dir, '/', dates2016[dir], '/shapefiles_idw', sep="")
  files<-list.files(file_dir)
  filename<-files[grep('cleaned.dbf', files)]
  loadname<-sub('.dbf', '', filename)
  
  shapefile<-readOGR(file_dir, loadname)
  maps_list[[dir]]<-shapefile
  names(maps_list)[[dir]]<-loadname
  
  points_dir<-paste(data_dir, '/', dates2016[dir], sep="")
  points_files<-list.files(points_dir)
  pointsname<-points_files[grep('cleaned.csv', points_files)]
  
  points_df<-read.csv(paste(points_dir, '/', pointsname,  sep=""), header=T, stringsAsFactors = F)
  points_list[[dir]]<-points_df
  names(points_list)[[dir]]<-loadname
  
}

# Save to Git folder
saveRDS(maps_list, file='Data/All2016Maps.rds')
saveRDS(points_list, file='Data/All2016Points.rds')


