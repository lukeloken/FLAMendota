setwd("C:/Dropbox/FLAME/Data")

library(maptools)
library(RgoogleMaps)
library(rgdal)
library(plyr)


# Set directory to publication and get variable names

DocFolder<-("C:/Dropbox/FLAME_YaharaLakes/Documents/DataPublication")
DataFolder<-("C:/Dropbox/FLAME_YaharaLakes/Data")
setwd(DocFolder)
metatemplate<-read.csv('FLAMendota_FlameSensor_metadata.csv', header=T,stringsAsFactors = F)
variables<-metatemplate$Column.name

# chemtemplate<-read.csv('FLAMendota_FlameSensor_metadata.csv', header=T,stringsAsFactors = F)
# chemvariables<-chemtemplate$Column.name
# badvarnames<-c("Sample.Name", "NO3.NO2" , "Total.N..F.", "Total.N..UF.", "Total.P..F.", "Total.P..UF." )
# goodvarnames<-c("Sample_Name", "NO3 NO2" , "Total N (F)", "Total N (UF)", "Total P (F)", "Total P (UF)")

# variables<-variables[-which(variables %in% c('siteid'))]

# USGSvars<- c('ltime', 'Latitude', 'Longitude', 'TempC', 'SPCuScm', 'TurbFNU', 'NITRATEMG')
# USGSvars_NiceNames<- c('DateTime', 'Latitude', 'Longitude', 'Temperature', 'SpecificConductivity', 'Turbidity', 'Nitrate')

# =======================================
# Merge 13 UMR day shapefiles
# =======================================
setwd(DataFolder)
folders<-list.files()
folders<-folders[grep('LakeMendota', folders)]
folders<-folders[-grep('2017', folders)]


i=1
shapelist<-list()
for (i in 1:length(folders)){
  files<-list.files(folders[i])
  goodfile<-files[grep('cleaned.csv', files)]
  shape<-read.csv(paste(folders[i], "/", goodfile, sep=""), header=T)
  
  year<-median(year(shape$ltime), na.rm=T)
  if (year<=2015){ 
    shape$ltime<-as.POSIXct(shape$ltime, tz='America/Chicago')
  } else if (year>=2016) {
    shape$ltime<-as.POSIXct(shape$ltime, tz='UTC')
    attributes(shape$ltime)$tzone <- 'America/Chicago'
    }
   shapelist[[i]]<-shape
}
  
test<-ldply(shapelist,data.frame)


Sortedtest<-test[intersect(variables, names(test))]
head(Sortedtest)

test_noNA<-Sortedtest[-which(is.na(Sortedtest$Latitude) | is.na(Sortedtest$Longitude)),]


write.table(test_noNA, paste(DocFolder, '/', 'LakeMendota_SpatialSurfaceChemistry.csv', sep=''), row.names=F, sep=',')


# list.files('..')
# WaterChem<-read.csv('../MergedSamples.csv', header=T,stringsAsFactors = F)
# 
# names(WaterChem)[match(badvarnames, names(WaterChem))]<-goodvarnames
# 
# WaterChem2<-WaterChem[intersect(chemvariables, names(WaterChem))]
# 
# End csv merge



