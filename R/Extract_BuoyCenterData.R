

# Load packages
library(gtools)

sample_dir<-"E:/Dropbox/FLAME_YaharaLakes/Data/"

directories<-list.files(sample_dir)
directories_ME<-directories[grep('LakeMendota', directories)]
directories_ME2016<-directories[grep('2016', directories_ME)]

ME_data<-as.data.frame(matrix(nrow=0, ncol=0))

dir<-directories_ME2016[20]
for (dir in directories_ME2016){
  subdir<-paste(sample_dir, dir, sep="")
  subdir_files<-list.files(subdir)
  
  file<-subdir_files[grep('_Samples.csv', subdir_files)]
  
  if (length(file)==1){
  data1<-read.csv(paste(subdir, file, sep="/"), header=T)
  ME_data<-smartbind(ME_data, data1, fill=NA)
  }
  
}

sites<-unique(ME_data$Sample.Notes )
buoy_names<-c(sites[grep('buoy', sites, ignore.case=T)], 'Deep Hole')

Buoy_data<-ME_data[ME_data$Sample.Notes %in% buoy_names,]
str(Buoy_data)

Buoy_data$DateTime<-as.POSIXct(Buoy_data$DateTime, tz="UTC")
Buoy_data$Date<-as.Date(Buoy_data$DateTime, tz="UTC")


plot(Buoy_data$Date, Buoy_data$XCO2Dppm)
plot(Buoy_data$Date, Buoy_data$XCH4Dppm)




Buoy_daily = aggregate(Buoy_data, by=list(Buoy_data$Date), FUN=mean )
names(Buoy_daily)[1]<-c('Date')
plot(Buoy_daily$Date, Buoy_daily$XCO2Dppm)
plot(Buoy_daily$Date, Buoy_daily$XCH4Dppm)


