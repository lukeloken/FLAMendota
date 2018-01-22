
library(lubridate)
library(dplyr)


phyto<-read.csv('Data/phytoplankton_madison_v2.csv', header=T, stringsAsFactors=F, skip=1)

head(phyto)

phyto.daily = 
  phyto %>%
  group_by(sampledate) %>%
  select(biovolume_conc, division) %>%
  summarise(
    biovolumne_tot = sum(biovolume_conc, na.rm=T),
    cyano_tot =sum(biovolume_conc[division=='Cyanophyta'], na.rm=T)
  )
phyto.daily$sampledate<-ymd(phyto.daily$sampledate)
phyto.daily$month<-month(phyto.daily$sampledate)
phyto.daily$cyano_per<-phyto.daily$cyano_tot/phyto.daily$biovolumne_tot

plot(phyto.daily$cyano_per~ phyto.daily$sampledate, type='l')
points(phyto.daily$cyano_per[phyto.daily$month %in% seq(6:10)]~ phyto.daily$sampledate [phyto.daily$month %in% seq(6:10)], type='p', col='red', pch=16)
abline(v=ymd('2016-01-01'), col='green')

