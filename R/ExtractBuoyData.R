# Download Buoy Data and generate Rdata file

# ====================================
# Copy and Paste from LTER
# ====================================

# Package ID: knb-lter-ntl.129.12 Cataloging System:https://pasta.lternet.edu.
# Data set title: North Temperate Lakes LTER: High Frequency Data: Meteorological, Dissolved Oxygen, Chlorophyll, Phycocyanin - Lake Mendota Buoy 2006 - current.
# Data set creator:    - Center for Limnology 
# Data set creator:    - NTL LTER 
# Metadata Provider:    - North Temperate Lakes LTER 
# Contact:    - Information Manager LTER Network Office  - tech-support@lternet.edu
# Contact:    - NTL LTER Information Manager University of Wisconsin  - infomgr@lter.limnology.wisc.edu
# Contact:    - NTL LTER Lead PI Center for Limnology  - leadpi@lter.limnology.wisc.edu
# Metadata Link: https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-ntl.129.12
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@lternet.edu 

infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/129/12/aaeb2bb33887a65db75b1c9161764800" 
infile1 <- sub("^https","http",infile1) 
dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "year4",     
                 "sampledate",     
                 "month",     
                 "daynum",     
                 "avg.air.temp",     
                 "flag.avg.air.temp",     
                 "avg.rel.hum",     
                 "flag.avg.rel.hum",     
                 "avg.wind.speed",     
                 "flag.avg.wind.speed",     
                 "avg.wind.dir",     
                 "flag.avg.wind.dir",     
                 "avg.chlor",     
                 "flag.chlor",     
                 "avg.phycocyanin",     
                 "flag.phycocyanin",     
                 "avg.par",     
                 "flag.par",     
                 "avg.par.below",     
                 "flag.par.below",     
                 "avg.opt.wtemp",     
                 "flag.avg.opt.wtemp",     
                 "avg.opt.dosat.raw",     
                 "flag.avg.opt.dosat.raw",     
                 "avg.opt.do.raw",     
                 "flag.avg.opt.do.raw"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
dt1$sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
rm(tmpDateFormat) 
if (class(dt1$month)=="factor") dt1$month <-as.numeric(levels(dt1$month))[as.integer(dt1$month) ]
if (class(dt1$daynum)=="factor") dt1$daynum <-as.numeric(levels(dt1$daynum))[as.integer(dt1$daynum) ]
if (class(dt1$avg.air.temp)=="factor") dt1$avg.air.temp <-as.numeric(levels(dt1$avg.air.temp))[as.integer(dt1$avg.air.temp) ]
if (class(dt1$flag.avg.air.temp)!="factor") dt1$flag.avg.air.temp<- as.factor(dt1$flag.avg.air.temp)
if (class(dt1$avg.rel.hum)=="factor") dt1$avg.rel.hum <-as.numeric(levels(dt1$avg.rel.hum))[as.integer(dt1$avg.rel.hum) ]
if (class(dt1$flag.avg.rel.hum)!="factor") dt1$flag.avg.rel.hum<- as.factor(dt1$flag.avg.rel.hum)
if (class(dt1$avg.wind.speed)=="factor") dt1$avg.wind.speed <-as.numeric(levels(dt1$avg.wind.speed))[as.integer(dt1$avg.wind.speed) ]
if (class(dt1$flag.avg.wind.speed)!="factor") dt1$flag.avg.wind.speed<- as.factor(dt1$flag.avg.wind.speed)
if (class(dt1$avg.wind.dir)=="factor") dt1$avg.wind.dir <-as.numeric(levels(dt1$avg.wind.dir))[as.integer(dt1$avg.wind.dir) ]
if (class(dt1$flag.avg.wind.dir)!="factor") dt1$flag.avg.wind.dir<- as.factor(dt1$flag.avg.wind.dir)
if (class(dt1$avg.chlor)=="factor") dt1$avg.chlor <-as.numeric(levels(dt1$avg.chlor))[as.integer(dt1$avg.chlor) ]
if (class(dt1$flag.chlor)!="factor") dt1$flag.chlor<- as.factor(dt1$flag.chlor)
if (class(dt1$avg.phycocyanin)=="factor") dt1$avg.phycocyanin <-as.numeric(levels(dt1$avg.phycocyanin))[as.integer(dt1$avg.phycocyanin) ]
if (class(dt1$flag.phycocyanin)!="factor") dt1$flag.phycocyanin<- as.factor(dt1$flag.phycocyanin)
if (class(dt1$avg.par)=="factor") dt1$avg.par <-as.numeric(levels(dt1$avg.par))[as.integer(dt1$avg.par) ]
if (class(dt1$flag.par)!="factor") dt1$flag.par<- as.factor(dt1$flag.par)
if (class(dt1$avg.par.below)=="factor") dt1$avg.par.below <-as.numeric(levels(dt1$avg.par.below))[as.integer(dt1$avg.par.below) ]
if (class(dt1$flag.par.below)!="factor") dt1$flag.par.below<- as.factor(dt1$flag.par.below)
if (class(dt1$avg.opt.wtemp)=="factor") dt1$avg.opt.wtemp <-as.numeric(levels(dt1$avg.opt.wtemp))[as.integer(dt1$avg.opt.wtemp) ]
if (class(dt1$flag.avg.opt.wtemp)!="factor") dt1$flag.avg.opt.wtemp<- as.factor(dt1$flag.avg.opt.wtemp)
if (class(dt1$avg.opt.dosat.raw)=="factor") dt1$avg.opt.dosat.raw <-as.numeric(levels(dt1$avg.opt.dosat.raw))[as.integer(dt1$avg.opt.dosat.raw) ]
if (class(dt1$flag.avg.opt.dosat.raw)!="factor") dt1$flag.avg.opt.dosat.raw<- as.factor(dt1$flag.avg.opt.dosat.raw)
if (class(dt1$avg.opt.do.raw)=="factor") dt1$avg.opt.do.raw <-as.numeric(levels(dt1$avg.opt.do.raw))[as.integer(dt1$avg.opt.do.raw) ]
if (class(dt1$flag.avg.opt.do.raw)!="factor") dt1$flag.avg.opt.do.raw<- as.factor(dt1$flag.avg.opt.do.raw)

# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(year4)
summary(sampledate)
summary(month)
summary(daynum)
summary(avg.air.temp)
summary(flag.avg.air.temp)
summary(avg.rel.hum)
summary(flag.avg.rel.hum)
summary(avg.wind.speed)
summary(flag.avg.wind.speed)
summary(avg.wind.dir)
summary(flag.avg.wind.dir)
summary(avg.chlor)
summary(flag.chlor)
summary(avg.phycocyanin)
summary(flag.phycocyanin)
summary(avg.par)
summary(flag.par)
summary(avg.par.below)
summary(flag.par.below)
summary(avg.opt.wtemp)
summary(flag.avg.opt.wtemp)
summary(avg.opt.dosat.raw)
summary(flag.avg.opt.dosat.raw)
summary(avg.opt.do.raw)
summary(flag.avg.opt.do.raw) 
detach(dt1)               


infile2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/129/12/fa63fef3fedaf81384819d0cde1b8530" 
infile2 <- sub("^https","http",infile2) 
dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "year4",     
                 "sampledate",     
                 "month",     
                 "daynum",     
                 "avg.air.temp",     
                 "flag.avg.air.temp",     
                 "avg.rel.hum",     
                 "flag.avg.rel.hum",     
                 "avg.wind.speed",     
                 "flag.avg.wind.speed",     
                 "avg.wind.dir",     
                 "flag.avg.wind.dir",     
                 "avg.chlor",     
                 "flag.chlor",     
                 "avg.phycocyanin",     
                 "flag.phycocyanin",     
                 "avg.par",     
                 "flag.par",     
                 "avg.par.below",     
                 "flag.par.below",     
                 "avg.opt.wtemp",     
                 "flag.avg.opt.wtemp",     
                 "avg.opt.dosat.raw",     
                 "flag.avg.opt.dosat.raw",     
                 "avg.opt.do.raw",     
                 "flag.avg.opt.do.raw"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$year4)=="factor") dt2$year4 <-as.numeric(levels(dt2$year4))[as.integer(dt2$year4) ]                                   
# attempting to convert dt2$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
dt2$sampledate<-as.Date(dt2$sampledate,format=tmpDateFormat)
rm(tmpDateFormat) 
if (class(dt2$month)=="factor") dt2$month <-as.numeric(levels(dt2$month))[as.integer(dt2$month) ]
if (class(dt2$daynum)=="factor") dt2$daynum <-as.numeric(levels(dt2$daynum))[as.integer(dt2$daynum) ]
if (class(dt2$avg.air.temp)=="factor") dt2$avg.air.temp <-as.numeric(levels(dt2$avg.air.temp))[as.integer(dt2$avg.air.temp) ]
if (class(dt2$flag.avg.air.temp)!="factor") dt2$flag.avg.air.temp<- as.factor(dt2$flag.avg.air.temp)
if (class(dt2$avg.rel.hum)=="factor") dt2$avg.rel.hum <-as.numeric(levels(dt2$avg.rel.hum))[as.integer(dt2$avg.rel.hum) ]
if (class(dt2$flag.avg.rel.hum)!="factor") dt2$flag.avg.rel.hum<- as.factor(dt2$flag.avg.rel.hum)
if (class(dt2$avg.wind.speed)=="factor") dt2$avg.wind.speed <-as.numeric(levels(dt2$avg.wind.speed))[as.integer(dt2$avg.wind.speed) ]
if (class(dt2$flag.avg.wind.speed)!="factor") dt2$flag.avg.wind.speed<- as.factor(dt2$flag.avg.wind.speed)
if (class(dt2$avg.wind.dir)=="factor") dt2$avg.wind.dir <-as.numeric(levels(dt2$avg.wind.dir))[as.integer(dt2$avg.wind.dir) ]
if (class(dt2$flag.avg.wind.dir)!="factor") dt2$flag.avg.wind.dir<- as.factor(dt2$flag.avg.wind.dir)
if (class(dt2$avg.chlor)=="factor") dt2$avg.chlor <-as.numeric(levels(dt2$avg.chlor))[as.integer(dt2$avg.chlor) ]
if (class(dt2$flag.chlor)!="factor") dt2$flag.chlor<- as.factor(dt2$flag.chlor)
if (class(dt2$avg.phycocyanin)=="factor") dt2$avg.phycocyanin <-as.numeric(levels(dt2$avg.phycocyanin))[as.integer(dt2$avg.phycocyanin) ]
if (class(dt2$flag.phycocyanin)!="factor") dt2$flag.phycocyanin<- as.factor(dt2$flag.phycocyanin)
if (class(dt2$avg.par)=="factor") dt2$avg.par <-as.numeric(levels(dt2$avg.par))[as.integer(dt2$avg.par) ]
if (class(dt2$flag.par)!="factor") dt2$flag.par<- as.factor(dt2$flag.par)
if (class(dt2$avg.par.below)=="factor") dt2$avg.par.below <-as.numeric(levels(dt2$avg.par.below))[as.integer(dt2$avg.par.below) ]
if (class(dt2$flag.par.below)!="factor") dt2$flag.par.below<- as.factor(dt2$flag.par.below)
if (class(dt2$avg.opt.wtemp)=="factor") dt2$avg.opt.wtemp <-as.numeric(levels(dt2$avg.opt.wtemp))[as.integer(dt2$avg.opt.wtemp) ]
if (class(dt2$flag.avg.opt.wtemp)!="factor") dt2$flag.avg.opt.wtemp<- as.factor(dt2$flag.avg.opt.wtemp)
if (class(dt2$avg.opt.dosat.raw)=="factor") dt2$avg.opt.dosat.raw <-as.numeric(levels(dt2$avg.opt.dosat.raw))[as.integer(dt2$avg.opt.dosat.raw) ]
if (class(dt2$flag.avg.opt.dosat.raw)!="factor") dt2$flag.avg.opt.dosat.raw<- as.factor(dt2$flag.avg.opt.dosat.raw)
if (class(dt2$avg.opt.do.raw)=="factor") dt2$avg.opt.do.raw <-as.numeric(levels(dt2$avg.opt.do.raw))[as.integer(dt2$avg.opt.do.raw) ]
if (class(dt2$flag.avg.opt.do.raw)!="factor") dt2$flag.avg.opt.do.raw<- as.factor(dt2$flag.avg.opt.do.raw)

# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(year4)
summary(sampledate)
summary(month)
summary(daynum)
summary(avg.air.temp)
summary(flag.avg.air.temp)
summary(avg.rel.hum)
summary(flag.avg.rel.hum)
summary(avg.wind.speed)
summary(flag.avg.wind.speed)
summary(avg.wind.dir)
summary(flag.avg.wind.dir)
summary(avg.chlor)
summary(flag.chlor)
summary(avg.phycocyanin)
summary(flag.phycocyanin)
summary(avg.par)
summary(flag.par)
summary(avg.par.below)
summary(flag.par.below)
summary(avg.opt.wtemp)
summary(flag.avg.opt.wtemp)
summary(avg.opt.dosat.raw)
summary(flag.avg.opt.dosat.raw)
summary(avg.opt.do.raw)
summary(flag.avg.opt.do.raw) 
detach(dt2)               


infile3  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/129/12/168e4638bdb8a88990e031605dc11041" 
infile3 <- sub("^https","http",infile3) 
dt3 <-read.csv(infile3,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "year4",     
                 "station.id",     
                 "sampledate",     
                 "month",     
                 "daynum",     
                 "sample.time",     
                 "air.temp",     
                 "flag.air.temp",     
                 "rel.hum",     
                 "flag.rel.hum",     
                 "wind.speed",     
                 "flag.wind.speed",     
                 "wind.dir",     
                 "flag.wind.dir",     
                 "chlor",     
                 "flag.chlor",     
                 "phycocyanin",     
                 "flag.phycocyanin",     
                 "opt.do.raw",     
                 "opt.dosat.raw",     
                 "opt.wtemp",     
                 "flag.opt.do.raw",     
                 "flag.opt.dosat.raw",     
                 "flag.opt.wtemp"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt3$year4)=="factor") dt3$year4 <-as.numeric(levels(dt3$year4))[as.integer(dt3$year4) ]
if (class(dt3$station.id)!="factor") dt3$station.id<- as.factor(dt3$station.id)                                   
# attempting to convert dt3$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
dt3$sampledate<-as.Date(dt3$sampledate,format=tmpDateFormat)
rm(tmpDateFormat) 
if (class(dt3$month)=="factor") dt3$month <-as.numeric(levels(dt3$month))[as.integer(dt3$month) ]
if (class(dt3$daynum)=="factor") dt3$daynum <-as.numeric(levels(dt3$daynum))[as.integer(dt3$daynum) ]
if (class(dt3$air.temp)=="factor") dt3$air.temp <-as.numeric(levels(dt3$air.temp))[as.integer(dt3$air.temp) ]
if (class(dt3$flag.air.temp)!="factor") dt3$flag.air.temp<- as.factor(dt3$flag.air.temp)
if (class(dt3$rel.hum)=="factor") dt3$rel.hum <-as.numeric(levels(dt3$rel.hum))[as.integer(dt3$rel.hum) ]
if (class(dt3$flag.rel.hum)!="factor") dt3$flag.rel.hum<- as.factor(dt3$flag.rel.hum)
if (class(dt3$wind.speed)=="factor") dt3$wind.speed <-as.numeric(levels(dt3$wind.speed))[as.integer(dt3$wind.speed) ]
if (class(dt3$flag.wind.speed)!="factor") dt3$flag.wind.speed<- as.factor(dt3$flag.wind.speed)
if (class(dt3$wind.dir)=="factor") dt3$wind.dir <-as.numeric(levels(dt3$wind.dir))[as.integer(dt3$wind.dir) ]
if (class(dt3$flag.wind.dir)!="factor") dt3$flag.wind.dir<- as.factor(dt3$flag.wind.dir)
if (class(dt3$chlor)=="factor") dt3$chlor <-as.numeric(levels(dt3$chlor))[as.integer(dt3$chlor) ]
if (class(dt3$flag.chlor)!="factor") dt3$flag.chlor<- as.factor(dt3$flag.chlor)
if (class(dt3$phycocyanin)=="factor") dt3$phycocyanin <-as.numeric(levels(dt3$phycocyanin))[as.integer(dt3$phycocyanin) ]
if (class(dt3$flag.phycocyanin)!="factor") dt3$flag.phycocyanin<- as.factor(dt3$flag.phycocyanin)
if (class(dt3$opt.do.raw)=="factor") dt3$opt.do.raw <-as.numeric(levels(dt3$opt.do.raw))[as.integer(dt3$opt.do.raw) ]
if (class(dt3$opt.dosat.raw)=="factor") dt3$opt.dosat.raw <-as.numeric(levels(dt3$opt.dosat.raw))[as.integer(dt3$opt.dosat.raw) ]
if (class(dt3$opt.wtemp)=="factor") dt3$opt.wtemp <-as.numeric(levels(dt3$opt.wtemp))[as.integer(dt3$opt.wtemp) ]
if (class(dt3$flag.opt.do.raw)!="factor") dt3$flag.opt.do.raw<- as.factor(dt3$flag.opt.do.raw)
if (class(dt3$flag.opt.dosat.raw)!="factor") dt3$flag.opt.dosat.raw<- as.factor(dt3$flag.opt.dosat.raw)
if (class(dt3$flag.opt.wtemp)!="factor") dt3$flag.opt.wtemp<- as.factor(dt3$flag.opt.wtemp)

# Here is the structure of the input data frame:
str(dt3)                            
attach(dt3)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(year4)
summary(station.id)
summary(sampledate)
summary(month)
summary(daynum)
summary(sample.time)
summary(air.temp)
summary(flag.air.temp)
summary(rel.hum)
summary(flag.rel.hum)
summary(wind.speed)
summary(flag.wind.speed)
summary(wind.dir)
summary(flag.wind.dir)
summary(chlor)
summary(flag.chlor)
summary(phycocyanin)
summary(flag.phycocyanin)
summary(opt.do.raw)
summary(opt.dosat.raw)
summary(opt.wtemp)
summary(flag.opt.do.raw)
summary(flag.opt.dosat.raw)
summary(flag.opt.wtemp) 
detach(dt3)               

# ====================================
# End Copy and Paste from LTER
# ====================================
