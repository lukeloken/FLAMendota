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
                 "avg_air_temp",     
                 "flag_avg_air_temp",     
                 "avg_rel_hum",     
                 "flag_avg_rel_hum",     
                 "avg_wind_speed",     
                 "flag_avg_wind_speed",     
                 "avg_wind_dir",     
                 "flag_avg_wind_dir",     
                 "avg_chlor",     
                 "flag_chlor",     
                 "avg_phycocyanin",     
                 "flag_phycocyanin",     
                 "avg_par",     
                 "flag_par",     
                 "avg_par_below",     
                 "flag_par_below",     
                 "avg_opt_wtemp",     
                 "flag_avg_opt_wtemp",     
                 "avg_opt_dosat_raw",     
                 "flag_avg_opt_dosat_raw",     
                 "avg_opt_do_raw",     
                 "flag_avg_opt_do_raw"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
dt1$sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
rm(tmpDateFormat) 
if (class(dt1$month)=="factor") dt1$month <-as.numeric(levels(dt1$month))[as.integer(dt1$month) ]
if (class(dt1$daynum)=="factor") dt1$daynum <-as.numeric(levels(dt1$daynum))[as.integer(dt1$daynum) ]
if (class(dt1$avg_air_temp)=="factor") dt1$avg_air_temp <-as.numeric(levels(dt1$avg_air_temp))[as.integer(dt1$avg_air_temp) ]
if (class(dt1$flag_avg_air_temp)!="factor") dt1$flag_avg_air_temp<- as.factor(dt1$flag_avg_air_temp)
if (class(dt1$avg_rel_hum)=="factor") dt1$avg_rel_hum <-as.numeric(levels(dt1$avg_rel_hum))[as.integer(dt1$avg_rel_hum) ]
if (class(dt1$flag_avg_rel_hum)!="factor") dt1$flag_avg_rel_hum<- as.factor(dt1$flag_avg_rel_hum)
if (class(dt1$avg_wind_speed)=="factor") dt1$avg_wind_speed <-as.numeric(levels(dt1$avg_wind_speed))[as.integer(dt1$avg_wind_speed) ]
if (class(dt1$flag_avg_wind_speed)!="factor") dt1$flag_avg_wind_speed<- as.factor(dt1$flag_avg_wind_speed)
if (class(dt1$avg_wind_dir)=="factor") dt1$avg_wind_dir <-as.numeric(levels(dt1$avg_wind_dir))[as.integer(dt1$avg_wind_dir) ]
if (class(dt1$flag_avg_wind_dir)!="factor") dt1$flag_avg_wind_dir<- as.factor(dt1$flag_avg_wind_dir)
if (class(dt1$avg_chlor)=="factor") dt1$avg_chlor <-as.numeric(levels(dt1$avg_chlor))[as.integer(dt1$avg_chlor) ]
if (class(dt1$flag_chlor)!="factor") dt1$flag_chlor<- as.factor(dt1$flag_chlor)
if (class(dt1$avg_phycocyanin)=="factor") dt1$avg_phycocyanin <-as.numeric(levels(dt1$avg_phycocyanin))[as.integer(dt1$avg_phycocyanin) ]
if (class(dt1$flag_phycocyanin)!="factor") dt1$flag_phycocyanin<- as.factor(dt1$flag_phycocyanin)
if (class(dt1$avg_par)=="factor") dt1$avg_par <-as.numeric(levels(dt1$avg_par))[as.integer(dt1$avg_par) ]
if (class(dt1$flag_par)!="factor") dt1$flag_par<- as.factor(dt1$flag_par)
if (class(dt1$avg_par_below)=="factor") dt1$avg_par_below <-as.numeric(levels(dt1$avg_par_below))[as.integer(dt1$avg_par_below) ]
if (class(dt1$flag_par_below)!="factor") dt1$flag_par_below<- as.factor(dt1$flag_par_below)
if (class(dt1$avg_opt_wtemp)=="factor") dt1$avg_opt_wtemp <-as.numeric(levels(dt1$avg_opt_wtemp))[as.integer(dt1$avg_opt_wtemp) ]
if (class(dt1$flag_avg_opt_wtemp)!="factor") dt1$flag_avg_opt_wtemp<- as.factor(dt1$flag_avg_opt_wtemp)
if (class(dt1$avg_opt_dosat_raw)=="factor") dt1$avg_opt_dosat_raw <-as.numeric(levels(dt1$avg_opt_dosat_raw))[as.integer(dt1$avg_opt_dosat_raw) ]
if (class(dt1$flag_avg_opt_dosat_raw)!="factor") dt1$flag_avg_opt_dosat_raw<- as.factor(dt1$flag_avg_opt_dosat_raw)
if (class(dt1$avg_opt_do_raw)=="factor") dt1$avg_opt_do_raw <-as.numeric(levels(dt1$avg_opt_do_raw))[as.integer(dt1$avg_opt_do_raw) ]
if (class(dt1$flag_avg_opt_do_raw)!="factor") dt1$flag_avg_opt_do_raw<- as.factor(dt1$flag_avg_opt_do_raw)

# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(year4)
summary(sampledate)
summary(month)
summary(daynum)
summary(avg_air_temp)
summary(flag_avg_air_temp)
summary(avg_rel_hum)
summary(flag_avg_rel_hum)
summary(avg_wind_speed)
summary(flag_avg_wind_speed)
summary(avg_wind_dir)
summary(flag_avg_wind_dir)
summary(avg_chlor)
summary(flag_chlor)
summary(avg_phycocyanin)
summary(flag_phycocyanin)
summary(avg_par)
summary(flag_par)
summary(avg_par_below)
summary(flag_par_below)
summary(avg_opt_wtemp)
summary(flag_avg_opt_wtemp)
summary(avg_opt_dosat_raw)
summary(flag_avg_opt_dosat_raw)
summary(avg_opt_do_raw)
summary(flag_avg_opt_do_raw) 
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
                 "avg_air_temp",     
                 "flag_avg_air_temp",     
                 "avg_rel_hum",     
                 "flag_avg_rel_hum",     
                 "avg_wind_speed",     
                 "flag_avg_wind_speed",     
                 "avg_wind_dir",     
                 "flag_avg_wind_dir",     
                 "avg_chlor",     
                 "flag_chlor",     
                 "avg_phycocyanin",     
                 "flag_phycocyanin",     
                 "avg_par",     
                 "flag_par",     
                 "avg_par_below",     
                 "flag_par_below",     
                 "avg_opt_wtemp",     
                 "flag_avg_opt_wtemp",     
                 "avg_opt_dosat_raw",     
                 "flag_avg_opt_dosat_raw",     
                 "avg_opt_do_raw",     
                 "flag_avg_opt_do_raw"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$year4)=="factor") dt2$year4 <-as.numeric(levels(dt2$year4))[as.integer(dt2$year4) ]                                   
# attempting to convert dt2$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
dt2$sampledate<-as.Date(dt2$sampledate,format=tmpDateFormat)
rm(tmpDateFormat) 
if (class(dt2$month)=="factor") dt2$month <-as.numeric(levels(dt2$month))[as.integer(dt2$month) ]
if (class(dt2$daynum)=="factor") dt2$daynum <-as.numeric(levels(dt2$daynum))[as.integer(dt2$daynum) ]
if (class(dt2$avg_air_temp)=="factor") dt2$avg_air_temp <-as.numeric(levels(dt2$avg_air_temp))[as.integer(dt2$avg_air_temp) ]
if (class(dt2$flag_avg_air_temp)!="factor") dt2$flag_avg_air_temp<- as.factor(dt2$flag_avg_air_temp)
if (class(dt2$avg_rel_hum)=="factor") dt2$avg_rel_hum <-as.numeric(levels(dt2$avg_rel_hum))[as.integer(dt2$avg_rel_hum) ]
if (class(dt2$flag_avg_rel_hum)!="factor") dt2$flag_avg_rel_hum<- as.factor(dt2$flag_avg_rel_hum)
if (class(dt2$avg_wind_speed)=="factor") dt2$avg_wind_speed <-as.numeric(levels(dt2$avg_wind_speed))[as.integer(dt2$avg_wind_speed) ]
if (class(dt2$flag_avg_wind_speed)!="factor") dt2$flag_avg_wind_speed<- as.factor(dt2$flag_avg_wind_speed)
if (class(dt2$avg_wind_dir)=="factor") dt2$avg_wind_dir <-as.numeric(levels(dt2$avg_wind_dir))[as.integer(dt2$avg_wind_dir) ]
if (class(dt2$flag_avg_wind_dir)!="factor") dt2$flag_avg_wind_dir<- as.factor(dt2$flag_avg_wind_dir)
if (class(dt2$avg_chlor)=="factor") dt2$avg_chlor <-as.numeric(levels(dt2$avg_chlor))[as.integer(dt2$avg_chlor) ]
if (class(dt2$flag_chlor)!="factor") dt2$flag_chlor<- as.factor(dt2$flag_chlor)
if (class(dt2$avg_phycocyanin)=="factor") dt2$avg_phycocyanin <-as.numeric(levels(dt2$avg_phycocyanin))[as.integer(dt2$avg_phycocyanin) ]
if (class(dt2$flag_phycocyanin)!="factor") dt2$flag_phycocyanin<- as.factor(dt2$flag_phycocyanin)
if (class(dt2$avg_par)=="factor") dt2$avg_par <-as.numeric(levels(dt2$avg_par))[as.integer(dt2$avg_par) ]
if (class(dt2$flag_par)!="factor") dt2$flag_par<- as.factor(dt2$flag_par)
if (class(dt2$avg_par_below)=="factor") dt2$avg_par_below <-as.numeric(levels(dt2$avg_par_below))[as.integer(dt2$avg_par_below) ]
if (class(dt2$flag_par_below)!="factor") dt2$flag_par_below<- as.factor(dt2$flag_par_below)
if (class(dt2$avg_opt_wtemp)=="factor") dt2$avg_opt_wtemp <-as.numeric(levels(dt2$avg_opt_wtemp))[as.integer(dt2$avg_opt_wtemp) ]
if (class(dt2$flag_avg_opt_wtemp)!="factor") dt2$flag_avg_opt_wtemp<- as.factor(dt2$flag_avg_opt_wtemp)
if (class(dt2$avg_opt_dosat_raw)=="factor") dt2$avg_opt_dosat_raw <-as.numeric(levels(dt2$avg_opt_dosat_raw))[as.integer(dt2$avg_opt_dosat_raw) ]
if (class(dt2$flag_avg_opt_dosat_raw)!="factor") dt2$flag_avg_opt_dosat_raw<- as.factor(dt2$flag_avg_opt_dosat_raw)
if (class(dt2$avg_opt_do_raw)=="factor") dt2$avg_opt_do_raw <-as.numeric(levels(dt2$avg_opt_do_raw))[as.integer(dt2$avg_opt_do_raw) ]
if (class(dt2$flag_avg_opt_do_raw)!="factor") dt2$flag_avg_opt_do_raw<- as.factor(dt2$flag_avg_opt_do_raw)

# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(year4)
summary(sampledate)
summary(month)
summary(daynum)
summary(avg_air_temp)
summary(flag_avg_air_temp)
summary(avg_rel_hum)
summary(flag_avg_rel_hum)
summary(avg_wind_speed)
summary(flag_avg_wind_speed)
summary(avg_wind_dir)
summary(flag_avg_wind_dir)
summary(avg_chlor)
summary(flag_chlor)
summary(avg_phycocyanin)
summary(flag_phycocyanin)
summary(avg_par)
summary(flag_par)
summary(avg_par_below)
summary(flag_par_below)
summary(avg_opt_wtemp)
summary(flag_avg_opt_wtemp)
summary(avg_opt_dosat_raw)
summary(flag_avg_opt_dosat_raw)
summary(avg_opt_do_raw)
summary(flag_avg_opt_do_raw) 
detach(dt2)               

# 
# infile3  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/129/12/168e4638bdb8a88990e031605dc11041" 
# infile3 <- sub("^https","http",infile3) 
# dt3 <-read.csv(infile3,header=F 
#                ,skip=1
#                ,sep=","  
#                ,quot='"' 
#                , col.names=c(
#                  "year4",     
#                  "station_id",     
#                  "sampledate",     
#                  "month",     
#                  "daynum",     
#                  "sample_time",     
#                  "air_temp",     
#                  "flag_air_temp",     
#                  "rel_hum",     
#                  "flag_rel_hum",     
#                  "wind_speed",     
#                  "flag_wind_speed",     
#                  "wind_dir",     
#                  "flag_wind_dir",     
#                  "chlor",     
#                  "flag_chlor",     
#                  "phycocyanin",     
#                  "flag_phycocyanin",     
#                  "opt_do_raw",     
#                  "opt_dosat_raw",     
#                  "opt_wtemp",     
#                  "flag_opt_do_raw",     
#                  "flag_opt_dosat_raw",     
#                  "flag_opt_wtemp"    ), check.names=TRUE)
# 
# 
# # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
# 
# if (class(dt3$year4)=="factor") dt3$year4 <-as.numeric(levels(dt3$year4))[as.integer(dt3$year4) ]
# if (class(dt3$station_id)!="factor") dt3$station_id<- as.factor(dt3$station_id)                                   
# # attempting to convert dt3$sampledate dateTime string to R date structure (date or POSIXct)                                
# tmpDateFormat<-"%Y-%m-%d"
# dt3$sampledate<-as.Date(dt3$sampledate,format=tmpDateFormat)
# rm(tmpDateFormat) 
# if (class(dt3$month)=="factor") dt3$month <-as.numeric(levels(dt3$month))[as.integer(dt3$month) ]
# if (class(dt3$daynum)=="factor") dt3$daynum <-as.numeric(levels(dt3$daynum))[as.integer(dt3$daynum) ]
# if (class(dt3$air_temp)=="factor") dt3$air_temp <-as.numeric(levels(dt3$air_temp))[as.integer(dt3$air_temp) ]
# if (class(dt3$flag_air_temp)!="factor") dt3$flag_air_temp<- as.factor(dt3$flag_air_temp)
# if (class(dt3$rel_hum)=="factor") dt3$rel_hum <-as.numeric(levels(dt3$rel_hum))[as.integer(dt3$rel_hum) ]
# if (class(dt3$flag_rel_hum)!="factor") dt3$flag_rel_hum<- as.factor(dt3$flag_rel_hum)
# if (class(dt3$wind_speed)=="factor") dt3$wind_speed <-as.numeric(levels(dt3$wind_speed))[as.integer(dt3$wind_speed) ]
# if (class(dt3$flag_wind_speed)!="factor") dt3$flag_wind_speed<- as.factor(dt3$flag_wind_speed)
# if (class(dt3$wind_dir)=="factor") dt3$wind_dir <-as.numeric(levels(dt3$wind_dir))[as.integer(dt3$wind_dir) ]
# if (class(dt3$flag_wind_dir)!="factor") dt3$flag_wind_dir<- as.factor(dt3$flag_wind_dir)
# if (class(dt3$chlor)=="factor") dt3$chlor <-as.numeric(levels(dt3$chlor))[as.integer(dt3$chlor) ]
# if (class(dt3$flag_chlor)!="factor") dt3$flag_chlor<- as.factor(dt3$flag_chlor)
# if (class(dt3$phycocyanin)=="factor") dt3$phycocyanin <-as.numeric(levels(dt3$phycocyanin))[as.integer(dt3$phycocyanin) ]
# if (class(dt3$flag_phycocyanin)!="factor") dt3$flag_phycocyanin<- as.factor(dt3$flag_phycocyanin)
# if (class(dt3$opt_do_raw)=="factor") dt3$opt_do_raw <-as.numeric(levels(dt3$opt_do_raw))[as.integer(dt3$opt_do_raw) ]
# if (class(dt3$opt_dosat_raw)=="factor") dt3$opt_dosat_raw <-as.numeric(levels(dt3$opt_dosat_raw))[as.integer(dt3$opt_dosat_raw) ]
# if (class(dt3$opt_wtemp)=="factor") dt3$opt_wtemp <-as.numeric(levels(dt3$opt_wtemp))[as.integer(dt3$opt_wtemp) ]
# if (class(dt3$flag_opt_do_raw)!="factor") dt3$flag_opt_do_raw<- as.factor(dt3$flag_opt_do_raw)
# if (class(dt3$flag_opt_dosat_raw)!="factor") dt3$flag_opt_dosat_raw<- as.factor(dt3$flag_opt_dosat_raw)
# if (class(dt3$flag_opt_wtemp)!="factor") dt3$flag_opt_wtemp<- as.factor(dt3$flag_opt_wtemp)
# 
# # Here is the structure of the input data frame:
# str(dt3)                            
# attach(dt3)                            
# # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
# 
# summary(year4)
# summary(station_id)
# summary(sampledate)
# summary(month)
# summary(daynum)
# summary(sample_time)
# summary(air_temp)
# summary(flag_air_temp)
# summary(rel_hum)
# summary(flag_rel_hum)
# summary(wind_speed)
# summary(flag_wind_speed)
# summary(wind_dir)
# summary(flag_wind_dir)
# summary(chlor)
# summary(flag_chlor)
# summary(phycocyanin)
# summary(flag_phycocyanin)
# summary(opt_do_raw)
# summary(opt_dosat_raw)
# summary(opt_wtemp)
# summary(flag_opt_do_raw)
# summary(flag_opt_dosat_raw)
# summary(flag_opt_wtemp) 
# detach(dt3)               
# 
# 

saveRDS(dt2, file="Data/MendotaHourlySurface.rds")
