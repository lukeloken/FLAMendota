# Package ID: knb-lter-ntl.339.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Lake Mendota Carbon and Greenhouse Gas Measurements at North Temperate Lakes LTER 2016.
# Data set creator:  Julia Hart -  
# Data set creator:  Hilary Dugan - University of Wisconsin 
# Data set creator:  Cayelan Carey -  
# Data set creator:  Emily Stanley - University of Wisconsin 
# Data set creator:  Paul Hanson - University of Wisconsin 
# Contact:  Emily Stanley -  University of Wisconsin  - ehstanley@wisc.edu
# Contact:  NTL Information Manager -  University of Wisconsin  - ntl.infomgr@gmail.com
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@lternet.edu 

infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/339/1/d1a582384bf5c2814555e83dfe51c496" 
infile1 <- sub("^https","http",infile1) 
dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "sampledate",     
                 "julian_day",     
                 "sample_site",     
                 "water_depth",     
                 "poc",     
                 "dic",     
                 "doc",     
                 "ch4",     
                 "co2"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
dt1$sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
rm(tmpDateFormat) 
if (class(dt1$julian_day)=="factor") dt1$julian_day <-as.numeric(levels(dt1$julian_day))[as.integer(dt1$julian_day) ]
if (class(dt1$sample_site)!="factor") dt1$sample_site<- as.factor(dt1$sample_site)
if (class(dt1$water_depth)=="factor") dt1$water_depth <-as.numeric(levels(dt1$water_depth))[as.integer(dt1$water_depth) ]
if (class(dt1$poc)=="factor") dt1$poc <-as.numeric(levels(dt1$poc))[as.integer(dt1$poc) ]
if (class(dt1$dic)=="factor") dt1$dic <-as.numeric(levels(dt1$dic))[as.integer(dt1$dic) ]
if (class(dt1$doc)=="factor") dt1$doc <-as.numeric(levels(dt1$doc))[as.integer(dt1$doc) ]
if (class(dt1$ch4)=="factor") dt1$ch4 <-as.numeric(levels(dt1$ch4))[as.integer(dt1$ch4) ]
if (class(dt1$co2)=="factor") dt1$co2 <-as.numeric(levels(dt1$co2))[as.integer(dt1$co2) ]

# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(sampledate)
summary(julian_day)
summary(sample_site)
summary(water_depth)
summary(poc)
summary(dic)
summary(doc)
summary(ch4)
summary(co2) 
detach(dt1)               


infile2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/339/1/90d68e430e1d6a860dd413f00173ff40" 
infile2 <- sub("^https","http",infile2) 
dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "sampledate",     
                 "sampletime",     
                 "temp",     
                 "cond",     
                 "spcond",     
                 "salinity",     
                 "chl_rfu",     
                 "chl_ugl",     
                 "bga_rfu",     
                 "bga_ugl",     
                 "turb",     
                 "fdom_rfu",     
                 "fdom_qsu",     
                 "do_sat",     
                 "do",     
                 "ph",     
                 "orp",     
                 "orp_raw",     
                 "pressure",     
                 "water_depth"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt2$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%m/%d/%y"
dt2$sampledate<-as.Date(dt2$sampledate,format=tmpDateFormat)
rm(tmpDateFormat) 
if (class(dt2$temp)=="factor") dt2$temp <-as.numeric(levels(dt2$temp))[as.integer(dt2$temp) ]
if (class(dt2$cond)=="factor") dt2$cond <-as.numeric(levels(dt2$cond))[as.integer(dt2$cond) ]
if (class(dt2$spcond)=="factor") dt2$spcond <-as.numeric(levels(dt2$spcond))[as.integer(dt2$spcond) ]
if (class(dt2$salinity)=="factor") dt2$salinity <-as.numeric(levels(dt2$salinity))[as.integer(dt2$salinity) ]
if (class(dt2$chl_rfu)=="factor") dt2$chl_rfu <-as.numeric(levels(dt2$chl_rfu))[as.integer(dt2$chl_rfu) ]
if (class(dt2$chl_ugl)=="factor") dt2$chl_ugl <-as.numeric(levels(dt2$chl_ugl))[as.integer(dt2$chl_ugl) ]
if (class(dt2$bga_rfu)=="factor") dt2$bga_rfu <-as.numeric(levels(dt2$bga_rfu))[as.integer(dt2$bga_rfu) ]
if (class(dt2$bga_ugl)=="factor") dt2$bga_ugl <-as.numeric(levels(dt2$bga_ugl))[as.integer(dt2$bga_ugl) ]
if (class(dt2$turb)=="factor") dt2$turb <-as.numeric(levels(dt2$turb))[as.integer(dt2$turb) ]
if (class(dt2$fdom_rfu)=="factor") dt2$fdom_rfu <-as.numeric(levels(dt2$fdom_rfu))[as.integer(dt2$fdom_rfu) ]
if (class(dt2$fdom_qsu)=="factor") dt2$fdom_qsu <-as.numeric(levels(dt2$fdom_qsu))[as.integer(dt2$fdom_qsu) ]
if (class(dt2$do_sat)=="factor") dt2$do_sat <-as.numeric(levels(dt2$do_sat))[as.integer(dt2$do_sat) ]
if (class(dt2$do)=="factor") dt2$do <-as.numeric(levels(dt2$do))[as.integer(dt2$do) ]
if (class(dt2$ph)=="factor") dt2$ph <-as.numeric(levels(dt2$ph))[as.integer(dt2$ph) ]
if (class(dt2$orp)=="factor") dt2$orp <-as.numeric(levels(dt2$orp))[as.integer(dt2$orp) ]
if (class(dt2$orp_raw)=="factor") dt2$orp_raw <-as.numeric(levels(dt2$orp_raw))[as.integer(dt2$orp_raw) ]
if (class(dt2$pressure)=="factor") dt2$pressure <-as.numeric(levels(dt2$pressure))[as.integer(dt2$pressure) ]
if (class(dt2$water_depth)=="factor") dt2$water_depth <-as.numeric(levels(dt2$water_depth))[as.integer(dt2$water_depth) ]

# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(sampledate)
summary(sampletime)
summary(temp)
summary(cond)
summary(spcond)
summary(salinity)
summary(chl_rfu)
summary(chl_ugl)
summary(bga_rfu)
summary(bga_ugl)
summary(turb)
summary(fdom_rfu)
summary(fdom_qsu)
summary(do_sat)
summary(do)
summary(ph)
summary(orp)
summary(orp_raw)
summary(pressure)
summary(water_depth) 
detach(dt2)               


infile3  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/339/1/537aaa14b2e9cdda912a1f2f457b7e55" 
infile3 <- sub("^https","http",infile3) 
dt3 <-read.csv(infile3,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "datetime_deployed",     
                 "datetime_retrieved",     
                 "sample_site",     
                 "rep",     
                 "ebullition_rate"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt3$sample_site)!="factor") dt3$sample_site<- as.factor(dt3$sample_site)
if (class(dt3$rep)!="factor") dt3$rep<- as.factor(dt3$rep)
if (class(dt3$ebullition_rate)=="factor") dt3$ebullition_rate <-as.numeric(levels(dt3$ebullition_rate))[as.integer(dt3$ebullition_rate) ]

# Here is the structure of the input data frame:
str(dt3)                            
attach(dt3)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(datetime_deployed)
summary(datetime_retrieved)
summary(sample_site)
summary(rep)
summary(ebullition_rate) 
detach(dt3)               


infile4  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/339/1/5004134a030d8151978d0fa194e37f6b" 
infile4 <- sub("^https","http",infile4) 
dt4 <-read.csv(infile4,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "sampledate",     
                 "julian_day",     
                 "sample_site",     
                 "water_depth",     
                 "poc",     
                 "dic",     
                 "doc",     
                 "temp",     
                 "do",     
                 "spc",     
                 "ph",     
                 "orp",     
                 "turb",     
                 "chl_rfu",     
                 "bga_rfu",     
                 "discharge"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt4$julian_day)=="factor") dt4$julian_day <-as.numeric(levels(dt4$julian_day))[as.integer(dt4$julian_day) ]
if (class(dt4$sample_site)!="factor") dt4$sample_site<- as.factor(dt4$sample_site)
if (class(dt4$water_depth)=="factor") dt4$water_depth <-as.numeric(levels(dt4$water_depth))[as.integer(dt4$water_depth) ]
if (class(dt4$poc)=="factor") dt4$poc <-as.numeric(levels(dt4$poc))[as.integer(dt4$poc) ]
if (class(dt4$dic)=="factor") dt4$dic <-as.numeric(levels(dt4$dic))[as.integer(dt4$dic) ]
if (class(dt4$doc)=="factor") dt4$doc <-as.numeric(levels(dt4$doc))[as.integer(dt4$doc) ]
if (class(dt4$temp)=="factor") dt4$temp <-as.numeric(levels(dt4$temp))[as.integer(dt4$temp) ]
if (class(dt4$do)=="factor") dt4$do <-as.numeric(levels(dt4$do))[as.integer(dt4$do) ]
if (class(dt4$spc)=="factor") dt4$spc <-as.numeric(levels(dt4$spc))[as.integer(dt4$spc) ]
if (class(dt4$ph)=="factor") dt4$ph <-as.numeric(levels(dt4$ph))[as.integer(dt4$ph) ]
if (class(dt4$orp)=="factor") dt4$orp <-as.numeric(levels(dt4$orp))[as.integer(dt4$orp) ]
if (class(dt4$turb)=="factor") dt4$turb <-as.numeric(levels(dt4$turb))[as.integer(dt4$turb) ]
if (class(dt4$chl_rfu)=="factor") dt4$chl_rfu <-as.numeric(levels(dt4$chl_rfu))[as.integer(dt4$chl_rfu) ]
if (class(dt4$bga_rfu)=="factor") dt4$bga_rfu <-as.numeric(levels(dt4$bga_rfu))[as.integer(dt4$bga_rfu) ]
if (class(dt4$discharge)=="factor") dt4$discharge <-as.numeric(levels(dt4$discharge))[as.integer(dt4$discharge) ]

# Here is the structure of the input data frame:
str(dt4)                            
attach(dt4)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(sampledate)
summary(julian_day)
summary(sample_site)
summary(water_depth)
summary(poc)
summary(dic)
summary(doc)
summary(temp)
summary(do)
summary(spc)
summary(ph)
summary(orp)
summary(turb)
summary(chl_rfu)
summary(bga_rfu)
summary(discharge) 
detach(dt4)               


saveRDS(dt1, file='Data/Hard_dt1.rds')
saveRDS(dt2, file='Data/Hard_dt2.rds')
saveRDS(dt3, file='Data/Hard_dt3.rds')
saveRDS(dt4, file='Data/Hard_dt4.rds')

