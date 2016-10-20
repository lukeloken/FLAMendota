# Download Buoy Data and generate Rdata file

# ====================================
# Copy and Paste from LTER
# ====================================

# Package ID: knb-lter-ntl.130.11 Cataloging System:https://pasta.lternet.edu.
# Data set title: North Temperate Lakes LTER: High Frequency Water Temperature Data - Lake Mendota Buoy 2006 - current.
# Data set creator:  NTL Lead PI - University of Wisconsin 
# Data set creator:  John Magnuson - University of Wisconsin 
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Emily Stanley - University of Wisconsin 
# Metadata Provider:  NTL Information Manager - University of Wisconsin 
# Contact:    - Information Manager LTER Network Office  - tech-support@lternet.edu
# Contact:  NTL Information Manager -  University of Wisconsin  - infomgr@lter.limnology.wisc.edu
# Contact:  NTL Lead PI -  University of Wisconsin  - leadpi@lter.limnology.wisc.edu
# Metadata Link: https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-ntl.130.11
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@lternet.edu 
GetBuoyTemp<-function(startdate, enddate, type){

  if (type=="Daily"){
  
    infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/130/11/a5919fc36f07fcf99765f084d18f5174" 
    infile1 <- sub("^https","http",infile1) 
    
    
    dt1 <-read.csv(infile1,header=F 
                   ,skip=1
                   ,sep=","  
                   ,quot='"' 
                   , col.names=c(
                     "sampledate",     
                     "year4",     
                     "month",     
                     "daynum",     
                     "depth",     
                     "wtemp",     
                     "flag.wtemp"    ), check.names=TRUE)
    
    
    # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
    
    # attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
    tmpDateFormat<-"%Y-%m-%d"
    dt1$sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
    rm(tmpDateFormat) 
    if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]
    if (class(dt1$month)=="factor") dt1$month <-as.numeric(levels(dt1$month))[as.integer(dt1$month) ]
    if (class(dt1$daynum)=="factor") dt1$daynum <-as.numeric(levels(dt1$daynum))[as.integer(dt1$daynum) ]
    if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]
    if (class(dt1$wtemp)=="factor") dt1$wtemp <-as.numeric(levels(dt1$wtemp))[as.integer(dt1$wtemp) ]
    if (class(dt1$flag.wtemp)!="factor") dt1$flag.wtemp<- as.factor(dt1$flag.wtemp)
    
    # Here is the structure of the input data frame:
    str(dt1)                            
    attach(dt1)                            
    # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
    
    summary(sampledate)
    summary(year4)
    summary(month)
    summary(daynum)
    summary(depth)
    summary(wtemp)
    summary(flag.wtemp) 
    detach(dt1)               

  }
  
  if (type=="Hourly"){


    infile2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/130/11/63d0587cf326e83f57b054bf2ad0f7fe" 
    infile2 <- sub("^https","http",infile2) 
    dt2 <-read.csv(infile2,header=F 
                   ,skip=1
                   ,sep=","  
                   ,quot='"' 
                   , col.names=c(
                     "sampledate",     
                     "year4",     
                     "month",     
                     "daynum",     
                     "hour",     
                     "depth",     
                     "wtemp",     
                     "flag.wtemp"    ), check.names=TRUE)
    
    
    # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
    
    # attempting to convert dt2$sampledate dateTime string to R date structure (date or POSIXct)                                
    tmpDateFormat<-"%Y-%m-%d"
    dt2$sampledate<-as.Date(dt2$sampledate,format=tmpDateFormat)
    rm(tmpDateFormat) 
    if (class(dt2$year4)=="factor") dt2$year4 <-as.numeric(levels(dt2$year4))[as.integer(dt2$year4) ]
    if (class(dt2$month)=="factor") dt2$month <-as.numeric(levels(dt2$month))[as.integer(dt2$month) ]
    if (class(dt2$daynum)=="factor") dt2$daynum <-as.numeric(levels(dt2$daynum))[as.integer(dt2$daynum) ]
    if (class(dt2$depth)=="factor") dt2$depth <-as.numeric(levels(dt2$depth))[as.integer(dt2$depth) ]
    if (class(dt2$wtemp)=="factor") dt2$wtemp <-as.numeric(levels(dt2$wtemp))[as.integer(dt2$wtemp) ]
    if (class(dt2$flag.wtemp)!="factor") dt2$flag.wtemp<- as.factor(dt2$flag.wtemp)
    
    # Here is the structure of the input data frame:
    str(dt2)                            
    attach(dt2)                            
    # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
    
    summary(sampledate)
    summary(year4)
    summary(month)
    summary(daynum)
    summary(hour)
    summary(depth)
    summary(wtemp)
    summary(flag.wtemp) 
    detach(dt2)
    
    dt1<-dt2
    rm(dt2)
  }
  

# ====================================
# End Copy and Paste from LTER
# ====================================

  date1<-as.Date(startdate)
  date2<-as.Date(enddate)
  dt1<-dt1[dt1$sampledate>date1  & dt1$sampledate<date2,]
  
  return(dt1)
}

# Function testing
# Do not run ###########################
# startdate<-"2015-01-31"
# enddate<-"2015-12-31"
# type="Hourly"
# test<-GetBuoyTemp(startdate=startdate, enddate=enddate, type=type)
