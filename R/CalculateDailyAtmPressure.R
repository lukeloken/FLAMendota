library(lubridate)

AOS<-read.csv('Data/HourlyAOSMet.csv', header=T, stringsAsFactors = F)

AOS$DateTime<-as.POSIXct(paste(AOS$YYYY.MM.DD, AOS$hh.mm.ss, sep=' '), tz='UTC')
AOS$DateTime<-round_date(AOS$DateTime, unit = "hour")

head(AOS)


AOS$pressureME<-AOS$pressure((AOS$air_temp+273.15)/((AOS$air_temp+273.15)+9.80665*0.0289644*(257-331.5))/(8.3144598*(AOS$air_temp+273.15)))


AOS$pressureME<-AOS$pressure*exp((-9.80665*0.0289644*(257-331.5))/(8.3144598*(AOS$air_temp+273.15)))


-0.0065

plot(AOS$DateTime, AOS$pressureME, type='l', lwd=2)
points(AOS$DateTime, AOS$pressure, type='l', col='blue')
hist(AOS$pressureME)
summary(AOS$pressureME)

abline(v=982.754721375, col='red', lwd=2)
abline(v=mean(AOS$pressureME, na.rm=T), col='blue', lwd=2)
