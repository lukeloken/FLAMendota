# ###########################################
# Code to do.....
# ##########################################
library(lubridate)
source('R/AddAlpha.R')

# ################
# Flame Buoy Data
# ################

# Get 'buoy' data from flame
Buoy_data<-readRDS('Data/FlameBuoyMeasurements.rds')

# Aggregate multiple buoy samples to a single value
Buoy_daily = aggregate(Buoy_data, by=list(Buoy_data$Date), FUN=mean, na.rm=T)
names(Buoy_daily)[1]<-c('Date')

# ################
# David Buoy Turner CO2 sensor
# ################

DavidBuoy<-read.table('Data/mendotabuoy_co2par2016.txt', sep=",", stringsAsFactors = F)
names(DavidBuoy)<-c('Date', 'Time_UTC', 'CO2_ppm', 'Par_above', 'Par_below')
DavidBuoy$Date<-as.Date(DavidBuoy$Date)
DavidBuoy$DateTime<-as.POSIXct(paste(DavidBuoy$Date, DavidBuoy$Time_UTC), format="%Y-%m-%d %H%M", tz='UTC')

#Calculate mean Daily CO2
DavidBuoyDaily<-aggregate(DavidBuoy[,3:5], by=list(DavidBuoy$Date), FUN='mean', na.rm=T)
names(DavidBuoyDaily)[1]<-'Date'
DavidBuoyDaily$Date<-as.Date(DavidBuoyDaily$Date)

# ##############################
# Lake Flux Data
# ##############################
LakeFlux<-readRDS(file='Data/DailyConcFluxStats.rds')
BuoyFlux<-readRDS(file='Data/DailyBuoyConcFluxStats.rds')

# ################
# Lake wide Flame Data
# ################

# Get table of spatial summaries for all flame runs on Lake Mendota
merged_summary<-readRDS('Data/FlameSpatialSummaries.rds')

# Separate Sensor files so NAs can be removed
# Use one variable to indicate weather or not sensor was working that day
LGR_Only<-merged_summary[!is.na(merged_summary$XCH4Dppm_t),]
YSI_Only<-merged_summary[!is.na(merged_summary$pH),]
NO3_Only<-merged_summary[!is.na(merged_summary$NITRATEM),]

# Split Data based on summary stat (YSIList$Mean == table of mean values)
YSIList<-split(YSI_Only, YSI_Only$Statistic)
LGRList<-split(LGR_Only, LGR_Only$Statistic)
NO3List<-split(NO3_Only, NO3_Only$Statistic)

# Look at any single variable
# plot(YSIList$Mean$Date, YSIList$Mean$TempC, type="b")
# plot(YSIList$Mean$Date, YSIList$Mean$NITRATEM, type="b")
# plot(YSIList$Mean$Date, YSIList$Mean$XCO2Dppm_t, type="b")
# plot(YSIList$Mean$Date, YSIList$Mean$CO2St_t, type="b")
# plot(YSIList$Mean$Date, YSIList$Mean$XCH4Dppm_t, type="b")
# plot(YSIList$Mean$Date, YSIList$Mean$CH4St_t, type="b")
# plot(YSIList$Mean$Date, YSIList$Mean$BGAPCRFU_t, type="b")
# plot(YSIList$Mean$Date, YSIList$Mean$ChlARFU_t, type="b")
# plot(YSIList$Mean$Date, YSIList$Mean$fDOMRFU_t, type="b")
# plot(YSIList$Mean$Date, YSIList$Mean$SPCScm_t, type="b")
# plot(YSIList$Mean$Date, YSIList$Mean$ODOmgL, type="b")


# ########################
# Visualize Data
# ########################

# Methane moving boxplots
# Code plots a polygon of IQR across entire lake surface, mean, median, and buoy

png('Figures/Methane2016.png', width=8, height=4, units='in', res=200, bg='white')
par(pch=16)
par(ps=12)
par(mfrow=c(1,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)

xticks<-seq(ceiling_date(min(LGRList$Mean$Date), "months"),floor_date(max(LGRList$Mean$Date), "months"), by='months')
xlabels<-paste(month(xticks, label=TRUE, abbr=T), " 1", sep="")

ch4_ylim<-range(c(LGRList$Mean$CH4St_t, LGRList$Q5$CH4St_t, LGRList$Q95$CH4St_t, Buoy_daily$ CH4Sat), na.rm=T)
colors<-c('red', 'black', 'grey85', 'orangered2', 'grey55')
buoypch=20

plot(LGRList$Mean$Date, LGRList$Mean$CH4St_t/100, type="n", pch=15, ylim=ch4_ylim/100, ylab="", xlab="", xaxt="n")
axis(1, at=xticks, labels=xlabels)

mtext(expression(paste(CH[4], " (Sat ratio)", sep="")), 2, 2)
mtext('2016', 1, 1.5)
abline(h=1, lty=3)

# Polygon of 5-95%
polyx90<-c(LGRList$Q05$Date, rev(LGRList$Q95$Date))
polyy90<-c(LGRList$Q05$CH4St_t, rev(LGRList$Q95$CH4St_t))
polygon(polyx90, polyy90/100, border=colors[3], col=colors[3])

# Polygon of IQR
polyx<-c(LGRList$Q25$Date, rev(LGRList$Q75$Date))
polyy<-c(LGRList$Q25$CH4St_t, rev(LGRList$Q75$CH4St_t))
polygon(polyx, polyy/100, border=colors[5], col=colors[5])

# Mean and Median
points(LGRList$Median$Date, LGRList$Median$CH4St_t/100, type="l", col=colors[2], lwd=2)
# points(LGRList$Mean$Date, LGRList$Mean$CH4St_t/100, type="l", col=colors[1], lwd=2)

#Buoy
points(Buoy_daily$Date, Buoy_daily$CH4Sat/100, type="o", col=colors[4], pch=buoypch, cex=1.5, lty=2, lwd=1.5)

usr<-par('usr')
yscale<-diff(usr[3:4])/10
box.y<-usr[4]-yscale*seq(0.5,4, length.out=7)

xscale<-diff(usr[1:2])/30
box.x<-usr[1]+xscale*seq(0.7,2, length.out=2)

polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[5],2), rep(box.y[1],2)), col=colors[3], border=colors[3], lwd=2)
polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[4],2), rep(box.y[2],2)), col=colors[5], border=colors[5], lwd=2)
polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[3],2), rep(box.y[3],2)), col=colors[2], border=colors[2], lwd=2)

lines(x=box.x, y=rep(box.y[6], 2), lty=2, col=colors[4], type="l", pch=16, lwd=1.5)
lines(x=box.x, y=rep(box.y[7], 2), lty=3, type="l")
points(x=mean(box.x), y=box.y[6], col=colors[4], type="p", pch=buoypch, cex=1.5)

text(x=box.x[2], y=box.y, c(expression(paste(Q[95])), expression(paste(Q[75])), expression(paste(Q[50])), expression(paste(Q[25])), expression(paste(Q[5])), 'Buoy', "Atm"), pos=4)

box(which='plot')

dev.off()





# Carbon Dioxide moving boxplots
# Code plots a polygon of IQR across entire lake surface, mean, median, and buoy

png('Figures/CarbonDioxide2016.png', width=8, height=4, units='in', res=200, bg='white')
par(pch=16)
par(ps=12)
par(mfrow=c(1,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)

xticks<-seq(ceiling_date(min(LGRList$Mean$Date), "months"),floor_date(max(LGRList$Mean$Date), "months"), by='months')
xlabels<-paste(month(xticks, label=TRUE, abbr=T), " 1", sep="")
buoypch<-20

co2_ylim<-range(c(LGRList$Mean$CO2St_t, LGRList$Q5$CO2St_t, LGRList$Q75$CO2St_t, Buoy_daily$CO2Sat), na.rm=T)
# co2_ylim[2]<-150

plot(LGRList$Mean$Date, LGRList$Mean$CO2St_t/100, type="n", pch=15, ylim=co2_ylim/100, ylab="", xlab="", xaxt="n")
axis(1, at=xticks, labels=xlabels)

mtext(expression(paste(CO[2], " (Sat ratio)", sep="")), 2, 2)
mtext('2016', 1, 1.5)
abline(h=1, lty=3)

# Polygon of 5-95%
polyx90<-c(LGRList$Q05$Date, rev(LGRList$Q95$Date))
polyy90<-c(LGRList$Q05$CO2St_t, rev(LGRList$Q95$CO2St_t))
polygon(polyx90, polyy90/100, border=colors[3], col=colors[3])

# Polygon of IQR
polyx<-c(LGRList$Q25$Date, rev(LGRList$Q75$Date))
polyy<-c(LGRList$Q25$CO2St_t, rev(LGRList$Q75$CO2St_t))
polygon(polyx, polyy/100, border=colors[5], col=colors[5])

# Mean and Median
points(LGRList$Median$Date, LGRList$Median$CO2St_t/100, type="l", col=colors[2], lwd=2)
# points(LGRList$Mean$Date, LGRList$Mean$CO2St_t/100, type="l", col=colors[1], lwd=2)

#Buoy
points(Buoy_daily$Date, Buoy_daily$CO2Sat/100, type="o", col=colors[4], pch=buoypch, cex=1.5, lty=2, lwd=1.5)

usr<-par('usr')
yscale<-diff(usr[3:4])/10
box.y<-usr[4]-yscale*seq(0.5,4, length.out=7)

xscale<-diff(usr[1:2])/30
box.x<-usr[1]+xscale*seq(0.7,2, length.out=2)

polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[5],2), rep(box.y[1],2)), col=colors[3], border=colors[3], lwd=2)
polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[4],2), rep(box.y[2],2)), col=colors[5], border=colors[5], lwd=2)
polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[3],2), rep(box.y[3],2)), col=colors[2], border=colors[2], lwd=2)

lines(x=box.x, y=rep(box.y[6], 2), lty=2, col=colors[4], type="l", pch=16, lwd=1.5)
lines(x=box.x, y=rep(box.y[7], 2), lty=3, type="l")
points(x=mean(box.x), y=box.y[6], col=colors[4], type="p", pch=buoypch, cex=1.5)

text(x=box.x[2], y=box.y, c(expression(paste(Q[95])),  expression(paste(Q[75])), expression(paste(Q[50])), expression(paste(Q[25])), expression(paste(Q[5])), 'Buoy', "Atm"), pos=4)

box(which='plot')

dev.off()



# Other units
# Carbon Dioxide (ppm)
# Code plots a polygon of IQR across entire lake surface, mean, median, and buoy

png('Figures/CarbonDioxidePPM2016.png', width=8, height=4, units='in', res=200, bg='white')
par(pch=16)
par(ps=12)
par(mfrow=c(1,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)

xticks<-seq(ceiling_date(min(LGRList$Mean$Date), "months"),floor_date(max(LGRList$Mean$Date), "months"), by='months')
xlabels<-paste(month(xticks, label=TRUE, abbr=T), " 1", sep="")
turnercolors<-c('aquamarine2', 'aquamarine4')

co2_ylim<-range(abs(c(LGRList$Mean$XCO2Dppm_t, LGRList$Q05$XCO2Dppm_t, LGRList$Q95$XCO2Dppm_t, Buoy_daily$XCO2Dppm_tau, DavidBuoyDaily$CO2_ppm, 0)), na.rm=T)
# co2_ylim[2]<-150

plot(LGRList$Mean$Date, LGRList$Mean$XCO2Dppm_t, type="n", pch=15, ylim=co2_ylim, ylab="", xlab="", xaxt="n")
axis(1, at=xticks, labels=xlabels)

mtext(expression(paste(CO[2], " (ppm)", sep="")), 2, 2)
mtext('2016', 1, 1.5)
abline(h=400, lty=3)

# Polygon of 5-95%
polyx90<-c(LGRList$Q05$Date, rev(LGRList$Q95$Date))
polyy90<-c(LGRList$Q05$XCO2Dppm_t, rev(LGRList$Q95$XCO2Dppm_t))
polygon(polyx90, polyy90, border=colors[3], col=colors[3])

# Polygon of IQR
polyx<-c(LGRList$Q25$Date, rev(LGRList$Q75$Date))
polyy<-c(LGRList$Q25$XCO2Dppm_t, rev(LGRList$Q75$XCO2Dppm_t))
polygon(polyx, polyy, border=colors[5], col=colors[5])

# Mean and Median
points(LGRList$Median$Date, LGRList$Median$XCO2Dppm_t, type="l", col=colors[2], lwd=2)
# points(LGRList$Mean$Date, LGRList$Mean$XCO2Dppm_t, type="l", col=colors[1], lwd=2)

#Buoy
points(Buoy_daily$Date, Buoy_daily$XCO2Dppm_tau, type="o", col=colors[4], pch=16, cex=1, lty=2)

# legend('topleft', inset=0.01, c('Mean', 'Median', 'IQR', 'Flame at buoy', 'Daily mean buoy'), col=c(colors[1:4], 'black'), lty=c(1,1,1,2, 1), pch=c(-1,-1,-1,16, -1), lwd=c(2,2,15,1, 1), pt.cex=c(1,1,1,1, 1), bty="n")

points(DavidBuoy$Date, DavidBuoy$CO2_ppm, type="p", pch=16, cex=.1, col=turnercolors[1])
points(DavidBuoyDaily$Date, DavidBuoyDaily$CO2_ppm, type="l", lwd=1, col=turnercolors[2])



usr<-par('usr')
yscale<-diff(usr[3:4])/10
box.y<-usr[4]-yscale*seq(0.5,5, length.out=8)

xscale<-diff(usr[1:2])/30
box.x<-usr[1]+xscale*seq(0.7,2, length.out=2)

polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[5],2), rep(box.y[1],2)), col=colors[3], border=colors[3], lwd=2)
polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[4],2), rep(box.y[2],2)), col=colors[5], border=colors[5], lwd=2)
polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[3],2), rep(box.y[3],2)), col=colors[2], border=colors[2], lwd=2)

lines(x=box.x, y=rep(box.y[6], 2), lty=2, col=colors[4], type="l", pch=16, lwd=1.5)
points(x=rep(mean(box.x),10), y=box.y[7]+yscale*0.2*seq(-1,1, length.out = 10), type='p', col=turnercolors[1], pch=16, cex=.1)
lines(x=box.x, y=rep(box.y[7], 2), lty=1, type="l", col=turnercolors[2])

points(x=mean(box.x), y=box.y[6], col=colors[4], type="p", pch=buoypch, cex=1.5)

lines(x=box.x, y=rep(box.y[8], 2), lty=3, type="l")

text(x=box.x[2], y=box.y, c(expression(paste(Q[95])),  expression(paste(Q[75])), expression(paste(Q[50])), expression(paste(Q[25])), expression(paste(Q[5])), 'FLAMe at buoy', "Turner buoy sensor", "Atm"), pos=4)

box(which='plot')

dev.off()

# ############################
# Plot Flux Data through time
# ############################

#CO2

png('Figures/CarbonDioxideFlux2016.png', width=8, height=4, units='in', res=200, bg='white')
par(pch=16)
par(ps=12)
par(mfrow=c(1,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)


xticks<-seq(ceiling_date(min(LakeFlux$date_names), "months"),floor_date(max(LakeFlux$date_names), "months"), by='months')
xlabels<-paste(month(xticks, label=TRUE, abbr=T), " 1", sep="")


co2flux_ylim<-range(c(LakeFlux$CO2_Flux_Mean, LakeFlux$CO2_Flux_5., LakeFlux$CO2_Flux_95.), na.rm=T)
# co2_ylim[2]<-150


plot(LakeFlux$date_names, LakeFlux$CO2_Flux_Mean,  type="n", pch=15, ylim=co2flux_ylim, ylab="", xlab="", xaxt="n")
axis(1, at=xticks, labels=xlabels)

mtext(expression(paste(CO[2], " efflux (mmol m"^"-2", " d"^"-1", ")", sep="")), 2, 2)
mtext('2016', 1, 1.5)
abline(h=0, lty=3)

# Polygon of 5-95%
polyx<-c(LakeFlux$date_names, rev(LakeFlux$date_names))
polyy90<-c(LakeFlux$CO2_Flux_5., rev(LakeFlux$CO2_Flux_95.))
polygon(polyx, polyy90, border=colors[3], col=colors[3])

# Polygon of IQR
polyy<-c(LakeFlux$CO2_Flux_25., rev(LakeFlux$CO2_Flux_75.))
polygon(polyx, polyy, border=colors[5], col=colors[5])

# Mean and Median
points(LakeFlux$date_names, LakeFlux$CO2_Flux_50., type="l", col=colors[2], lwd=2)
# points(LGRList$Mean$Date, LGRList$Mean$XCO2Dppm_t, type="l", col=colors[1], lwd=2)

#Buoy
# points(Buoy_daily$Date, Buoy_daily$XCO2Dppm_tau, type="o", col=colors[4], pch=16, cex=1, lty=2)
points(BuoyFlux$date_names, BuoyFlux$CO2Buoy_Flux_Mean,  type="l", col=colors[4], pch=16, cex=1, lty=1, lwd=1.5)

#Plot legend
usr<-par('usr')
yscale<-diff(usr[3:4])/10
box.y<-usr[4]-yscale*seq(0.5,5, length.out=7)

xscale<-diff(usr[1:2])/30
box.x<-usr[1]+xscale*seq(0.7,2, length.out=2)

polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[5],2), rep(box.y[1],2)), col=colors[3], border=colors[3], lwd=2)
polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[4],2), rep(box.y[2],2)), col=colors[5], border=colors[5], lwd=2)
polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[3],2), rep(box.y[3],2)), col=colors[2], border=colors[2], lwd=2)

#Buoy legend
lines(x=box.x, y=rep(box.y[6], 2), lty=1, col=colors[4], type="l", pch=16, lwd=1.5)
# points(x=mean(box.x), y=box.y[6], col=colors[4], type="p", pch=buoypch, cex=1.5)

#Atm legend
lines(x=box.x, y=rep(box.y[7], 2), lty=3, type="l")

# legend text
text(x=box.x[2], y=box.y, c(expression(paste(Q[95])),  expression(paste(Q[75])), expression(paste(Q[50])), expression(paste(Q[25])), expression(paste(Q[5])), 'Buoy', "Atm"), pos=4)

box(which='plot')

dev.off()

#CH4

png('Figures/MethaneFlux2016.png', width=8, height=4, units='in', res=200, bg='white')
par(pch=16)
par(ps=12)
par(mfrow=c(1,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)


xticks<-seq(ceiling_date(min(LakeFlux$date_names), "months"),floor_date(max(LakeFlux$date_names), "months"), by='months')
xlabels<-paste(month(xticks, label=TRUE, abbr=T), " 1", sep="")


ch4flux_ylim<-range(c(LakeFlux$CH4_Flux_Mean, LakeFlux$CH4_Flux_5., LakeFlux$CH4_Flux_95.), na.rm=T)
# co2_ylim[2]<-150


plot(LakeFlux$date_names, LakeFlux$CH4_Flux_Mean,  type="n", pch=15, ylim=ch4flux_ylim, ylab="", xlab="", xaxt="n")
axis(1, at=xticks, labels=xlabels)

mtext(expression(paste(CH[4], " efflux (mmol m"^"-2", " d"^"-1", ")", sep="")), 2, 2)
mtext('2016', 1, 1.5)
abline(h=0, lty=3)

# Polygon of 5-95%
polyx<-c(LakeFlux$date_names, rev(LakeFlux$date_names))
polyy90<-c(LakeFlux$CH4_Flux_5., rev(LakeFlux$CH4_Flux_95.))
polygon(polyx, polyy90, border=colors[3], col=colors[3])

# Polygon of IQR
polyy<-c(LakeFlux$CH4_Flux_25., rev(LakeFlux$CH4_Flux_75.))
polygon(polyx, polyy, border=colors[5], col=colors[5])

# Mean and Median
points(LakeFlux$date_names, LakeFlux$CH4_Flux_50., type="l", col=colors[2], lwd=2)
# points(LGRList$Mean$Date, LGRList$Mean$XCO2Dppm_t, type="l", col=colors[1], lwd=2)

#Buoy
# points(Buoy_daily$Date, Buoy_daily$XCO2Dppm_tau, type="o", col=colors[4], pch=16, cex=1, lty=2)
points(BuoyFlux$date_names, BuoyFlux$CH4Buoy_Flux_Mean,  type="l", col=colors[4], pch=16, cex=1, lty=1, lwd=1.5)

#Plot legend
usr<-par('usr')
yscale<-diff(usr[3:4])/10
box.y<-usr[4]-yscale*seq(0.5,5, length.out=7)

xscale<-diff(usr[1:2])/30
box.x<-usr[1]+xscale*seq(0.7,2, length.out=2)

polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[5],2), rep(box.y[1],2)), col=colors[3], border=colors[3], lwd=2)
polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[4],2), rep(box.y[2],2)), col=colors[5], border=colors[5], lwd=2)
polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[3],2), rep(box.y[3],2)), col=colors[2], border=colors[2], lwd=2)

#Buoy legend
lines(x=box.x, y=rep(box.y[6], 2), lty=1, col=colors[4], type="l", pch=16, lwd=1.5)
# points(x=mean(box.x), y=box.y[6], col=colors[4], type="p", pch=buoypch, cex=1.5)

#Atm legend
lines(x=box.x, y=rep(box.y[7], 2), lty=3, type="l")

# legend text
text(x=box.x[2], y=box.y, c(expression(paste(Q[95])),  expression(paste(Q[75])), expression(paste(Q[50])), expression(paste(Q[25])), expression(paste(Q[5])), 'Buoy', "Atm"), pos=4)

box(which='plot')

dev.off()



# ##########################################################
# Linear interpolate between points and calculate concentration for each day
# output is a dataframe of predicted CO2/CH4 for each day for entire summer (flame sampling window)
# ##########################################################


Buoy_daily$Jday<-yday(Buoy_daily$Date)

xo<-c(seq(min(Buoy_daily$Jday, na.rm=T), max(Buoy_daily$Jday, na.rm=T), by=1))
inter_CH4<-approx(Buoy_daily$Jday, Buoy_daily$CH4Sat, xout=xo, method='linear')
plot(Buoy_daily$Jday, Buoy_daily$CH4Sat)
lines(inter_CH4, col="red", type="b", cex=0.5)

Buoy_inter_CH4<-as.data.frame(inter_CH4)
names(Buoy_inter_CH4)<-c('Jday', 'CH4Sat_Buoy')

inter_CO2<-approx(Buoy_daily$Jday, Buoy_daily$CO2Sat, xout=xo, method='linear')
plot(Buoy_daily$Jday, Buoy_daily$CO2Sat)
lines(inter_CO2, col="red", type="b", cex=0.5)

Buoy_inter_CO2<-as.data.frame(inter_CO2)
names(Buoy_inter_CO2)<-c('Jday', 'CO2Sat_Buoy')

Buoy_inter_merge<-merge(Buoy_inter_CH4, Buoy_inter_CO2, by='Jday', all=T)


#Flame Mean
LGRList$Mean$Jday<-yday(LGRList$Mean$Date)

xo_F<-c(seq(min(LGRList$Mean$Jday, na.rm=T), max(LGRList$Mean$Jday, na.rm=T), by=1))

inter_CH4_F<-approx(LGRList$Mean$Jday, LGRList$Mean$CH4St_t, xout=xo, method='linear')
plot(LGRList$Mean$Jday, LGRList$Mean$CH4St_t)
lines(inter_CH4_F, col="red", type="b", cex=0.5)

Flame_inter_CH4<-as.data.frame(inter_CH4_F)
names(Flame_inter_CH4)<-c('Jday', 'CH4Sat_Flame')

inter_CO2_F<-approx(LGRList$Mean$Jday, LGRList$Mean$CO2St_t, xout=xo, method='linear')
plot(LGRList$Mean$Jday, LGRList$Mean$CO2St_t)
lines(inter_CO2_F, col="red", type="b", cex=0.5)

Flame_inter_CO2<-as.data.frame(inter_CO2_F)
names(Flame_inter_CO2)<-c('Jday', 'CO2Sat_Flame')

Flame_inter_merge<-merge(Flame_inter_CH4, Flame_inter_CO2, by='Jday', all=T)


All_inter_merge<-merge(Flame_inter_merge, Buoy_inter_merge, by='Jday', all=T)

All_inter_merge$Date<-as.Date(paste('2016-', All_inter_merge$Jday, sep=""), format="%Y-%j")

plot(All_inter_merge$Jday, All_inter_merge$CH4Sat_Flame-All_inter_merge$CH4Sat_Buoy)
mean(All_inter_merge$CH4Sat_Flame-All_inter_merge$CH4Sat_Buoy)


sum(All_inter_merge$CH4Sat_Flame-All_inter_merge$CH4Sat_Buoy)

mean(All_inter_merge$CH4Sat_Flame-All_inter_merge$CH4Sat_Buoy)

mean(All_inter_merge$CH4Sat_Flame-All_inter_merge$CH4Sat_Buoy)/100
mean(All_inter_merge$CO2Sat_Flame-All_inter_merge$CO2Sat_Buoy)/100

All_inter_merge$FCH4cum<-NA
All_inter_merge$BCH4cum<-NA
All_inter_merge$FCO2cum[row]<-NA
All_inter_merge$BCO2cum[row]<-NA
row<-1
for (row in 1:nrow(All_inter_merge)){
  All_inter_merge$FCH4cum[row]<-sum(All_inter_merge$CH4Sat_Flame[1:row])
  All_inter_merge$BCH4cum[row]<-sum(All_inter_merge$CH4Sat_Buoy[1:row])
  
  All_inter_merge$FCO2cum[row]<-sum(All_inter_merge$CO2Sat_Flame[1:row])
  All_inter_merge$BCO2cum[row]<-sum(All_inter_merge$CO2Sat_Buoy[1:row])
  
}


# Cumulative carbon concentration plots
# 

png('Figures/CumulativeCO2CH4.png', width=3.5, height=6, units='in', res=200, bg='white')
par(pch=16)
par(ps=12)
par(mfrow=c(2,1))
par(mar = c(1,4.5,1.5,0.5),mgp=c(2.5,0.4,0),tck=-0.02)
par(oma=c(2,0,0,0))
par(lend=2)
lwd=c(1,2,2)
lty=c(1,2,3)
colors<-add.alpha(c('royalblue4', 'red2'), alpha=0.9)
colors[3]<-'darkgrey'

xticks<-seq(ceiling_date(min(All_inter_merge$Date), "months"),floor_date(max(All_inter_merge$Date), "months"), by='months')
xlabels<-paste(month(xticks, label=TRUE, abbr=T), " 1", sep="")


#CO2
plot(All_inter_merge$Date, All_inter_merge$FCO2cum/100, type="l", axes=F, ylab="", xlab="", col=colors[1], lty=lty[1], lwd=lwd[1])
points(All_inter_merge$Date, All_inter_merge$BCO2cum/100, type="l", col=colors[1], lty=lty[2], lwd=lwd[2])
mtext(expression(paste('Cumulative ', CO[2], ' (sat ratio X day)', sep="")), 2, 3)
axis(2, las=1)
axis(1, at=xticks, labels=NA)
box(which='plot')
lines(x=range(All_inter_merge$Date), y=c(1, length(All_inter_merge$Date)), lty=lty[3], col=colors[3])

#CH4
plot(All_inter_merge$Date, All_inter_merge$FCH4cum/100, type="l", axes=F, ylab="", xlab="", col=colors[2], lty=lty[1], lwd=lwd[1])
points(All_inter_merge$Date, All_inter_merge$BCH4cum/100, type="l", col=colors[2], lty=lty[2], lwd=lwd[2])
mtext(expression(paste('Cumulative ', CH[4], ' (sat ratio X day)', sep="")), 2, 3)
axis(2, las=1)
axis(1, at=xticks, labels=xlabels)
box(which='plot')
lines(x=range(All_inter_merge$Date), y=c(1, length(All_inter_merge$Date)), lty=lty[3], col=colors[3])


legend('topleft', c('Lake-wide mean', 'Buoy'), col='black', lty=lty, bty="n", lwd=lwd)

mtext('Date', 1, 1.5)

dev.off()


# ##########################################################
# Scatterplots buoy vs flame mean
# ##########################################################

png('Figures/ScatterplotBuoyFlameCO2CH4.png', width=4, height=7, units='in', res=200, bg='white')
par(pch=16)
par(ps=12)
par(mfrow=c(2,1))
par(mar = c(2,3.5,1.5,0.5),mgp=c(2.5,0.4,0),tck=-0.02)
par(oma=c(1,0,0,0))
par(lend=2)
par(lwd=2)
lty=c(1,2)
pt.cex=1.5

color<-add.alpha(c('royalblue4', 'red2'), alpha=0.25)
co2lim<-range(c(All_inter_merge$CO2Sat_Buoy/100, All_inter_merge$CO2Sat_Flame/100), na.rm=T)
ch4lim<-range(c(All_inter_merge$CH4Sat_Buoy/100, All_inter_merge$CH4Sat_Flame/100), na.rm=T)

plot(All_inter_merge$CO2Sat_Buoy/100, All_inter_merge$CO2Sat_Flame/100, pch=16, ylab="", xlab="", las=1, ylim=co2lim, xlim=co2lim, col=color[1], cex=pt.cex)
box(which='plot')
mtext(expression(paste('Lake-wide mean  ', CO[2], ' (sat ratio)', sep="")), 2, 2)
mtext(expression(paste('Buoy ', CO[2], ' (sat ratio)', sep="")), 1, 1.5)
abline(0,1, lty=3, col="black")

plot(All_inter_merge$CH4Sat_Buoy/100, All_inter_merge$CH4Sat_Flame/100, pch=16, ylab="", xlab="", las=1, ylim=ch4lim, xlim=ch4lim, col=color[2], cex=pt.cex)
box(which='plot')
abline(0,1, lty=3, col="black")
mtext(expression(paste('Lake-wide mean  ', CH[4], ' (sat ratio)', sep="")), 2, 2)
mtext(expression(paste('Buoy ', CH[4], ' (sat ratio)', sep="")), 1, 1.5)

dev.off()

