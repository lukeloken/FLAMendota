# ###########################################
# Code to do.....
# ##########################################

# ################
# Buoy Data
# ################

# Get 'buoy' data from flame
Buoy_data<-readRDS('Data/FlameBuoyMeasurements.rds')

# Aggregate multiple buoy samples to a single value
Buoy_daily = aggregate(Buoy_data, by=list(Buoy_data$Date), FUN=mean, na.rm=T)
names(Buoy_daily)[1]<-c('Date')


# ################
# Lake wide Data
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
xlabels<-month(xticks, label=TRUE, abbr=T)

ch4_ylim<-range(c(LGRList$Mean$CH4St_t, LGRList$Q1$CH4St_t, LGRList$Q3$CH4St_t, Buoy_daily$ CH4Sat), na.rm=T)
colors<-c('red', 'black', 'grey', 'mediumblue', 'darkgrey')

plot(LGRList$Mean$Date, LGRList$Mean$CH4St_t/100, type="n", pch=15, ylim=ch4_ylim/100, ylab="", xlab="", xaxt="n")
axis(1, at=xticks, labels=NA)
axis(1, at=xticks+15, labels=xlabels, tck=0)
mtext(expression(paste(CH[4], " (Sat ratio)", sep="")), 2, 2)
mtext('2016', 1, 1.5)
abline(h=1, lty=3)

# Polygon of IQR
polyx<-c(LGRList$Q1$Date, rev(LGRList$Q3$Date))
polyy<-c(LGRList$Q1$CH4St_t, rev(LGRList$Q3$CH4St_t))
polygon(polyx, polyy/100, border=colors[5], col=colors[3])

# Mean and Median
points(LGRList$Median$Date, LGRList$Median$CH4St_t/100, type="l", col=colors[2], lwd=2)
points(LGRList$Mean$Date, LGRList$Mean$CH4St_t/100, type="l", col=colors[1], lwd=2)

#Buoy
points(Buoy_daily$Date, Buoy_daily$CH4Sat/100, type="o", col=colors[4], pch=16, cex=1, lty=2)

legend('topleft', inset=0.01, c('Mean', 'Meidan', 'IQR', 'Buoy'), col=colors, lty=c(1,1,1,2), pch=c(-1,-1,-1,16), lwd=c(2,2,15,1), pt.cex=c(1,1,1,1), bty="n")

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
xlabels<-month(xticks, label=TRUE, abbr=T)

co2_ylim<-range(c(LGRList$Mean$CO2St_t, LGRList$Q1$CO2St_t, LGRList$Q3$CO2St_t, Buoy_daily$CO2Sat), na.rm=T)
# co2_ylim[2]<-150

plot(LGRList$Mean$Date, LGRList$Mean$CO2St_t/100, type="n", pch=15, ylim=co2_ylim/100, ylab="", xlab="", xaxt="n")
axis(1, at=xticks, labels=NA)
axis(1, at=xticks+15, labels=xlabels, tck=0)
mtext(expression(paste(CO[2], " (Sat ratio)", sep="")), 2, 2)
mtext('2016', 1, 1.5)
abline(h=1, lty=3)

# Polygon of IQR
polyx<-c(LGRList$Q1$Date, rev(LGRList$Q3$Date))
polyy<-c(LGRList$Q1$CO2St_t, rev(LGRList$Q3$CO2St_t))
polygon(polyx, polyy/100, border=colors[5], col=colors[3])

# Mean and Median
points(LGRList$Median$Date, LGRList$Median$CO2St_t/100, type="l", col=colors[2], lwd=2)
points(LGRList$Mean$Date, LGRList$Mean$CO2St_t/100, type="l", col=colors[1], lwd=2)

#Buoy
points(Buoy_daily$Date, Buoy_daily$CO2Sat/100, type="o", col=colors[4], pch=16, cex=1, lty=2)

legend('topleft', inset=0.01, c('Mean', 'Meidan', 'IQR', 'Buoy'), col=colors, lty=c(1,1,1,2), pch=c(-1,-1,-1,16), lwd=c(2,2,15,1), pt.cex=c(1,1,1,1), bty="n")

dev.off()


