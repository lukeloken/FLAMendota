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

# ChlA moving boxplots
# Code plots a polygon of IQR across entire lake surface, mean, median, and buoy

png('Figures/ChlA2016.png', width=8, height=4, units='in', res=200, bg='white')
par(pch=16)
par(ps=12)
par(mfrow=c(1,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)

xticks<-seq(ceiling_date(min(YSIList$Mean$Date), "months"),floor_date(max(YSIList$Mean$Date), "months"), by='months')
xlabels<-paste(month(xticks, label=TRUE, abbr=T), " 1", sep="")

chla_ylim<-range(c(YSIList$Mean$ChlAgL_t, YSIList$Q1$ChlAgL_t, YSIList$Q3$ChlAgL_t, Buoy_daily$ChlAugL_tau), na.rm=T)
colors<-c('red', 'black', 'grey', 'mediumblue', 'darkgrey')

plot(YSIList$Mean$Date, YSIList$Mean$ChlAgL_t, type="n", pch=15, ylim=chla_ylim, ylab="", xlab="", xaxt="n")
axis(1, at=xticks, labels=xlabels)

mtext(expression(paste("Chl-A (", mu, "g L"^"-1", ")", sep="")), 2, 2)
mtext('2016', 1, 1.5)

# Polygon of IQR
polyx<-c(YSIList$Q1$Date, rev(YSIList$Q3$Date))
polyy<-c(YSIList$Q1$ChlAgL_t, rev(YSIList$Q3$ChlAgL_t))
polygon(polyx, polyy, border=colors[5], col=colors[3])

# Mean and Median
points(YSIList$Median$Date, YSIList$Median$ChlAgL_t, type="l", col=colors[2], lwd=2)
points(YSIList$Mean$Date, YSIList$Mean$ChlAgL_t, type="l", col=colors[1], lwd=2)

#Buoy
points(Buoy_daily$Date, Buoy_daily$ChlAugL_tau, type="o", col=colors[4], pch=16, cex=1, lty=2)

legend('topright', inset=0.01, c('Mean', 'Median', 'IQR', 'Buoy'), col=colors, lty=c(1,1,1,2), pch=c(-1,-1,-1,16), lwd=c(2,2,15,1), pt.cex=c(1,1,1,1), bty="n")

dev.off()





# Carbon Dioxide moving boxplots
# Code plots a polygon of IQR across entire lake surface, mean, median, and buoy

png('Figures/BGA2016.png', width=8, height=4, units='in', res=200, bg='white')
par(pch=16)
par(ps=12)
par(mfrow=c(1,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)

xticks<-seq(ceiling_date(min(YSIList$Mean$Date), "months"),floor_date(max(YSIList$Mean$Date), "months"), by='months')
xlabels<-paste(month(xticks, label=TRUE, abbr=T), " 1", sep="")

bga_ylim<-range(c(YSIList$Mean$BGAPCgL_t, YSIList$Q1$BGAPCgL_t, YSIList$Q3$BGAPCgL_t, Buoy_daily$BGAPCugL_tau), na.rm=T)
# co2_ylim[2]<-150

plot(YSIList$Mean$Date, YSIList$Mean$BGAPCgL_t, type="n", pch=15, ylim=bga_ylim, ylab="", xlab="", xaxt="n")
axis(1, at=xticks, labels=xlabels)

mtext(expression(paste("BGA (", mu, "g L"^"-1", ")", sep="")), 2, 2)
mtext('2016', 1, 1.5)

# Polygon of IQR
polyx<-c(YSIList$Q1$Date, rev(YSIList$Q3$Date))
polyy<-c(YSIList$Q1$BGAPCgL_t, rev(YSIList$Q3$BGAPCgL_t))
polygon(polyx, polyy, border=colors[5], col=colors[3])

# Mean and Median
points(YSIList$Median$Date, YSIList$Median$BGAPCgL_t, type="l", col=colors[2], lwd=2)
points(YSIList$Mean$Date, YSIList$Mean$BGAPCgL_t, type="l", col=colors[1], lwd=2)

#Buoy
points(Buoy_daily$Date, Buoy_daily$BGAPCugL_tau, type="o", col=colors[4], pch=16, cex=1, lty=2)

legend('top', inset=0.01, c('Mean', 'Median', 'IQR', 'Buoy'), col=colors, lty=c(1,1,1,2), pch=c(-1,-1,-1,16), lwd=c(2,2,15,1), pt.cex=c(1,1,1,1), bty="n")

dev.off()


