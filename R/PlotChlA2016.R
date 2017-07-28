# ###########################################
# Code to do.....
# ##########################################
library(lubridate)

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

chla_ylim<-range(c(YSIList$Mean$ChlAgL_t, YSIList$Q05$ChlAgL_t, YSIList$Q95$ChlAgL_t, Buoy_daily$ChlAugL_tau), na.rm=T)
colors<-c('red', 'black', 'grey85', 'orangered2', 'grey55')
buoypch=20

plot(YSIList$Mean$Date, YSIList$Mean$ChlAgL_t, type="n", pch=15, ylim=chla_ylim, ylab="", xlab="", xaxt="n")
axis(1, at=xticks, labels=xlabels)

mtext(expression(paste("Chl ", italic('a'), " (", mu, "g L"^"-1", ")", sep="")), 2, 2)
mtext('2016', 1, 1.5)

# Polygon of IQR
polyx90<-c(YSIList$Q05$Date, rev(YSIList$Q95$Date))
polyy90<-c(YSIList$Q05$ChlAgL_t, rev(YSIList$Q95$ChlAgL_t))
polygon(polyx90, polyy90, border=colors[3], col=colors[3])

# Polygon of IQR
polyx<-c(YSIList$Q25$Date, rev(YSIList$Q75$Date))
polyy<-c(YSIList$Q25$ChlAgL_t, rev(YSIList$Q75$ChlAgL_t))
polygon(polyx, polyy, border=colors[5], col=colors[5])

# Mean and Median
points(YSIList$Median$Date, YSIList$Median$ChlAgL_t, type="l", col=colors[2], lwd=2)
# points(YSIList$Mean$Date, YSIList$Mean$ChlAgL_t, type="l", col=colors[1], lwd=2)

#Buoy
points(Buoy_daily$Date, Buoy_daily$ChlAugL_tau, type="o", col=colors[4], pch=buoypch, cex=1.5, lty=2, lwd=1.5)


usr<-par('usr')
yscale<-diff(usr[3:4])/10
box.y<-usr[4]-yscale*seq(0.5,4, length.out=6)

xscale<-diff(usr[1:2])/30
box.x<-usr[1]+xscale*seq(0.7,2, length.out=2)+230

polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[5],2), rep(box.y[1],2)), col=colors[3], border=colors[3], lwd=2)
polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[4],2), rep(box.y[2],2)), col=colors[5], border=colors[5], lwd=2)
polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[3],2), rep(box.y[3],2)), col=colors[2], border=colors[2], lwd=2)

lines(x=box.x, y=rep(box.y[6], 2), lty=2, col=colors[4], type="l", pch=16, lwd=1.5)
points(x=mean(box.x), y=box.y[6], col=colors[4], type="p", pch=buoypch, cex=1.5)

text(x=box.x[2], y=box.y, c(expression(paste(Q[95])), expression(paste(Q[75])), expression(paste(Q[50])), expression(paste(Q[25])), expression(paste(Q[5])), 'Buoy'), pos=4)

box(which='plot')

dev.off()





# Blue Green Algae moving boxplots
# Code plots a polygon of IQR across entire lake surface, mean, median, and buoy

png('Figures/BGA2016.png', width=8, height=4, units='in', res=200, bg='white')
par(pch=16)
par(ps=12)
par(mfrow=c(1,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)

xticks<-seq(ceiling_date(min(YSIList$Mean$Date), "months"),floor_date(max(YSIList$Mean$Date), "months"), by='months')
xlabels<-paste(month(xticks, label=TRUE, abbr=T), " 1", sep="")

bga_ylim<-range(c(YSIList$Mean$BGAPCgL_t, YSIList$Q95$BGAPCgL_t, YSIList$Q05$BGAPCgL_t, Buoy_daily$BGAPCugL_tau), na.rm=T)
# co2_ylim[2]<-150

plot(YSIList$Mean$Date, YSIList$Mean$BGAPCgL_t, type="n", pch=15, ylim=bga_ylim, ylab="", xlab="", xaxt="n")
axis(1, at=xticks, labels=xlabels)

mtext(expression(paste("BGA (", mu, "g L"^"-1", ")", sep="")), 2, 2)
mtext('2016', 1, 1.5)

# Polygon of IQR
polyx90<-c(YSIList$Q05$Date, rev(YSIList$Q95$Date))
polyy90<-c(YSIList$Q05$BGAPCgL_t, rev(YSIList$Q95$BGAPCgL_t))
polygon(polyx90, polyy90, border=colors[3], col=colors[3])

# Polygon of IQR
polyx<-c(YSIList$Q25$Date, rev(YSIList$Q75$Date))
polyy<-c(YSIList$Q25$BGAPCgL_t, rev(YSIList$Q75$BGAPCgL_t))
polygon(polyx, polyy, border=colors[5], col=colors[5])

# Mean and Median
points(YSIList$Median$Date, YSIList$Median$BGAPCgL_t, type="l", col=colors[2], lwd=2)
# points(YSIList$Mean$Date, YSIList$Mean$BGAPCgL_t, type="l", col=colors[1], lwd=2)

#Buoy
points(Buoy_daily$Date, Buoy_daily$BGAPCugL_tau, type="o", col=colors[4], pch=buoypch, cex=1.5, lty=2, lwd=1.5)


usr<-par('usr')
yscale<-diff(usr[3:4])/10
box.y<-usr[4]-yscale*seq(0.5,4, length.out=6)

xscale<-diff(usr[1:2])/30
box.x<-usr[1]+xscale*seq(0.7,2, length.out=2)+50

polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[5],2), rep(box.y[1],2)), col=colors[3], border=colors[3], lwd=2)
polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[4],2), rep(box.y[2],2)), col=colors[5], border=colors[5], lwd=2)
polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[3],2), rep(box.y[3],2)), col=colors[2], border=colors[2], lwd=2)

lines(x=box.x, y=rep(box.y[6], 2), lty=2, col=colors[4], type="l", pch=16, lwd=1.5)
points(x=mean(box.x), y=box.y[6], col=colors[4], type="p", pch=buoypch, cex=1.5)

text(x=box.x[2], y=box.y, c(expression(paste(Q[95])), expression(paste(Q[75])), expression(paste(Q[50])), expression(paste(Q[25])), expression(paste(Q[5])), 'Buoy'), pos=4)

box(which='plot')

dev.off()






# DO moving boxplots
# Code plots a polygon of IQR across entire lake surface, mean, median, and buoy

png('Figures/DO2016.png', width=8, height=4, units='in', res=200, bg='white')
par(pch=16)
par(ps=12)
par(mfrow=c(1,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)
colors<-c('red', 'black', 'grey85', 'orangered2', 'grey55')
buoypch<-16

xticks<-seq(ceiling_date(min(YSIList$Mean$Date), "months"),floor_date(max(YSIList$Mean$Date), "months"), by='months')
xlabels<-paste(month(xticks, label=TRUE, abbr=T), " 1", sep="")

do_ylim<-range(c(YSIList$Mean$ODOst_t, YSIList$Q05$ODOst_t, YSIList$Q95$ODOst_t, Buoy_daily$ODOsat_tau), na.rm=T)
# co2_ylim[2]<-150

plot(YSIList$Mean$Date, YSIList$Mean$ODOst_t, type="n", pch=15, ylim=do_ylim, ylab="", xlab="", xaxt="n")
axis(1, at=xticks, labels=xlabels)

mtext(expression(paste("DO (%Sat)", sep="")), 2, 2)
mtext('2016', 1, 1.5)

# Polygon of 5-95%
polyx90<-c(YSIList$Q05$Date, rev(YSIList$Q95$Date))
polyy90<-c(YSIList$Q05$ODOst_t, rev(YSIList$Q95$ODOst_t))
polygon(polyx90, polyy90, border=colors[3], col=colors[3])

# Polygon of IQR
polyx<-c(YSIList$Q25$Date, rev(YSIList$Q75$Date))
polyy<-c(YSIList$Q25$ODOst_t, rev(YSIList$Q75$ODOst_t))
polygon(polyx, polyy, border=colors[5], col=colors[5])


# Mean and Median
points(YSIList$Median$Date, YSIList$Median$ODOst_t, type="l", col=colors[2], lwd=2)
# points(YSIList$Mean$Date, YSIList$Mean$ODOst_t, type="l", col=colors[1], lwd=2)

#Buoy
points(Buoy_daily$Date, Buoy_daily$ODOsat_tau, type="o", col=colors[4], pch=16, cex=1, lty=2)


usr<-par('usr')
yscale<-diff(usr[3:4])/10
box.y<-usr[4]-yscale*seq(0.5,4, length.out=7)

xscale<-diff(usr[1:2])/30
box.x<-usr[1]+xscale*seq(0.7,2, length.out=2)+230

polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[5],2), rep(box.y[1],2)), col=colors[3], border=colors[3], lwd=2)
polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[4],2), rep(box.y[2],2)), col=colors[5], border=colors[5], lwd=2)
polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[3],2), rep(box.y[3],2)), col=colors[2], border=colors[2], lwd=2)

lines(x=box.x, y=rep(box.y[6], 2), lty=2, col=colors[4], type="l", pch=16, lwd=1.5)
lines(x=box.x, y=rep(box.y[7], 2), lty=3, type="l")
points(x=mean(box.x), y=box.y[6], col=colors[4], type="p", pch=buoypch, cex=1.5)

text(x=box.x[2], y=box.y, c(expression(paste(Q[95])), expression(paste(Q[75])), expression(paste(Q[50])), expression(paste(Q[25])), expression(paste(Q[5])), 'Buoy', "Atm"), pos=4)

box(which='plot')

abline(h=100, lty=3)

dev.off()





# pH moving boxplots
# Code plots a polygon of IQR across entire lake surface, mean, median, and buoy

png('Figures/pH2016.png', width=8, height=4, units='in', res=200, bg='white')
par(pch=16)
par(ps=12)
par(mfrow=c(1,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)
colors<-c('red', 'black', 'grey85', 'orangered2', 'grey55')
buoypch<-16

xticks<-seq(ceiling_date(min(YSIList$Mean$Date), "months"),floor_date(max(YSIList$Mean$Date), "months"), by='months')
xlabels<-paste(month(xticks, label=TRUE, abbr=T), " 1", sep="")

ph_ylim<-range(c(YSIList$Mean$pH_tau, YSIList$Q05$pH_tau, YSIList$Q95$pH_tau, Buoy_daily$pH_tau), na.rm=T)
# co2_ylim[2]<-150

plot(YSIList$Mean$Date, YSIList$Mean$pH_tau, type="n", pch=15, ylim=ph_ylim, ylab="", xlab="", xaxt="n")
axis(1, at=xticks, labels=xlabels)

mtext(expression(paste("pH", sep="")), 2, 2)
mtext('2016', 1, 1.5)

# Polygon of 5-95%
polyx90<-c(YSIList$Q05$Date, rev(YSIList$Q95$Date))
polyy90<-c(YSIList$Q05$pH_tau, rev(YSIList$Q95$pH_tau))
polygon(polyx90, polyy90, border=colors[3], col=colors[3])

# Polygon of IQR
polyx<-c(YSIList$Q25$Date, rev(YSIList$Q75$Date))
polyy<-c(YSIList$Q25$pH_tau, rev(YSIList$Q75$pH_tau))
polygon(polyx, polyy, border=colors[5], col=colors[5])


# Mean and Median
points(YSIList$Median$Date, YSIList$Median$pH_tau, type="l", col=colors[2], lwd=2)
# points(YSIList$Mean$Date, YSIList$Mean$pH_tau, type="l", col=colors[1], lwd=2)

#Buoy
points(Buoy_daily$Date, Buoy_daily$pH_tau, type="o", col=colors[4], pch=16, cex=1, lty=2)


usr<-par('usr')
yscale<-diff(usr[3:4])/10
box.y<-usr[4]-yscale*seq(0.5,4, length.out=6)

xscale<-diff(usr[1:2])/30
box.x<-usr[1]+xscale*seq(0.7,2, length.out=2)+230

polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[5],2), rep(box.y[1],2)), col=colors[3], border=colors[3], lwd=2)
polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[4],2), rep(box.y[2],2)), col=colors[5], border=colors[5], lwd=2)
polygon(x=c(box.x, rev(box.x)), y=c(rep(box.y[3],2), rep(box.y[3],2)), col=colors[2], border=colors[2], lwd=2)

lines(x=box.x, y=rep(box.y[6], 2), lty=2, col=colors[4], type="l", pch=16, lwd=1.5)
points(x=mean(box.x), y=box.y[6], col=colors[4], type="p", pch=buoypch, cex=1.5)

text(x=box.x[2], y=box.y, c(expression(paste(Q[95])), expression(paste(Q[75])), expression(paste(Q[50])), expression(paste(Q[25])), expression(paste(Q[5])), 'Buoy'), pos=4)

box(which='plot')

dev.off()



