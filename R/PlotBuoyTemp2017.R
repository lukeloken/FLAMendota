# Code to plot Timeseries Temperature Depth Data
# Input is a dataframe consisting of "sampledate", "depth", and "wtemp" as columns. 

library(tidyr)
library(gtools)
library(LakeMetabolizer)
library(rLakeAnalyzer)
library(stringr)

# LTERdf<-read.csv('Data/sensor_mendota_lake_watertemp_hourly2017.csv', header=T, stringsAsFactors = F)
# LTERdf$sample_time<-str_pad(LTERdf$hour, 4, pad = "0")

LTERdf<-read.csv('Data/sensor_mendota_lake_watertemp_hi_res_Sep2017.csv', header=T, stringsAsFactors = F)
LTERdf$sample_time<-str_pad(LTERdf$sample_time, 4, pad = "0")

LTERdf$datetime<-as.POSIXct(paste(LTERdf$sampledate, LTERdf$sample_time, sep=" "), format='%Y-%m-%d %H%M')
LTERdf_subset<-LTERdf[LTERdf$datetime>as.POSIXct('2017-09-07 00:00:00'),]

LTERdf_subset<-LTERdf_subset[order(LTERdf_subset$datetime),]

depths = unique(LTERdf_subset$depth)
dates = seq(min(LTERdf_subset$datetime), max(LTERdf_subset$datetime), by='mins')
# dates = seq(min(LTERdf_subset$datetime), max(LTERdf_subset$datetime), by='hours')
dates.df<-data.frame(datetime = c(dates))

#Make Matrix of temperature (x=depth, y=date, z=temp)
wrt1<-spread(LTERdf_subset[c('datetime', 'depth', 'wtemp')], key=depth, value=wtemp, fill=NA )
wrt2<-merge(wrt1, dates.df, by='datetime', all=T)
wrt<-as.matrix(wrt2[,-1])

m.d.1 <- apply(wrt1[,-1], 1, function (x) meta.depths(x, depths=depths)[1])
m.d.2 <- apply(wrt1[,-1], 1, function (x) meta.depths(x, depths=depths)[2])

plot(wrt1[,1],m.d.1, ylim=c(20,0), type='l')
points(wrt1[,1], m.d.2, col='red', type='l')

polygon(x=c(wrt1[,1], rev(wrt1[,1])), y=c(m.d.2, rev(m.d.1)), col='grey')

png('Figures/TempBathyGram2017.png', width=7.15, height=3.5, units='in', res=400, bg='white')
par(pch=16)
par(ps=10)
par(mfrow=c(1,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)


filled.contour(x=dates, y=depths, z=wrt, ylim=c(max(depths), 0), nlevels = 100, color.palette = colorRampPalette(c("violet", "blue", "cyan", "green3", "yellow", "orange", "red"), bias = 1, space = "rgb"), ylab="Depth (m)")
mtext(expression(paste("Water temperature (", degree, "C)", sep="")), 4, -6)

dev.off()


LTERmet<-read.csv('Data/sensor_mendota_lake_met_hourly_Sep2017.csv', header=T, stringsAsFactors = F)
LTERmet$sample_time<-str_pad(LTERmet$hour, 4, pad = "0")

LTERmet$datetime<-as.POSIXct(paste(LTERmet$sampledate, LTERmet$sample_time, sep=" "), format='%Y-%m-%d %H%M')
LTERmet_subset<-LTERmet[LTERmet$datetime>as.POSIXct('2017-09-07 00:00:00'),]



png('Figures/AitTemp2017.png', width=7.15, height=3.5, units='in', res=600, bg='white')
par(pch=16)
par(ps=10)
par(mfrow=c(2,1))
par(mar = c(0,3.5,0.5,7.5),mgp=c(1.5,0.4,0), oma=c(1.5,0,0,0), tck=-0.02)
par(lend=2)
colors<-c('darkred', 'darkblue')

plot(LTERmet_subset$datetime, LTERmet_subset$avg_air_temp, type='l', xlab='', ylab='', las=1, xaxs='i', yaxt='n', col=colors[1], lwd=2, xaxt='n')
axis(1, labels=NA)
axis(4, las=1, col=colors[1], col.ticks=colors[1], col.axis=colors[1])
mtext(expression(paste('Air temperature (', degree, 'C)')), 4, 1.5, col=colors[1])
par(new=T)
plot(LTERmet_subset$datetime, LTERmet_subset$avg_air_temp*9/5+32, type='n', xlab='', ylab='', axes=F, xaxs='i')
axis(4, las=1, line=3, col=colors[1], col.axis=colors[1])
mtext(expression(paste('(', degree, 'F)')), 4, 4.5, col=colors[1])

# par(new=T)
plot(LTERmet_subset$datetime, LTERmet_subset$avg_wind_speed, type='l', xlab='', ylab='', las=1, xaxs='i', col=colors[2], yaxt='n', lwd=2)
mtext(expression(paste('Wind speed (m s'^'-1', ')')), 4, 1.5, col=colors[2])
axis(4, las=1, col=colors[2], col.ticks=colors[2], col.axis=colors[2])

dev.off()

