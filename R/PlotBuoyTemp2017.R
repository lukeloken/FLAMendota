# Code to plot Timeseries Temperature Depth Data
# Input is a dataframe consisting of "sampledate", "depth", and "wtemp" as columns. 

library(tidyr)
library(gtools)
library(LakeMetabolizer)
library(rLakeAnalyzer)
library(stringr)
library(RcppRoll)
library(lubridate)
library(viridis)
library(akima)

mindate<-as.POSIXct('2017-09-07 00:00:00')
cleandate<-as.POSIXct('2017-09-25 15:40:00', tz='America/Chicago')
colors<-c('darkred', 'darkblue', 'olivedrab')

#Data for all of 2017
LTERdf_all<-read.csv('Data/sensor_mendota_lake_watertemp_hourly_All2017.csv', header=T, stringsAsFactors = F)
LTERdf_all$sample_time<-str_pad(LTERdf_all$hour, 4, pad = "0")
LTERdf_all$datetime<-as.POSIXct(paste(LTERdf_all$sampledate, LTERdf_all$sample_time, sep=" "), format='%Y-%m-%d %H%M')


#omits NAs and INF
LTERdf_sub<-LTERdf_all[is.finite(LTERdf_all$wtemp),]

#Construct interpolated surface to plot
dates1=LTERdf_sub$datetime[1:45011]
depths1=LTERdf_sub$depth[1:45011]
value1=LTERdf_sub$wtemp[1:45011]
interp_dates1<-seq(ceiling_date(min(dates1), "hours"),floor_date(max(dates1), "hours"), by='hours')
interp1 <- akima::interp(dates1,depths1,value1, yo=seq(min(depths1),max(depths1), by=1), xo=interp_dates1, extrap=F)
names(interp1)<-c('Date', 'Depth', 'wtemp')
interp1$DateTime<-(interp_dates1)

#Second half
dates2=LTERdf_sub$datetime[45012:length(LTERdf_sub$datetime)]
depths2=LTERdf_sub$depth[45012:length(LTERdf_sub$depth)]
value2=LTERdf_sub$wtemp[45012:length(LTERdf_sub$wtemp)]
interp_dates2<-seq(ceiling_date(min(dates2), "hours"),floor_date(max(dates2), "hours"), by='hours')
interp2 <- akima::interp(dates2,depths2,value2, yo=seq(min(depths2),max(depths2), by=1), xo=interp_dates2, extrap=F)
names(interp2)<-c('Date', 'Depth', 'wtemp')
interp2$DateTime<-(interp_dates2)

interp<-list()
interp$DateTime<-c(interp1$DateTime, interp2$DateTime)
interp$Depth <-interp1$Depth 
interp$wtemp<-rbind(interp1$wtemp, interp2$wtemp)


#Plot 2017 Temp Data
# png('Figures/TempBathyGram_All2017.png', width=7, height=3, units='in', res=400, bg='white')
par(pch=16)
par(ps=10)
par(mfrow=c(1,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)

# datelabels<-unique(year(dates))
# datelocation<- (365.25*(datelabels-1970))

filled.contour(x=interp[[1]], y=interp[[2]], z=interp[[3]], ylim=c(max(interp[[2]]), 0), nlevels = 40, color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1), ylab="Depth (m)")

mtext(expression(paste("Water temperature (", degree, "C)", sep="")), 4, -6)

# dev.off()



#Construct interpolated surface to plot
dates=LTERdf_sub$datetime
depths=LTERdf_sub$depth
value=LTERdf_sub$wtemp
interp_dates<-seq(ceiling_date(min(dates), "days"),floor_date(max(dates), "days"), by='days')
interp_depths<-seq(min(depths),max(depths), by=.5)
interp <- akima::interp(dates,depths,value, yo=interp_depths, xo=interp_dates, linear=F)
names(interp)<-c('Date', 'Depth', 'wtemp')
interp$DateTime<-(interp_dates)

par(pch=16)
par(ps=10)
par(mfrow=c(1,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)


filled.contour(x=interp[[4]], y=interp[[2]], z=interp[[3]], ylim=c(max(interp[[2]]), 0), nlevels = 40, color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1), ylab="Depth (m)")



###################


#High resolution data for Fall 2017
LTERdf<-read.csv('Data/sensor_mendota_lake_watertemp_hi_res_Oct4_2017.csv', header=T, stringsAsFactors = F)
LTERdf$sample_time<-str_pad(LTERdf$sample_time, 4, pad = "0")

LTERdf$datetime<-as.POSIXct(paste(LTERdf$sampledate, LTERdf$sample_time, sep=" "), format='%Y-%m-%d %H%M')

LTERdf2<-read.csv('Data/sensor_mendota_lake_watertemp_hi_res_Oct26_2017.csv', header=T, stringsAsFactors = F)
LTERdf2$sample_time<-str_pad(LTERdf2$sample_time, 4, pad = "0")

LTERdf2$datetime<-as.POSIXct(paste(LTERdf2$sampledate, LTERdf2$sample_time, sep=" "), format='%Y-%m-%d %H%M')

LTERdf3<-read.csv('Data/sensor_mendota_lake_watertemp_hi_res_Nov30_2017.csv', header=T, stringsAsFactors = F)
LTERdf3$sample_time<-str_pad(LTERdf3$sample_time, 4, pad = "0")

LTERdf3$datetime<-as.POSIXct(paste(LTERdf3$sampledate, LTERdf3$sample_time, sep=" "), format='%Y-%m-%d %H%M')

LTERdf4<-smartbind(LTERdf, LTERdf2, LTERdf3)
LTERdf4$datetime<-as.POSIXct(LTERdf4$datetime, format='%Y-%m-%d %H:%M:%S')
LTERdf5 <- unique( LTERdf4[ , ] )

LTERdf_subset<-LTERdf5[LTERdf5$datetime>mindate,]

LTERdf_subset<-LTERdf_subset[order(LTERdf_subset$datetime),]

depths = unique(LTERdf_subset$depth)
dates = seq(min(LTERdf_subset$datetime), max(LTERdf_subset$datetime), by='mins')
# dates = seq(min(LTERdf_subset$datetime), max(LTERdf_subset$datetime), by='hours')
dates.df<-data.frame(datetime = c(dates))

#Make Matrix of temperature (x=depth, y=date, z=temp)
wrt1<-spread(LTERdf_subset[c('datetime', 'depth', 'wtemp')], key=depth, value=wtemp, fill=NA )
wrt3<-wrt1[which(!is.na(rowSums(wrt1[,-1]))),]
wrt_top<-wrt3[,1:15]
wrt2<-merge(wrt1, dates.df, by='datetime', all=T)
wrt<-as.matrix(wrt2[,-1])

# m.d.1 <- apply(wrt3[,-1], 1, function (x) meta.depths(x, depths=depths)[1])
# m.d.2 <- apply(wrt3[,-1], 1, function (x) meta.depths(x, depths=depths)[2])
# m.d<-rowMeans(data.frame(m.d.1, m.d.2))
# m.d.roll<-roll_mean(m.d, n=60*6+1, align='center', fill=NA)

t.d<-apply(wrt3[,-1], 1, function (x) thermo.depth(x, depths=depths)[1])
t.d.roll<-roll_mean(t.d, n=60*2+1, align='center', fill=NA)

# m.d.t1 <- apply(wrt_top[,-1], 1, function (x) meta.depths(x, depths=depths[1:14])[1])
# m.d.t2 <- apply(wrt_top[,-1], 1, function (x) meta.depths(x, depths=depths[1:14])[2])
# m.d.t<-rowMeans(data.frame(m.d.t1, m.d.t2))
# m.d.t.roll<-roll_mean(m.d.t, n=60*6+1, align='center', fill=NA)

t.d.t<-apply(wrt_top[,-1], 1, function (x) thermo.depth(x, depths=depths[1:14])[1])
t.d.t.roll<-roll_mean(t.d.t, n=60*2+1, align='center', fill=NA)


# plot(wrt3[,1],m.d.1, ylim=c(20,0), type='l', lwd=0.2, col='grey50')
# points(wrt3[,1], m.d.2, type='l', lwd=0.2, col='grey50')
# polygon(x=c(wrt3[,1], rev(wrt3[,1])), y=c(m.d.2, rev(m.d.1)), col='grey50', border=NA)
# lines(wrt3[,1],m.d.roll, lwd=2, col='blue')
# lines(wrt3[,1], t.d.roll, lwd=1, col='lightblue')
# 
# polygon(x=c(wrt_top[,1], rev(wrt_top[,1])), y=c(m.d.t2, rev(m.d.t1)), col='lightgreen', border=NA)
# lines(wrt_top[,1],m.d.t.roll, lwd=2, col='green')
# lines(wrt_top[,1], t.d.t.roll, lwd=1, col='magenta')


png('Figures/TempBathyGramSepNov2017.png', width=7.15, height=3.5, units='in', res=400, bg='white')
par(pch=16)
par(ps=10)
par(mfrow=c(1,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)


filled.contour(x=dates, y=depths, z=wrt, ylim=c(max(depths), 0), nlevels = 40, color.palette = colorRampPalette(c("violet", "blue", "cyan", "green3", "yellow", "orange", "red"), bias = 1, space = "rgb"), ylab="Depth (m)")


mtext(expression(paste("Water temperature (", degree, "C)", sep="")), 4, -6)

dev.off()

png('Figures/TempBathyGramSepNov2017_WithThermocline.png', width=7.15, height=3.5, units='in', res=400, bg='white')
par(pch=16)
par(ps=10)
par(mfrow=c(1,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)


xticks<-seq(ceiling_date(min(dates), "weeks"),floor_date(max(dates), "weeks"), by='weeks')
xlabels<-paste(lubridate::month(xticks, label=TRUE, abbr=T), day(xticks), sep=" ")


filled.contour(x=dates, y=depths, z=wrt, ylim=c(max(depths), 0), nlevels = 40, color.palette = colorRampPalette(c('navy', "blue", "cyan", "green3", "yellow", "orange", "red"), bias = 1, space = "rgb"), ylab="Depth (m)", plot.axes = { axis(1, at=xticks, labels=xlabels); axis(2);lines(wrt3[,1],t.d.roll, lwd=1, col='black'); lines(wrt_top[,1], t.d.t.roll, lwd=1, col='black') })

mtext(expression(paste("Water temperature (", degree, "C)", sep="")), 4, -6)

dev.off()


png('Figures/TempBathyGramSepNov2017_WithThermoclineViridis.png', width=7.15, height=3.5, units='in', res=400, bg='white')
par(pch=16)
par(ps=10)
par(mfrow=c(1,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)


xticks<-seq(ceiling_date(min(dates), "weeks"),floor_date(max(dates), "weeks"), by='weeks')
xlabels<-paste(lubridate::month(xticks, label=TRUE, abbr=T), day(xticks), sep=" ")


filled.contour(x=dates, y=depths, z=wrt, ylim=c(max(depths), 0), nlevels = 40, color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1), ylab="Depth (m)", plot.axes = { axis(1, at=xticks, labels=xlabels); axis(2);lines(wrt3[,1],t.d.roll, lwd=1, col='black'); lines(wrt_top[,1], t.d.t.roll, lwd=1, col='black') })

mtext(expression(paste("Water temperature (", degree, "C)", sep="")), 4, -6)

dev.off()



png('Figures/TempBathyGramSepNov2017_Viridis.png', width=7.5, height=3, units='in', res=400, bg='white')
par(pch=16)
par(ps=10)
par(mfrow=c(1,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)


xticks<-seq(ceiling_date(min(dates), "weeks"),floor_date(max(dates), "weeks"), by='weeks')
xlabels<-paste(lubridate::month(xticks, label=TRUE, abbr=T), day(xticks), sep=" ")


filled.contour(x=dates, y=depths, z=wrt, ylim=c(max(depths), 0), nlevels = 40, color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1), ylab="Depth (m)", plot.axes = { axis(1, at=xticks, labels=xlabels); axis(1, at=xticks[2], labels=xlabels[2]); axis(2)})

mtext(expression(paste("Water temperature (", degree, "C)", sep="")), 4, -6)

dev.off()


LTERmet<-read.csv('Data/sensor_mendota_lake_met_hourly_All2017.csv', header=T, stringsAsFactors = F)
LTERmet$sample_time<-str_pad(LTERmet$hour, 4, pad = "0")

LTERmet$datetime<-as.POSIXct(paste(LTERmet$sampledate, LTERmet$sample_time, sep=" "), format='%Y-%m-%d %H%M')
LTERmet_subset<-LTERmet[LTERmet$datetime>mindate,]



png('Figures/AitTemp2017.png', width=7.15, height=3.5, units='in', res=600, bg='white')
par(pch=16)
par(ps=10)
par(mfrow=c(2,1))
par(mar = c(0,3.5,0.5,7.5),mgp=c(1.5,0.4,0), oma=c(1.5,0,0,0), tck=-0.02)
par(lend=2)

plot(LTERmet_subset$datetime, LTERmet_subset$avg_air_temp, type='l', xlab='', ylab='', las=1, xaxs='i', yaxt='n', col=colors[1], lwd=2, xaxt='n')
axis(1, labels=NA, at=xticks)
axis(4, las=1,  col.ticks=colors[1], col.axis=colors[1])
mtext(expression(paste('Air temperature (', degree, 'C)')), 4, 1.5, col=colors[1])
par(new=T)
plot(LTERmet_subset$datetime, LTERmet_subset$avg_air_temp*9/5+32, type='n', xlab='', ylab='', axes=F, xaxs='i')
axis(4, las=1, line=3, col=colors[1], col.axis=colors[1])
mtext(expression(paste('(', degree, 'F)')), 4, 4.5, col=colors[1])

# par(new=T)
plot(LTERmet_subset$datetime, LTERmet_subset$avg_wind_speed, type='l', xlab='', ylab='', las=1, xaxs='i', col=colors[2], yaxt='n', lwd=2, xaxt='n')
axis(1, labels=xlabels, at=xticks)
mtext(expression(paste('Wind speed (m s'^'-1', ')')), 4, 1.5, col=colors[2])
axis(4, las=1, col.ticks=colors[2], col.axis=colors[2])

dev.off()


# With Oxygen

png('Figures/AirTempWindDO2017.png', width=7.15, height=4.5, units='in', res=600, bg='white')
par(pch=16)
par(ps=10)
par(mfrow=c(3,1))
par(mar = c(0,3.5,0.5,7.5),mgp=c(1.5,0.4,0), oma=c(1.5,0,0,0), tck=-0.02)
par(lend=2)

plot(LTERmet_subset$datetime, LTERmet_subset$avg_air_temp, type='l', xlab='', ylab='', las=1, xaxs='i', yaxt='n', col=colors[1], lwd=2, xaxt='n')
axis(1, labels=NA, at=xticks)
axis(4, las=1,  col.ticks=colors[1], col.axis=colors[1])
mtext(expression(paste('Air temperature (', degree, 'C)')), 4, 2, col=colors[1])
par(new=T)
plot(LTERmet_subset$datetime, LTERmet_subset$avg_air_temp*9/5+32, type='n', xlab='', ylab='', axes=F, xaxs='i')
axis(4, las=1, line=4, col.axis=colors[1], col.ticks=colors[1])
mtext(expression(paste('(', degree, 'F)')), 4, 5.5, col=colors[1])

# par(new=T)
plot(LTERmet_subset$datetime, LTERmet_subset$avg_wind_speed, type='l', xlab='', ylab='', las=1, xaxs='i', col=colors[2], yaxt='n', lwd=2, xaxt='n')
axis(1, labels=NA, at=xticks)
mtext(expression(paste('Wind speed (m s'^'-1', ')')), 4, 2, col=colors[2])
axis(4, las=1, col.ticks=colors[2], col.axis=colors[2])

plot(LTERmet_subset$datetime, LTERmet_subset$avg_opt_dosat_raw, type='l', xlab='', ylab='', las=1, xaxs='i', yaxt='n', col=colors[3], lwd=2, xaxt='n')

axis(1, labels=xlabels, at=xticks)
mtext(expression(paste('DO (% sat)')), 4, 2, col=colors[3])
axis(4, las=1,  col.ticks=colors[3], col.axis=colors[3])

abline(v=cleandate, lty=3)
text(x=cleandate, y=mean(par('usr')[4])-50, 'sensor cleaned', srt=0, pos=4)

dev.off()

