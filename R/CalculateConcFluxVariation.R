
source('R/CalculateGasSaturation.R')
colors<-c('darkblue', 'darkred')

xticks<-seq(ceiling_date(min(LGRList$Mean$Date), "months"),floor_date(max(LGRList$Mean$Date), "months"), by='months')
xlabels<-paste(month(xticks, label=TRUE, abbr=T), " 1", sep="")
buoypch<-20


plot(LGRList$Mean$Date, (LGRList$Q75$CH4uM_t-LGRList$Q25$CH4uM_t)/(LGRList$Q75$CH4uM_t+LGRList$Q25$CH4uM_t), type="b", pch=15, ylab="", xlab="", xaxt="n", col=colors[2], ylim=c(0,.4))


points(LGRList$Mean$Date, (LGRList$Q75$CO2uM_t-LGRList$Q25$CO2uM_t)/(LGRList$Q75$CO2uM_t+LGRList$Q25$CO2uM_t), type="b", pch=15, ylab="", xlab="", xaxt="n", col=colors[1])

axis(1, at=xticks, labels=xlabels)

mtext('Quartile coefficient of dispersion', 2, 2)



plot(LGRList$Mean$Date, (LGRList$sd$CH4uM_t/LGRList$Mean$CH4uM_t), type="b", pch=15, ylab="", xlab="", xaxt="n", col=colors[2], ylim=c(0,1.8))

points(LGRList$Mean$Date, (LGRList$sd$CO2uM_t/LGRList$Mean$CO2uM_t), type="b", pch=15, ylab="", xlab="", xaxt="n", col=colors[1])

axis(1, at=xticks, labels=xlabels)


mtext('Coefficient of variation', 2, 2)



plot(LGRList$Mean$CH4uM_t~(LGRList$sd$CH4uM_t/LGRList$Mean$CH4uM_t), type="p", pch=15, ylab="mean CH4", xlab="CV CH4", col=colors[2], ylim=c(0,1.8))

plot(LGRList$Mean$CO2uM_t~(LGRList$sd$CO2uM_t/LGRList$Mean$CO2uM_t), type="p", pch=15, ylab="mean CO2", xlab="CV CO2",  col=colors[1])

axis(1, at=xticks, labels=xlabels)


ConcArray<-readRDS(file='Data/DailyFlamebyPixel.rds')

fluxmatrix<-readRDS('Data/DailyFluxperPixel.rds')
str(fluxmatrix)
sd(fluxmatrix)




sdCO2conc<-apply(ConcArray[,,'CO2uM_t'],1,sd) 
meanCO2conc<-apply(ConcArray[,,'CO2uM_t'],1,mean)
minCO2conc<-apply(ConcArray[,,'CO2uM_t'],1,min)
maxCO2conc<-apply(ConcArray[,,'CO2uM_t'],1,max)
summary(data.frame(minCO2conc, meanCO2conc, maxCO2conc))


Q5CO2<-apply(ConcArray[,,'CO2uM_t'],1,quantile, probs=0.05)
Q95CO2<-apply(ConcArray[,,'CO2uM_t'],1,quantile, probs=0.95)
mean(Q95CO2-Q5CO2)

Q25CO2<-apply(ConcArray[,,'CO2uM_t'],1,quantile, probs=0.25)
Q75CO2<-apply(ConcArray[,,'CO2uM_t'],1,quantile, probs=0.75)
mean(Q75CO2-Q25CO2)

sdCH4conc<-apply(ConcArray[,,'CH4uM_t'],1,sd) 
meanCH4conc<-apply(ConcArray[,,'CH4uM_t'],1,mean)
minCH4conc<-apply(ConcArray[,,'CH4uM_t'],1,min)
maxCH4conc<-apply(ConcArray[,,'CH4uM_t'],1,max)
summary(data.frame(minCH4conc, meanCH4conc, maxCH4conc))

Q5CH4<-apply(ConcArray[,,'CH4uM_t'],1,quantile, probs=0.05)
Q95CH4<-apply(ConcArray[,,'CH4uM_t'],1,quantile, probs=0.95)
mean(Q95CH4-Q5CH4)

Q25CH4<-apply(ConcArray[,,'CH4uM_t'],1,quantile, probs=0.25)
Q75CH4<-apply(ConcArray[,,'CH4uM_t'],1,quantile, probs=0.75)
mean(Q75CH4-Q25CH4)



sdCO2flux<-apply(fluxmatrix[,,'CO2'],1,sd) 
meanCO2flux<-apply(fluxmatrix[,,'CO2'],1,mean)
minCO2flux<-apply(fluxmatrix[,,'CO2'],1,min)
maxCO2flux<-apply(fluxmatrix[,,'CO2'],1,max)
Q1CO2flux<-apply(fluxmatrix[,,'CO2'],1,quantile, probs=0.25)
Q3CO2flux<-apply(fluxmatrix[,,'CO2'],1,quantile, probs=0.75)
MADCO2flux<-apply(fluxmatrix[,,'CO2'],1,mad)
QuantileDispersionCO2flux<-(Q3CO2flux-Q1CO2flux)/(Q3CO2flux+Q1CO2flux)
sumCO2flux<-apply(fluxmatrix[,,'CO2'],1,sum)

Q5CO2flux<-apply(fluxmatrix[,,'CO2'],1,quantile, probs=0.05)
Q95CO2flux<-apply(fluxmatrix[,,'CO2'],1,quantile, probs=0.95)
mean(Q95CO2flux-Q5CO2flux)

summary(data.frame(minCO2flux, meanCO2flux, maxCO2flux))

sdCH4flux<-apply(fluxmatrix[,,'CH4'],1,sd) 
meanCH4flux<-apply(fluxmatrix[,,'CH4'],1,mean)
minCH4flux<-apply(fluxmatrix[,,'CH4'],1,min)
maxCH4flux<-apply(fluxmatrix[,,'CH4'],1,max)
sumCH4flux<-apply(fluxmatrix[,,'CH4'],1,sum)

Q5CH4flux<-apply(fluxmatrix[,,'CH4'],1,quantile, probs=0.05)
Q95CH4flux<-apply(fluxmatrix[,,'CH4'],1,quantile, probs=0.95)
mean(Q95CH4flux-Q5CH4flux)


summary(data.frame(minCH4flux, meanCH4flux, maxCH4flux))

mean(meanCO2conc)/mean(meanCH4conc)
mean(meanCO2flux)/mean(meanCH4flux)

# Cumulative flux
# Multiple by pixel size to get annual emission estimate
# (mmol per Lake Mendota per 249 days)
# Change unit by multiplying by 10^-9 (Mmol)
sum(sumCO2flux)*200*200 *10^-9
sum(sumCH4flux)*200*200 *10^-9

# or do this with mean values
# multiple by lake area (200*200*988)
# 249 days
# 10^-9 mmol to Mmol
mean(meanCO2flux)*200*200*988*249*10^-9
mean(meanCH4flux)*200*200*988*249*10^-9

#Julia's numbers
43.61*200*200*988*249*10^-9
1.016*200*200*988*249*10^-9

mean(MADCO2flux)

mean(sdCO2conc/meanCO2conc)
mean(sdCH4conc/meanCH4conc)
mean(sdCO2flux/meanCO2flux)
mean(QuantileDispersionCO2flux)
mean(sdCH4flux/meanCH4flux)

plot(sdCO2conc/meanCO2conc)
plot(sdCH4conc/meanCH4conc)
plot(sdCO2flux/meanCO2flux)
plot(sdCH4flux/meanCH4flux)

plot(sdCO2flux)
plot(meanCO2flux)

meanCO2Sat<-apply(ConcArray[,,c('CO2St_t')],1,mean) 
maxCO2Sat<-apply(ConcArray[,,c('CO2St_t')],1,max) 
minCO2Sat<-apply(ConcArray[,,c('CO2St_t')],1,min) 
summary(data.frame(minCO2Sat, meanCO2Sat, maxCO2Sat))

meanCH4Sat<-apply(ConcArray[,,c('CH4St_t')],1,mean) 
maxCH4Sat<-apply(ConcArray[,,c('CH4St_t')],1,max) 
minCH4Sat<-apply(ConcArray[,,c('CH4St_t')],1,min) 
summary(data.frame(minCH4Sat, meanCH4Sat, maxCH4Sat))



#Conc percent diff
CH4ConcPerDiff<-100*abs((BuoyFlux$CH4Buoy_Conc_Mean-LakeFlux$CH4_Conc_Mean)/rowMeans(data.frame(BuoyFlux$CH4Buoy_Conc_Mean,LakeFlux$CH4_Conc_Mean)))

CO2ConcPerDiff<-100*abs((BuoyFlux$CO2Buoy_Conc_Mean-LakeFlux$CO2_Conc_Mean)/rowMeans(data.frame(BuoyFlux$CO2Buoy_Conc_Mean,LakeFlux$CO2_Conc_Mean)))


summary(CO2ConcPerDiff)
summary(CH4ConcPerDiff)

summary(BuoyFlux$CO2Buoy_Conc_Mean)
summary(BuoyFlux$CH4Buoy_Conc_Mean)

summary(LakeFlux$CO2_Conc_Mean)
summary(LakeFlux$CH4_Conc_Mean)




#Flux
CH4PerDiff<-100*abs((BuoyFlux$CH4Buoy_Flux_Mean-LakeFlux$CH4_Flux_Mean)/rowMeans(data.frame(BuoyFlux$CH4Buoy_Flux_Mean,LakeFlux$CH4_Flux_Mean)))

CO2PerDiff<-100*abs((BuoyFlux$CO2Buoy_Flux_Mean-LakeFlux$CO2_Flux_Mean)/rowMeans(data.frame(BuoyFlux$CO2Buoy_Flux_Mean,LakeFlux$CO2_Flux_Mean)))

summary(CO2PerDiff)
summary(CH4PerDiff)

summary(BuoyFlux$CO2Buoy_Flux_Mean)
summary(BuoyFlux$CH4Buoy_Flux_Mean)

summary(LakeFlux$CO2_Flux_Mean)
summary(LakeFlux$CH4_Flux_Mean)

#Percent Error
#Conc
CH4ConcPerError<-100*abs((BuoyFlux$CH4Buoy_Conc_Mean-LakeFlux$CH4_Conc_Mean)/LakeFlux$CH4_Conc_Mean)

CO2ConcPerError<-100*abs((BuoyFlux$CO2Buoy_Conc_Mean-LakeFlux$CO2_Conc_Mean)/LakeFlux$CO2_Conc_Mean)

summary(CH4ConcPerError)
summary(CO2ConcPerError)

#Flux
CH4PerError<-100*abs((BuoyFlux$CH4Buoy_Flux_Mean-LakeFlux$CH4_Flux_Mean)/LakeFlux$CH4_Flux_Mean)

CO2PerError<-100*abs((BuoyFlux$CO2Buoy_Flux_Mean-LakeFlux$CO2_Flux_Mean)/LakeFlux$CO2_Flux_Mean)

summary(CO2PerError)
summary(CH4PerError)

#PercentDiffThroughTime
png('Figures/PercentDiffTS.png', width=3, height=5, units='in', res=200, bg='white')
par(ps=12)
par(mgp=c(2.5,0.4,0),tck=-0.02)
par(lend=1)
par(lwd=1)
lty=c(1,2)
pt.cex=.8
alpha=0.6
# colors<-c('darkblue', 'darkred')

par(mfcol=c(4,1))
par(mar=c(2,4,0.5,0.5))
par(pch=1)
ylim=c(0,100)

plot(BuoyFlux$date_names, CO2ConcPerDiff, ylim=ylim, col=colors[1], xlab='', ylab='', cex=pt.cex)
abline(h=0)
abline(h=median(CO2ConcPerDiff), col=colors[1], lty=2)
legend('top', expression(paste(CO[2], ' concentration', sep='')), bty='n')

plot(BuoyFlux$date_names, CH4ConcPerDiff, ylim=ylim, col=colors[2], xlab='', ylab='', cex=pt.cex)
abline(h=0)
abline(h=median(CH4ConcPerDiff), col=colors[2], lty=2)
legend('top', expression(paste(CH[4], ' concentration', sep='')), bty='n')


plot(BuoyFlux$date_names, CO2PerDiff, ylim=ylim, col=colors[1], xlab='', ylab='', cex=pt.cex)
abline(h=0)
abline(h=median(CO2PerDiff), col=colors[1], lty=2)
legend('top', expression(paste(CO[2], ' flux', sep='')), bty='n')


plot(BuoyFlux$date_names, CH4PerDiff, ylim=ylim, col=colors[2], xlab='', ylab='', cex=pt.cex)
abline(h=0)
abline(h=median(CH4PerDiff), col=colors[2], lty=2)
legend('top', expression(paste(CH[4], ' flux', sep='')), bty='n')


mtext('Percent Differnece (%)', 2, -1.5, outer=T)

dev.off()

#PercentDiffHistorgram
png('Figures/PercentDiffHist.png', width=4, height=4, units='in', res=200, bg='white')
par(ps=10)
par(mgp=c(2.5,0.4,0),tck=-0.02)
par(lend=1)
par(lwd=1)
pt.cex=.8
alpha=0.6
# colors<-c('darkblue', 'darkred')
par(mfcol=c(2,2))
par(mar=c(0.25,1.25,0.25,0.25), oma=c(2.5,1.5,1,0))
par(pch=1)
xlim=c(0,125)
breaks<-seq(0,100000,5)
xaxis<-seq(0,125,25)

hist(CO2ConcPerDiff, col=add.alpha(colors[1], alpha), main='', xlab='', ylab='', xlim=xlim, breaks=breaks, xaxt='n', las=1)
box(which='plot')
axis(1, at=xaxis, labels=NA)
mtext('Concentration', 3, 0)
legend('topright', inset=0.01, c(expression(CO[2]), expression(CH[4])), bty='n', pch=22, pt.cex=3, pt.bg=c(add.alpha(c(colors[1:2]), alpha)), y.intersp=2)


hist(CH4ConcPerDiff, col=add.alpha(colors[2], alpha), main='', xlab='', ylab='', xlim=xlim, breaks=breaks, xaxt='n', las=1)
box(which='plot')
axis(1, at=xaxis)


hist(CO2PerDiff, breaks=breaks, col=add.alpha(colors[1], alpha), main='', xlab='', ylab='', xlim=xlim, xaxt='n', las=1)
box(which='plot')
axis(1, at=xaxis, labels=NA)
mtext('Flux', 3, 0)

hist(CH4PerDiff, breaks=breaks, col=add.alpha(colors[2], alpha), main='', xlab='', ylab='', xlim=xlim, xaxt='n', las=1)
box(which='plot')
axis(1, at=xaxis)


mtext('Frequency', 2, .5, outer=T)
mtext('Percent difference (%)', 1, 1.25, outer=T)


dev.off()


#Conc
CO2ConcLakeCum<-cumsum(LakeFlux$CO2_Conc_Mean)
CO2ConcBuoyCum<-cumsum(BuoyFlux$CO2Buoy_Conc_Mean)
CH4ConcLakeCum<-cumsum(LakeFlux$CH4_Conc_Mean)
CH4ConcBuoyCum<-cumsum(BuoyFlux$CH4Buoy_Conc_Mean)

#Flux
CO2FluxLakeCum<-cumsum(LakeFlux$CO2_Flux_Mean)
CO2FluxBuoyCum<-cumsum(BuoyFlux$CO2Buoy_Flux_Mean)
CH4FluxLakeCum<-cumsum(LakeFlux$CH4_Flux_Mean)
CH4FluxBuoyCum<-cumsum(BuoyFlux$CH4Buoy_Flux_Mean)

#Plot Cumulative Differneces
plot((CH4FluxBuoyCum-CH4FluxLakeCum)/CH4FluxLakeCum)
plot((CH4ConcBuoyCum-CH4ConcLakeCum)/CH4ConcLakeCum)
plot((CO2FluxBuoyCum-CO2FluxLakeCum)/CO2FluxLakeCum)
plot((CO2ConcBuoyCum-CO2ConcLakeCum)/CO2ConcLakeCum)


#Histrograms
png('Figures/HistogramsCO2CHConc.png', width=8, height=7, units='in', res=200, bg='white')
par(pch=16)
par(ps=12)
par(mfcol=c(2,2))
par(mar = c(2,1.5,1.5,0.5),mgp=c(2.5,0.4,0),tck=-0.02)
par(oma=c(1,1.5,0,0))
par(lend=2)
par(lwd=2)
lty=c(1,2)
pt.cex=1.5
alpha=0.6
buoycol='yellow'
# colors<-c('darkblue', 'darkred')

hist((ConcArray[,,c('CO2uM_t')]), col=add.alpha(colors[1], alpha), freq=F, breaks=seq(0,190,2), xlim=c(5,70), yaxs='i', ylim=c(0,0.115), main='Concentration', ylab='')
hist(BuoyFlux$CO2Buoy_Conc_Mean, add=T, col=add.alpha(buoycol, alpha), freq=F, breaks=seq(0,190,2), ylab='')
box(which='plot')
mtext(expression(paste(CO[2], ' (', mu, 'M)')), 1,2)

legend('topright', inset=0.01, c(expression(CO[2]), expression(CH[4]), 'Buoy'), bty='n', pch=22, pt.cex=3, pt.bg=c(add.alpha(c(colors[1:2], buoycol), alpha)), y.intersp=2)

# hist(log10(ConcArray[,,c('CH4uM_t')]), col=add.alpha(colors[2], alpha), breaks=seq(-3,1,.1), freq=F, ylim=c(0,1.9), xlim=c(-2.2,0.8), yaxs='i', main='', ylab='')
# hist(log10(BuoyFlux$CH4Buoy_Conc_Mean), add=T, col=add.alpha('yellow', alpha), freq=F, breaks=seq(-3,1,.1), ylab='')
# box(which='plot')
# mtext(expression(paste(log[10], ' ', CH[4], ' (', mu, 'M)')), 1,2)

hist((ConcArray[,,c('CH4uM_t')]), col=add.alpha(colors[2], alpha), freq=F,  yaxs='i', main='', ylab='', breaks=seq(0,6,.05), xlim=c(0,2), ylim=c(0,4.5))
hist((BuoyFlux$CH4Buoy_Conc_Mean), add=T, col=add.alpha(buoycol, alpha), freq=F, breaks=seq(0,6,.05), ylab='')
box(which='plot')
mtext(expression(paste(CH[4], ' (', mu, 'M)')), 1,2)


hist((fluxmatrix[,,'CO2']), col=add.alpha(colors[1], alpha), freq=F,  yaxs='i', main='Flux', breaks=seq(-100,800,10), xlim=c(-50,300), ylim=c(0,0.0349), ylab='')
hist(BuoyFlux$CO2Buoy_Flux_Mean, add=T, col=add.alpha(buoycol, alpha), freq=F, breaks=seq(-100,800,10), ylab='')
box(which='plot')
mtext(expression(paste(CO[2], ' efflux (mmol m'^'-2', ' d'^'-1', ')')), 1,2)
# 
# hist(log10(fluxmatrix[,,'CH4']), col=add.alpha(colors[2], alpha), freq=F,  yaxs='i', main='', breaks=seq(-4,2,.15), xlim=c(-2,1.5), ylim=c(0,1.199), ylab='')
# hist(log10(BuoyFlux$CH4Buoy_Flux_Mean), add=T, col=add.alpha('yellow', alpha), freq=F, breaks=seq(-4,2,.15), ylab='')
# box(which='plot')
# mtext(expression(paste(log[10], ' ', CH[4], ' efflux (mmol m'^'-2', ' d'^'-1', ')')), 1,2)

hist((fluxmatrix[,,'CH4']), col=add.alpha(colors[2], alpha), freq=F,  yaxs='i', main='',  ylab='', breaks=seq(-.5, 25, .15), ylim=c(0,1.3), xlim=c(0,6))
hist(BuoyFlux$CH4Buoy_Flux_Mean, add=T, col=add.alpha(buoycol, alpha), freq=F, breaks=seq(-.5, 25, .15), ylab='')
box(which='plot')
mtext(expression(paste(CH[4], ' efflux (mmol m'^'-2', ' d'^'-1', ')')), 1,2)


mtext('Density', 2,0.25, outer=T)

dev.off()

