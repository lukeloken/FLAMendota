


xticks<-seq(ceiling_date(min(LGRList$Mean$Date), "months"),floor_date(max(LGRList$Mean$Date), "months"), by='months')
xlabels<-paste(month(xticks, label=TRUE, abbr=T), " 1", sep="")
buoypch<-20


plot(LGRList$Mean$Date, (LGRList$Q75$CH4uM_t-LGRList$Q25$CH4uM_t)/(LGRList$Q75$CH4uM_t+LGRList$Q25$CH4uM_t), type="b", pch=15, ylab="", xlab="", xaxt="n", col="red", ylim=c(0,.4))


points(LGRList$Mean$Date, (LGRList$Q75$CO2uM_t-LGRList$Q25$CO2uM_t)/(LGRList$Q75$CO2uM_t+LGRList$Q25$CO2uM_t), type="b", pch=15, ylab="", xlab="", xaxt="n", col="blue")

axis(1, at=xticks, labels=xlabels)

mtext('Quartile coefficient of dispersion', 2, 2)



plot(LGRList$Mean$Date, (LGRList$sd$CH4uM_t/LGRList$Mean$CH4uM_t), type="b", pch=15, ylab="", xlab="", xaxt="n", col="red", ylim=c(0,1.8))

points(LGRList$Mean$Date, (LGRList$sd$CO2uM_t/LGRList$Mean$CO2uM_t), type="b", pch=15, ylab="", xlab="", xaxt="n", col="blue")

axis(1, at=xticks, labels=xlabels)


mtext('Coefficient of variation', 2, 2)




fluxmatrix<-readRDS('Data/DailyFluxperPixel.rds')
str(fluxmatrix)
sd(fluxmatrix[])

sdCO2conc<-apply(ConcArray[,,'CO2uM_t'],1,sd) 
meanCO2conc<-apply(ConcArray[,,'CO2uM_t'],1,mean)
minCO2conc<-apply(ConcArray[,,'CO2uM_t'],1,min)
maxCO2conc<-apply(ConcArray[,,'CO2uM_t'],1,max)
summary(data.frame(minCO2conc, meanCO2conc, maxCO2conc))


Q5CO2<-apply(ConcArray[,,'CO2uM_t'],1,quantile, probs=0.05)
Q95CO2<-apply(ConcArray[,,'CO2uM_t'],1,quantile, probs=0.95)
mean(Q95CO2-Q5CO2)

sdCH4conc<-apply(ConcArray[,,'CH4uM_t'],1,sd) 
meanCH4conc<-apply(ConcArray[,,'CH4uM_t'],1,mean)
minCH4conc<-apply(ConcArray[,,'CH4uM_t'],1,min)
maxCH4conc<-apply(ConcArray[,,'CH4uM_t'],1,max)
summary(data.frame(minCH4conc, meanCH4conc, maxCH4conc))

Q5CH4<-apply(ConcArray[,,'CH4uM_t'],1,quantile, probs=0.05)
Q95CH4<-apply(ConcArray[,,'CH4uM_t'],1,quantile, probs=0.95)
mean(Q95CH4-Q5CH4)

gas.at.sat(c(5.793, 27.22), gas='CH4', atm_conc=median(Atm$XCH4Dppm))
gas.at.sat(c(5.793, 27.22), gas='CO2', atm_conc=median(Atm$XCO2Dppm))


sdCO2flux<-apply(fluxmatrix[,,'CO2'],1,sd) 
meanCO2flux<-apply(fluxmatrix[,,'CO2'],1,mean)
minCO2flux<-apply(fluxmatrix[,,'CO2'],1,min)
maxCO2flux<-apply(fluxmatrix[,,'CO2'],1,max)
Q1CO2flux<-apply(fluxmatrix[,,'CO2'],1,quantile, probs=0.25)
Q3CO2flux<-apply(fluxmatrix[,,'CO2'],1,quantile, probs=0.75)
MADCO2flux<-apply(fluxmatrix[,,'CO2'],1,mad)
QuantileDispersionCO2flux<-(Q3CO2flux-Q1CO2flux)/(Q3CO2flux+Q1CO2flux)
sumCO2flux<-apply(fluxmatrix[,,'CO2'],1,sum)

summary(data.frame(minCO2flux, meanCO2flux, maxCO2flux))

sdCH4flux<-apply(fluxmatrix[,,'CH4'],1,sd) 
meanCH4flux<-apply(fluxmatrix[,,'CH4'],1,mean)
minCH4flux<-apply(fluxmatrix[,,'CH4'],1,min)
maxCH4flux<-apply(fluxmatrix[,,'CH4'],1,max)
sumCH4flux<-apply(fluxmatrix[,,'CH4'],1,sum)


summary(data.frame(minCH4flux, meanCH4flux, maxCH4flux))

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

