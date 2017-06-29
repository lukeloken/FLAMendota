library(plyr)
library(dplyr)
library(ggplot2)

source('R/AddAlpha.R')

allflame<-readRDS('Data/DailyFlamebyPixel.rds')
allpoints<-readRDS('Data/All2016Points.rds')

df <- ldply(allpoints, data.frame)
str(df)
str(allflame)

rawcolors<-c('blue', 'red', 'grey90')
colors=add.alpha(rawcolors, 0.05)

# plot(allflame[,,"pH"], allflame[,,"XCO2Dppm_t"], col=colors[1], pch=16)
# plot(allflame[,,'ODOst_t'], allflame[,,"XCO2Dppm_t"], col=colors[1], pch=16)
# plot(allflame[,,'ChlARFU_t'], allflame[,,"XCO2Dppm_t"], col=color, pch=16)
# plot(allflame[,,'BGAPCRFU_t'], allflame[,,"XCO2Dppm_t"], col=color, pch=16)
# plot(allflame[,,'SPCScm_t'], allflame[,,"XCO2Dppm_t"], col=color, pch=16)
# 
# 
# plot(allflame[,,'ODOst_t'], allflame[,,"XCH4Dppm_t"], col=colors[2], pch=16)
# plot(allflame[,,'fDOMRFU_t'], allflame[,,"XCH4Dppm_t"], col=colors[2], pch=16)
# plot(allflame[,,'SPCScm_t'], allflame[,,"XCH4Dppm_t"], col=colors[2], pch=16)
# plot(allflame[,,'TrbFNU_t'], allflame[,,"XCH4Dppm_t"], col=colors[2], pch=16)
# plot(allflame[,,'BGAPCRFU_t'], allflame[,,"XCH4Dppm_t"], col=colors[2], pch=16)
# plot(allflame[,,'ChlARFU_t'], allflame[,,"XCH4Dppm_t"], col=colors[2], pch=16)



par(mar=c(3,3,1,1))
par(mgp=c(1.5,0.4,0),tck=-0.02)

smoothScatter(df[,'ODOsat_tau'], df[,"CO2Sat_tau"], colramp = colorRampPalette(c( blues9)), pch=16, col='lightgrey', cex=.5)
abline(200,-1, lty=2)

colfunc<-colorRampPalette(c('grey90', "cyan", "darkorchid3"))


baseplot<- ggplot(df,aes(x=ODOsat_tau,y=CO2Sat_tau)) + theme_bw() + ylim( low=0, high=1000)

baseplot + stat_bin2d(bins=200) + scale_fill_gradientn(colours=colfunc(10))

baseplot + stat_density_2d(aes(fill = ..level..), geom = "polygon")



baseplot +   geom_point(colour=colors[3]) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour=NA) + 
  scale_fill_continuous(low="purple4",high="orangered2") +
  # geom_smooth(method=lm,linetype=1,colour="red",se=F) + 
  geom_abline(slope=-1, intercept=200, colour='black', linetype=2) + 
  guides(alpha="none") +
  theme_bw()



png("Figures/Mendota_CO2vO2.png", height=4, width=5, units="in", res=300)
commonTheme = list(labs(color="Density",fill="Density",
                        x="O2 (%Sat)",
                        y="CO2 (%Sat)"),
                   theme_bw(),
                   theme(legend.position=c(0.75,0.75)))

ggplot(data=df,aes(x=ODOsat_tau,y=CO2Sat_tau))  + 
  ylim( low=0, high=1000) +
  geom_point(alpha=0.04, colour="gray") +
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour=NA, size=0.1) +
  scale_fill_continuous(low="cyan",high="magenta") +
  guides(alpha="none") +
  geom_hline(aes(yintercept=100), linetype="dashed", colour="gray") +
  geom_vline(aes(xintercept=100), linetype="dashed", colour="gray") +
  geom_abline(intercept=200, slope=-1) +
  commonTheme
dev.off()


png("Figures/Mendota_logCO2vpH.png", height=4, width=5, units="in", res=300)
commonTheme = list(labs(color="Density",fill="Density",
                        x="pH",
                        # y="log10 CO2 (uM)")))
                        y=expression(paste(log[10], ' ', CO[2], ' (', mu, 'M)', sep=''))),
                   theme_bw(),
                   theme(legend.position=c(0.1,0.25)))

ggplot(data=df,aes(x=pH_tau,y=log10(CO2uM_tau)))  + 
  ylim(0.5,2.5) + 
  # geom_hline(aes(yintercept=100), linetype="dashed", colour="gray") +
  geom_smooth(method=glm,linetype=2,colour="black",se=F, size=0.5) + 
  geom_point(alpha=0.04, colour="gray80") +
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour=NA, size=0.1) +
  scale_fill_continuous(low="purple4",high="cyan") +
  guides(alpha="none") +
  commonTheme
dev.off()


png("Figures/Mendota_CO2vpH.png", height=4, width=5, units="in", res=300)
commonTheme = list(labs(color="Density",fill="Density",
                        x="pH",
                        y="CO2 (uM)"),
                   theme_bw(),
                   theme(legend.position=c(0.75,0.75)))

ggplot(data=df,aes(x=pH_tau,y=CO2uM_tau))  + 
  ylim(0,200) + 
  geom_smooth(method=glm,linetype=2,colour="black",se=F, size=0.5) + 
  geom_point(alpha=0.04, colour="gray80") +
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour=NA, size=0.1) +
  scale_fill_continuous(low="purple4",high="cyan") +
  guides(alpha="none") +
  commonTheme
dev.off()


png("Figures/Mendota_CH4vpH.png", height=4, width=5, units="in", res=300)
commonTheme = list(labs(color="Density",fill="Density",
                        x="pH",
                        y="CH4 (uM)"),
                   theme_bw(),
                   theme(legend.position=c(0.75,0.75)))

ggplot(data=df,aes(x=pH_tau,y=CH4uM_tau))  + 
  ylim(0,15) + 
  geom_smooth(method=glm,linetype=2,colour="black",se=F, size=0.5) + 
  geom_point(alpha=0.04, colour="gray80") +
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour=NA, size=0.1) +
  scale_fill_continuous(low="purple4",high="cyan") +
  guides(alpha="none") +
  commonTheme
dev.off()


png("Figures/Mendota_CH4vBGA.png", height=4, width=5, units="in", res=300)
commonTheme = list(labs(color="Density",fill="Density",
                        x="BGA (RFU)",
                        y="CH4 (uM)"),
                   theme_bw(),
                   theme(legend.position=c(0.75,0.75)))

ggplot(data=df,aes(x=BGAPCRFU,y=CH4uM_tau))  + 
  ylim(0,15) + 
  geom_smooth(method=glm,linetype=2,colour="black",se=F, size=0.5) + 
  geom_point(alpha=0.04, colour="gray80") +
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour=NA, size=0.1) +
  scale_fill_continuous(low="purple4",high="cyan") +
  guides(alpha="none") +
  commonTheme
dev.off()

png("Figures/Mendota_CH4vchlA.png", height=4, width=5, units="in", res=300)
commonTheme = list(labs(color="Density",fill="Density",
                        x="ChlA (RFU)",
                        y="CH4 (uM)"),
                   theme_bw(),
                   theme(legend.position=c(0.75,0.75)))

ggplot(data=df,aes(x=ChlARFU,y=CH4uM_tau))  + 
  ylim(0,15) + 
  geom_smooth(method=glm,linetype=2,colour="black",se=F, size=0.5) + 
  geom_point(alpha=0.04, colour="gray80") +
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour=NA, size=0.1) +
  scale_fill_continuous(low="purple4",high="cyan") +
  guides(alpha="none") +
  commonTheme
dev.off()


png("Figures/Mendota_CH4vfdom.png", height=4, width=5, units="in", res=300)
commonTheme = list(labs(color="Density",fill="Density",
                        x="fDOM (RFU)",
                        y="CH4 (uM)"),
                   theme_bw(),
                   theme(legend.position=c(0.75,0.75)))

ggplot(data=df,aes(x=fDOMRFU,y=CH4uM_tau))  + 
  ylim(0,15) + 
  geom_smooth(method=glm,linetype=2,colour="black",se=F, size=0.5) + 
  geom_point(alpha=0.04, colour="gray80") +
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour=NA, size=0.1) +
  scale_fill_continuous(low="purple4",high="cyan") +
  guides(alpha="none") +
  commonTheme
dev.off()

png("Figures/Mendota_CH4vDO.png", height=4, width=5, units="in", res=300)
commonTheme = list(labs(color="Density",fill="Density",
                        x="DO (%sat)",
                        y="CH4 (uM)"),
                   theme_bw(),
                   theme(legend.position=c(0.75,0.75)))

ggplot(data=df,aes(x=ODOsat,y=CH4uM_tau))  + 
  ylim(0,15) + 
  geom_smooth(method=glm,linetype=2,colour="black",se=F, size=0.5) + 
  geom_point(alpha=0.04, colour="gray80") +
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour=NA, size=0.1) +
  scale_fill_continuous(low="purple4",high="cyan") +
  guides(alpha="none") +
  commonTheme
dev.off()






#Check what CO2 should be based on DIC and Alk
MedDICmolKG/2
36/12.01/1000

df$CO2guessDIC<-carb(flag=9, var1=df$pH, var2=MedDICmolKG, S=0.3, T=df$TempC, warn='n', k1k2="m06", Patm=0.93)$CO2*1000000

df$CO2uatmguessDIC<-carb(flag=9, var1=df$pH, var2=MedDICmolKG, S=0.3, T=df$TempC, warn='n', k1k2="m06", Patm=0.93)$pCO2

df$CO2guessALK<-carb(flag=8, var1=df$pH, var2=MedALKmolKG, S=0.3, T=df$TempC, warn='n', k1k2="m06", Patm=0.93)$CO2*1000000

plot(df$CO2guessDIC, df$CO2guessALK)
abline(0,1, col="red")

png("Figures/CO2obsvsDICmodelHeatPlot.png", height=4, width=5, units="in", res=300)
commonTheme = list(labs(color="Density",fill="Density",
                        x="CO2 Measured (uM)",
                        y="CO2 Calculated DIC-pH (uM)"),
                   theme_bw(),
                   theme(legend.position=c(0.75,0.25)))

ggplot(data=df,aes(x=CO2uM_tau,y=CO2guessDIC))  + 
  ylim(0,450) + 
  xlim(0,450) + 
  geom_smooth(method=glm,linetype=2,colour="black",se=F, size=0.5) + 
  geom_point(alpha=0.04, colour="gray80") +
  geom_abline(intercept=0, slope=1) +
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour=NA, size=0.1) +
  scale_fill_continuous(low="purple4",high="cyan") +
  guides(alpha="none") +
  commonTheme
dev.off()

png("Figures/CO2obsvsALKmodelHeatPlot.png", height=4, width=5, units="in", res=300)
commonTheme = list(labs(color="Density",fill="Density",
                        x="CO2 Measured (uM)",
                        y="CO2 Calculated ALK-pH (uM)"),
                   theme_bw(),
                   theme(legend.position=c(0.75,0.25)))

ggplot(data=df,aes(x=CO2uM_tau,y=CO2guessALK))  + 
  ylim(0,450) + 
  xlim(0,450) + 
  geom_smooth(method=glm,linetype=2,colour="black",se=F, size=0.5) + 
  geom_point(alpha=0.04, colour="gray80", shape=8) +
  geom_abline(intercept=0, slope=1) +
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour=NA, size=0.1) +
  scale_fill_continuous(low="purple4",high="cyan") +
  guides(alpha="none") +
  commonTheme
dev.off()


png("Figures/CO2obsvsDICmodel.png", height=5, width=5, units="in", res=300)

plot(df$CO2uM_tau, df$CO2guessDIC, col=colors[1], pch=16, ylim=c(0,450), xlim=c(0,450), xlab='Measured CO2 (uM)', ylab='Caulcated CO2 using DIC/pH (uM)')
abline(a=0,b=1)
DICmodel<-lm(df$CO2guessDIC~ df$CO2uM_tau)
summary(DICmodel)
abline(DICmodel, lty=2, col=rawcolors[1])
legend('right', inset=0.01, c('1:1', 'Best fit'), col=c('black', rawcolors[1]), lty=c(1,2), bty="n")

dev.off()

png("Figures/CO2obsvsALKmodel.png", height=5, width=5, units="in", res=300)

plot(df$CO2uM_tau, df$CO2guessALK, col=colors[1], pch=16, ylim=c(0,450), xlim=c(0,450), xlab='Measured CO2 (uM)', ylab='Caulcated CO2 using ALK/pH (uM)')
abline(a=0,b=1)
ALKmodel<-lm(df$CO2guessALK~ df$CO2uM_tau)
summary(ALKmodel)
abline(ALKmodel, lty=2, col=rawcolors[1])
legend('right', inset=0.01, c('1:1', 'Best fit'), col=c('black', rawcolors[1]), lty=c(1,2), bty="n")

dev.off()

#Old code

stat_density_2d

plot(df[,"pH_tau"], log10(df[,"CO2uM_tau"]), col=colors[1], pch=16)
plot(df[,'ODOsat_tau'], df[,"CO2Sat_tau"], col=colors[1], pch=16)
abline(200,-1, lty=2)
plot(df[,'ChlARFU'], df[,"CO2uM_tau"], col=color, pch=16)
plot(df[,'BGAPCRFU'], df[,"CO2uM_tau"], col=color, pch=16)
plot(df[,'SPCuScm_tau'], df[,"CO2uM_tau"], col=color, pch=16)


plot(df[,'ODOsat_tau'], log10(df[,"CH4uM_tau"]), col=colors[2], pch=16)
plot(df[,'fDOMRFU_tau'], log10(df[,"CH4uM_tau"]), col=colors[2], pch=16)
plot(df[,'SPCuScm_tau'], log10(df[,"CH4uM_tau"]), col=colors[2], pch=16)
plot(df[,'TurbFNU'], log10(df[,"CH4uM_tau"]), col=colors[2], pch=16)
plot(df[,'BGAPCRFU'], log10(df[,"CH4uM_tau"]), col=colors[2], pch=16)
plot(df[,'ChlARFU'], log10(df[,"CH4uM_tau"]), col=colors[2], pch=16)

df[df$CH4Sat_tau<1 & is.finite(df$CH4Sat_tau),1:5]



