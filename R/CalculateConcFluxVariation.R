


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






