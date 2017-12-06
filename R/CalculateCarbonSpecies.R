
library(seacarb)

LTERChem<-readRDS('Data/NTL_ChemicalLimnology.rds')

MendotaChem<-LTERChem[LTERChem$lakeid=='ME',]
str(MendotaChem)

MendotaDIC<-MendotaChem[!is.na(MendotaChem$dic) & MendotaChem$dic!=(-99),]
MendotaPH<-MendotaChem[!is.na(MendotaChem$ph) & MendotaChem$ph!=(-99),]
MendotaALK<-MendotaChem[!is.na(MendotaChem$alk) & MendotaChem$alk!=(-99),]

plot(MendotaChem$dic[MendotaChem$depth<8]/12.01/1000, MendotaChem$alk[MendotaChem$depth<8]/1000000, xlim=c(0,0.006), ylim=c(0,0.006)) 
abline(0,1)

summary(MendotaDIC)
summary(MendotaPH)
summary(MendotaALK)

hist(MendotaDIC$dic[MendotaDIC$depth<8], breaks=100)
hist(MendotaPH$ph[MendotaPH$depth<8], breaks=100)
hist(MendotaALK$alk[MendotaALK$depth<8], breaks=100)

plot(MendotaDIC$depth, MendotaDIC$dic)
plot(MendotaDIC$sampledate, MendotaDIC$dic)

plot(MendotaPH$depth, MendotaPH$ph)
plot(MendotaPH$sampledate[MendotaPH$depth<8], MendotaPH$ph[MendotaPH$depth<8], type="l")

plot(MendotaALK$depth, MendotaALK$alk)
plot(MendotaALK$sampledate, MendotaALK$alk)



MedDIC<-median(MendotaDIC$dic[MendotaDIC$depth<8])
MedPH<-median(MendotaPH$ph[MendotaPH$depth<8])
MedALK<-median(MendotaALK$alk[MendotaALK$depth<8])

MedDICmolKG<-MedDIC/12.01/1000
MedALKmolKG<-MedALK/1000000

testcarb<-carb(flag=9, var1=8.4, var2=MedDICmolKG, S=0.3, T=25, warn='n', k1k2="m06", Patm=0.93)
testcarb$CO2*1000000
log10(testcarb$CO2*1000000)

testcarb2<-carb(flag=8, var1=8.4, var2=MedALKmolKG, S=0.3, T=25, warn='n', k1k2="m06", Patm=0.93)
testcarb2$CO2*1000000
log10(testcarb2$CO2*1000000)

f2pCO2(T=20, Patm=1, P=0, fCO2=604.6461)


