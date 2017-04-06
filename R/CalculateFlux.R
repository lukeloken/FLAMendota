
library(LakeMetabolizer)
source('R/CalculateGasSaturation.R')

#Test names
spdf<-Mendota_surface_UTM
temp_name<-"TempC"
conc_name<-'CH4uM_t'
K600_name<-'k600'
gas<-'CH4'

#Test names
spdf<-Mendota_surface_UTM
temp_name<-"TempC"
conc_name<-'CO2uM_t'
K600_name<-'k600'
gas<-'CO2'
elevation=257
atm_conc<-1.91


#Still need to figure out how to allow pressure and elevation to be incorporated into gas.at.sat function

SpatailFlux<-function(spdf, conc_name, temp_name, K600_name, gas, ...){

  # temp
  temp<-spdf@data[,c(temp_name)] 
  
  # conc is in micromol per liter
  conc<-spdf@data[,c(conc_name)] 
  
  # kgas is in cm/hr 
  kgas<-k600.2.kGAS.base(spdf@data[,c(K600_name)], temp, gas)
  
  # conc.sat is in micromol per liter
  conc.sat<- gas.at.sat(tempC=temp, gas=gas, ...)
  
  # flux_out is in mmol per meter squared per day
  # Equation includes conversions: cm/hr to m/d ; micromol/liter to milimol/meter cubed
  flux_out<-(conc- conc.sat)*kgas*24/100

  return(flux_out)
  
}


flux_test<-SpatailFlux(spdf=Mendota_surface_UTM, conc_name='CO2uM_t', temp_name='TempC', K600_name='k600', gas='CO2', pressure= 1, atm_conc=650)

mean(flux_test)
18.73573
mean(flux_out)

spdf$flux_out<-flux_out

png('Figures/ExampleFlux.png', width=6, height=6, units='in', res=200, bg='white')

par(mfrow=c(1,1))
par(mar=c(4,4,4,4), oma=c(1,1,1,1))

print(spplot(spdf, zcol='flux_out', cuts=100, colorkey=TRUE, sp.layout=list(Mendota_Base_UTM, col=1, fill=0, lwd=3, lty=1, first=F), cex=1.06, pch=15, main="CO2 Efflux (mmol/m2/day)"))

dev.off()
closeAllConnections()



png('Figures/ExampleFluxHistogram.png', width=3, height=3, units='in', res=200, bg='white')
par(pch=16)
par(ps=12)
par(mfrow=c(1,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)


hist(spdf$flux_out, main="", xlab=expression(paste(CH[4], " efflux (mmol m"^"-2", " d"^"-1", ")")), breaks=seq(0,max(spdf$flux_out, na.rm=T)+1, 0.5), col="grey")
box(which='plot')

dev.off()

# wind.scale(wind, height, units) # Function to convert wind speed to U10. 
# wind.scale(wind data frame..., height, units)
wind.scale.base(4,1)

getSchmidt(temperature=20, gas="CO2")

# k.vachon(timeseries data, etc...) get gas exchange velocity. Need to check but I believe this is in k600

k600.2.kGAS.base(c(5:15), 20, "CO2")
k600.2.kGAS.base(c(5:15), 20, "O2")
k600.2.kGAS.base(c(5:15), 20, "CH4")
