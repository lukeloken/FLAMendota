
library(LakeMetabolizer)

#Test names
spdf<-Mendota_surface_UTM
temp<-mean(spdf$TempC, na.rm=T)
conc_name<-'CH4uM_t'
K600_name<-'k600'
gas<-'CH4'

#Convert k600 to k for CO2 at temperature of system. 

SpatailFlux<-function(spdf, conc_name, temp, K600_name, gas){

  # conc is in micromol per liter
  conc<-spdf@data[,c(conc_name)] 
  
  # kgas is in cm/hr 
  kgas<-k600.2.kGAS.base(spdf@data[,c(K600_name)], temp, gas)
  
  # conc.sat is in micromol per liter
  conc.sat<- 0.003 # Need to figure this out (based on temperature, pressure)
  
  # flux_out is in mmol per meter squared per day
  # Equation includes conversions: cm/hr to m/d ; micromol/liter to milimol/meter cubed
  flux_out<-(conc- conc.sat)*kgas*24/100

  return(flux_out)
  

}

mean(flux_out)


# wind.scale(wind, height, units) # Function to convert wind speed to U10. 
# wind.scale(wind data frame..., height, units)
wind.scale.base(4,1)

getSchmidt(temperature=20, gas="CO2")

# k.vachon(timeseries data, etc...) get gas exchange velocity. Need to check but I believe this is in k600

k600.2.kGAS.base(c(5:15), 20, "CO2")
k600.2.kGAS.base(c(5:15), 20, "O2")
k600.2.kGAS.base(c(5:15), 20, "CH4")
