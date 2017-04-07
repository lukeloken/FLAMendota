
# Function to calculate flux at multiple points across the lake surface
# input is a spatial points data frame
# and column names of concentration (uM), temperature (C), and k600 (cm/hr)
# Also requires character string noting the gas of interest ('CO2' or 'CH4')
# Returns vector of length equal to nrow of spdf of flux in mmol/m2/d

library(LakeMetabolizer)

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
