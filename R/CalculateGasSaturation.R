# =====================================================
# Get saturation concentration for CO2 and CH4 in water
# Code needs temperature in C and gas
# optional inputs: atmosheric concentration (ppm), pressure, (atm), and elevation (m)
# =====================================================


gas.at.sat<-function(tempC, gas, atm_conc = NULL, pressure = NULL, elevation = NULL){
  tempK<-tempC+273.15
  
  # If no atmosphere concentration is supplied, use these numbers (ppm)
  if (is.null(atm_conc)){
    Atmosphere  <-  data.frame("CO2"=400)
    Atmosphere <- cbind(Atmosphere, "CH4"= 1.91)
    atm_conc<-unlist(Atmosphere[gas])[1]
  }
  
  # If no pressure and or elevation, default to sea level and one atmosphere
  if (is.null(pressure)){
    if (is.null(elevation)){
      elevation <- 0
      }
    pressure=(1-(.0000225577*elevation))^5.25588
  }

  # Determine Kh for gas based on temperature
  if (gas == "CO2"){
    #function to calculate equilibrium concentration of CO2 in water due to changes in water temperature
    #Plummer and Busenberg 1982; Geochimica et Cosmochimica Acta
    kh=10^(108.3865 + 0.01985076*tempK - 6919.53*(tempK^-1) -40.45154*log10(tempK)+669365*(tempK^-2))
    }
  if (gas == "CH4"){
    #Henry's Law constants from http://www.henrys-law.org/henry.pdf
    #Temperature correction using van't Hoff equation , temperature is in (Kelvin)
    #LakeKh is in ("Kh, cp") = (mol/L*Atm) at STP
    kh= as.numeric(0.0014*exp(1700*((1/tempK)-(1/298))))
  }
  
  #Equilibrium Concentration
  EquilSaturation=atm_conc*kh/pressure #umol/L, mmol/m3
  return(EquilSaturation)
}
