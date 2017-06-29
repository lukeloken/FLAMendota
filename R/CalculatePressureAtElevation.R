
# Function to calculate actual air pressure from meterological data
# Most weather stations report atmospheric pressure corrected to sea-level
# This function converts back to actual pressure

PressureAtElevation<-function (PressureAtSeaLevel, Temperature, Elevation){
  Pressure <- PressureAtSeaLevel/((1-0.0065*Elevation/(Temperature+0.0065*Elevation+273.15))^-5.257)
  return (Pressure)
}

#End




