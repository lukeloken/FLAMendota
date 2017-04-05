

#Convert k600 to k for CO2 at temperature of system. 

library(LakeMetabolizer)

# wind.scale(wind, height, units) # Function to convert wind speed to U10. 
# wind.scale(wind data frame..., height, units)


getSchmidt(temperature=20, gas="CO2")

# k.vachon(timeseries data, etc...) get gas exchange velocity. Need to check but I believe this is in k600

k600.2.kGAS.base(c(5:15), 20, "CO2")
