# ######################################################
# Code to calculate k600 for points across the lake surface
# inputs are spatial points data frame, wind_height, fetch distance, wind speed
# Uses Vachon and Praire 2013 spatial k model constants
# Returns vector of k600 that aligns with spatial points data frame
# More Comments
# ######################################################

library(LakeMetabolizer)

# other formals to add to fetch_len - projected=T, dmax=20000
Spatialk600<-function(spdf, wind_speed, wind_height, fetch_name, ...){
  
  # Convert wind to a height of 10 m
  U10<-wind.scale.base(wind_speed, wind_height)
  
  #Vector of fetch distance (asuming input is in m)
  fetch<-spdf@data[,c(fetch_name)]
  
  # Vachon Constants
  A <- 2.13
  B <- 2.18
  C <- 0.82
  
  #Calculate K600; equation converts fetch (m) to km
  k600<- A+ B*U10 + C*U10*log10(fetch/1000)
  
  # Replace negaative k with barely positive value; probably should figure out what this should be...
  k600[which(k600<0.01)]<-0.01
  
  return(k600)
}