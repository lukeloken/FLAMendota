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
  A <- 2.13 #plusmin 1.06
  B <- 2.18 #plusmin 0.30
  C <- 0.82 #plusmin 0.24
  
  #Vachon constants high/low
  A_high <- 2.13 + 1.06 
  A_low  <- 2.13 - 1.06
  B_high <- 2.18 + 0.30 
  B_low  <- 2.18 - 0.30
  C_high <- 0.82 + 0.24 
  C_low  <- 0.82 - 0.24
  
  #Calculate K600; equation converts fetch (m) to km
  k600<- A+ B*U10 + C*U10*log10(fetch/1000)
  
  k600_low  <- A_low + B_low*U10 + C_low*U10*log10(fetch/1000)
  k600_high <- A_high+ B_high*U10 + C_high*U10*log10(fetch/1000)
  
  
  # Replace negaative k with barely positive value
  k600[which(k600<0.01)]<-0.01
  k600_low[which(k600_low<0.01)]<-0.01
  k600_high[which(k600_high<0.01)]<-0.01
  
  return(list(k600, k600_low, k600_high))
}