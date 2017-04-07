# ###############################################
# Function to calculate fetch across lake surface
# inputs are ... spatial points data frame (spdf)
# wind direction (degrees) This should be the bearing from where the wind is blowing from (upwind)
# shoreline is either a spatial polygon or spatial lines object of lakeshore
# Should also pass projected=T if all objects are projected
# and dmax=distance for maximum fetch distance
# returns are ... vector fetch lengths (m). vector length = nrow(spdf)
# More comments....
# ###############################################

library(waver)
# other formals to add to fetch_len - projected=T, dmax=20000
SpatialFetch<-function(spdf, wind_dir, shoreline, ...){
  
  #check if spatial objects are accurate
  if (!is(spdf, "SpatialPoints")) 
    stop("spdf must be a SpatialPoints* object.")
  
  if (!(is(shoreline, "SpatialLines") || is(shoreline, "SpatialPolygons"))) {
    stop("shoreline must be a SpatialLines* or SpatialPolygons* object.")
  }
  
  #If shoreline is polygon, convert to line
  if (is(shoreline, "SpatialPolygons")) {
  shoreline<-as(shoreline, 'SpatialLines')}

  # Loop through all points of spdf and calculate fetch
  fetch_vector<-rep(NA, nrow(spdf))
  for (point in 1:nrow(spdf)){
    
    
    fetch_vector[point]<-fetch_len(spdf[point,], bearings=wind_dir, shoreline=shoreline, projected=T, dmax=10000)
  }
  
  #output is vector of distances. default is in meters
  return(fetch_vector)
}