
# Parse and returns start and ending lat and long coords
geoData <- function(x){
  # JSON to lists
  locationData <- fromJSON(x)
  # Parse out Starting Lat coords from lists
  startLat <- locationData[["timelineObjects"]][["activitySegment"]][["startLocation"]][["latitudeE7"]]
  # Parse out Starting Long coords from lists
  startLong <- locationData[["timelineObjects"]][["activitySegment"]][["startLocation"]][["longitudeE7"]]
  # Parse out Ending Lat coords from lists
  endLat <- locationData[["timelineObjects"]][["activitySegment"]][["endLocation"]][["latitudeE7"]]
  # Parse out End Long coords from lists
  endLong <- locationData[["timelineObjects"]][["activitySegment"]][["endLocation"]][["longitudeE7"]]
  
  # Data does not come with decimal points
  convertToDec <- function(x){
    # Checks if leading char is a '-' char 
    if( substr(x, 1,1) == "-" ){
      # If line has leading '-' char, add a decimal place in 3 location
      Long <- as.numeric(paste(substr(x, 1,3), ".", substr(x, 4, length(x)), sep = ""))
    }else{
      # If line does not have leading '-' char, add a decimal place in the 2 location
      Lat <- as.numeric(paste(substr(x, 1,2), ".", substr(x, 3, length(x)), sep = ""))
    }
  }
  # Converts to dec
  startLatFinal <- convertToDec(startLat)
  startLongFinal <- convertToDec(startLong)
  endLatFinal <- convertToDec(endLat)
  endLongFinal <- convertToDec(endLong)  
  
  # Creates DF with start/ending lat and long coords
  locationHistory <- data.frame(startLatFinal, startLongFinal, endLatFinal, endLongFinal)
  # Removes N/As
  locationHistory <- remove_missing(locationHistory)
}
