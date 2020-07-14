# functions for getting location info specific to a lat lon, including FIPS codes, from the data.fcc.gov API

library(jsonlite)
library(data.table)

location2FIPS <- function(place_id, lon, lat) {
  res <- fromJSON(paste0("http://data.fcc.gov/api/block/find?format=json&latitude=", lat, "&longitude=", lon, "&showall=true&format=JSON"))
  return(data.table(place_id = place_id, blockFIPS = res$Block$FIPS, countyFIPS = res$County$FIPS))
}

locations2FIPS <- function(idCol, lonCol, latCol) {
  return(as.data.table(t(mapply(location2FIPS, idCol, lonCol, latCol))))
}

location2PlaceId <- function(lon, lat, keyword) {
  keyword <- URLencode(keyword)
  url <- paste0("https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=", 
                lat, ",", lon, "&keyword=", keyword, "&key=AIzaSyCLmT3kQD4udAyLHlBaqYrBEEeX8XdZo7I")
  nearbysearch <- fromJSON(url)
  
  if (nearbysearch$status == "OK") {
    dt <- data.table(place_id = nearbysearch$results$place_id, lat, lon, keyword = URLdecode(keyword))
    return(dt)
  }
}

location2PlaceIds <- function(lonCol, latCol, keywordCol) {
  rbindlist(mapply(location2PlaceId, lonCol, latCol, keywordCol))
}