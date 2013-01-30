# Script that fills the empty values for the latitude and longitude
# in the frame users_preprocessed using data from OpenStreetMaps

library(RJSONIO)

is.empty <-
function(x)
  # Empty if NA, empty string or string with only spaces
  is.na(x) || x=="" || prod(unlist(strsplit(x," ")) == "")

getCoordinatesfromLoc <- 
function(location)
{
  if ( is.empty(location) ) 
    c(NA,NA)
  else
  {
  	print(location)
    location <- gsub(' ','%20',location)
    url <- paste(
      "http://nominatim.openstreetmap.org/search?q="
      , location
      , "&limit=9&format=json"
      , sep="")
    x <- fromJSON(url)
    if(is.vector(x))
      c(x[[1]]$lat,x[[1]]$lon)
    else
      c(NA,NA)
  }
}

load("data/users_preprocessed.Rdata")

# Using Google Maps
library(taRifx.geo)
#coords <- apply(users,1,function(x)gGeoCode(x[6],verbose=TRUE,floodControl=TRUE))

users_preprocessed$Latitude <- users_latitude
users_preprocessed$Longitude <- users_longitude

# Using OpenStreetMaps
found <- 0
for (i in 1:nrow(users_preprocessed))
{
	if ( is.na(users_preprocessed[i,]$Latitude) && 
	     !is.empty(users_preprocessed[i,]$location) )
	{
		#OpenStreetMaps
		#coords <- getCoordinatesfromLoc(users_preprocessed[i,]$location)
		#print(coords)
		coords<-gGeoCode(users_preprocessed[i,]$location,verbose=TRUE,floodControl=TRUE)
		Sys.sleep(0.01)
		users_preprocessed[i,]$Latitude <- coords[1]
		users_preprocessed[i,]$Longitude <- coords[2]
		found <- found + 1
	}
}

users_latitude <- users_preprocessed$Latitude
users_longitude <- users_preprocessed$Longitude

users_coordinates <- data.frame(user=users_preprocessed$user_id,Latitude=users_latitude,Longitude=users_longitude)
save(users_coordinates, users_longitude, file="data/users_coordinates.Rdata")