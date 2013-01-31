deg2rad <- function(deg) (deg*pi/180)

getDistanceLatLong <- function(lat1, lon1, lat2, lon2)
{
	r <- 6371
	dLat <- deg2rad(lat2-lat1) 
	dLon <- deg2rad(lon2-lon1); 
  	a <-  sin(dLat/2) * sin(dLat/2) + cos(deg2rad(lat1)) * 
          cos(deg2rad(lat2)) * sin(dLon/2) * sin(dLon/2)
  	c <- 2 * atan2(sqrt(a), sqrt(1-a))
  	d <- r * c # Distance in km
  	return(d)
}

getDistance <- function(user_id, event_id)
{
	user <- users[match(user_id,users$user_id),];
	event <- events[match(event_id,events$event_id),];
	return(getDistanceLatLong(user$Latitude,user$Longitude,event$lat,event$lng));
}

load("data/users_preprocessed.Rdata");
load("data/users_coordinates.Rdata");
load("data/codedData.Rdata");

users$Latitude <- as.numeric(as.character(users_coordinates$Latitude))
users$Longitude <- as.numeric(as.character(users_coordinates$Longitude))
users_events <- rbind(train[,1:2],test[,1:2])

distances <- apply(users_events,1,function(x)getDistance(x[1],x[2]))