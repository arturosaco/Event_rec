deg2rad <- function(deg) (deg*pi/180)

getDistanceLatLong <- function(lat1, lon1, lat2, lon2)
{
	r <- 6371
	print(lat2)
	print(lat1)
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
	print(event)
	return(getDistanceLatLong(user$Latitude,user$Longitude,event$event_lat,event$event_long));
}

load("data/users_preprocessed.Rdata");
load("data/users_coordinates.Rdata");
load("data/codedData.Rdata");

users_events <- rbind(train[,1:2],test[,1:2])
distances <- apply(users_events[1:10000,],1,function(x)getDistance(x[1],x[2]))