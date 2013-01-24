library(RJSONIO)

is.empty <-
function(x)
  # Empty if NA, empty string or string with only spaces
  is.na(x) || x=="" || prod(unlist(strsplit(x," ")) == "")

getCoordinates <- 
function(city, country)
{
  if ( is.empty(city) && is.empty(country) ) 
    c(NA,NA)
  else
  {
    city <- gsub(' ','%20',city)
    country <- gsub(' ','%20',country)
    print(city)
    print(country)
    url <- paste(
      "http://nominatim.openstreetmap.org/search?city="
      , city
      , "&contry="
      , country
      , "&limit=9&format=json"
      , sep="")
    x <- fromJSON(url)
    if(is.vector(x))
      c(x[[1]]$lat,x[[1]]$lon)
    else
      c(NA,NA)
  }
}

events <- read.csv("../data/events.csv",nrows=100)
apply(events,1,function(x)getCoordinates(x[4],x[7]))