################################################################
# Purpose: Extract geodata out of the events file
################################################################

library(reshape2)
library(plyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(data.table)
library(glmnet)
library(chron)
library(stringr)
# =============
# = Functions =
# =============


recode <- function(x,reflistFrom, reflistTo){
  inlist <- (x %in% reflistFrom)
  elemind <- match(x,reflistFrom)
  replvec <- reflistTo[elemind]
  x[inlist] <- replvec[inlist]
  return(x)
}

replaceEmpty <- function(char.vec)
{
  if (!is.character(char.vec)) {char.vec <- as.character(char.vec)}
  char.vec[grep("^\\s*$", char.vec)] <- NA
  return(char.vec)
}

extractCountry2 <- function(string,world_states,US_states,CA_states)
{
  state <- rep(NA, length(string))
  region <- state
  unmatched_string <- state
  abrev <-  c(US_states$Abrev,CA_states$Abrev)

  
  state <- str_match(string,paste(c(world_states$Country),collapse="|"))
  #get rid of world countries from string, otherwise Tbilisi is in US
  string <- gsub(paste(c(world_states$Country),collapse="|"),"",string)
  matches <- str_match(string,paste(c(US_states$Country,US_states$Abrev),collapse="|"))
  region[!is.na(matches)] <- matches[!is.na(matches)]
  state[!is.na(matches)] <- "United States"
  
  matches <- str_match(string,paste(c(CA_states$Country,CA_states$Abrev),collapse="|"))
  region[!is.na(matches)] <- matches[!is.na(matches)]
  state[!is.na(matches)] <- "Canada"
  
  region <- recode(region,c(US_states$Abrev,CA_states$Abrev),c(US_states$Country,CA_states$Country))
  unmatched_string <- gsub(paste(c(world_states$Country,US_states$Country,CA_states$Country,abrev),collapse="|"),"",string)
  unmatched_string <- gsub(" *$","",unmatched_string)
  
  # fix 'special' candidates  
  matches <- str_match(string,paste(c("Yogyakarta","Jogjakarta"),collapse="|"))
  unmatched_string[!is.na(matches)] <- "Yogyakarta"
  state[!is.na(matches)] <- "Indonesia"
  
  matches <- str_match(string,paste(c("Phnom Penh","Phnum Penh"),collapse="|"))
  unmatched_string[!is.na(matches)] <- "Phnom Penh"
  state[!is.na(matches)] <- "Cambodia"
  
  return(cbind(unmatched_string, region, state))
}

# ======================
# = Data Preprocessing =
# ======================

# Logic: (prefer exact string matching to Levenshtein distance matching)

# 1. Split strings to known country and 'remaining string'
# 2a.Check if remaining string is present in the list of cities
# 2b. Check if a city from list of cities matches the beginning of remaining string (only 68 cases presently)
# 3. Try to infer missing country from city if city is present and known (only )
# 4. final step: attach Geo info for each user from known City and Country

# Don't use locale for guessing location because it produces bad results

# load and prepare dictionaries
# for the cities:
# downloaded from http://www.geobytes.com/GeoWorldMap.zip

country_dict <- read.csv("data/Geodict/Countries.txt", stringsAsFactors = FALSE)
city_dict <- read.csv("data/Geodict/cities.txt", stringsAsFactors = FALSE)
country_dict <- country_dict[match(city_dict$CountryID,country_dict$CountryId),-c(1,15,16)]
city_dict <- cbind(city_dict,country_dict)
#remove multiple instances of a city (some cities have a lot of entries)
city_dict <- cbind(city_dict,country_dict)
city_dict <- city_dict[!duplicated(cbind(city_dict$City,city_dict$Country)),]
cities_list <- unique(city_dict$City)

# for the countries:
# copied from Wiki lists
locale_vocab <- read.csv("data/Geodict/locale_list.txt")
world_states <- read.csv("data/Geodict/world_states.csv", stringsAsFactors = FALSE,header=F)
US_states <- read.csv("data/Geodict/US_states.csv", stringsAsFactors = FALSE,header=F)
CA_states <- read.csv("data/Geodict/CA_states.csv", stringsAsFactors = FALSE,header=F)
colnames(world_states) <- c("Country","Capital")
colnames(US_states) <- c("Country","Capital","Abrev")
colnames(CA_states) <- c("Country","Capital","Abrev")
colnames(locale_vocab) <- c("Country","Locale")

# initial recoding and NA replacement

load(file = "data/stage_1.Rdata")

for(var in c("city", "state", "zip", "country", "lat", "lng")){
  events[, var] <- replaceEmpty(events[, var])
}

# =============================================
# = Retrieve latitude and longitude from city =
# =============================================

events.orig <- events
#events <- events.orig
events$lat <- as.numeric(events$lat)
events$lng <- as.numeric(events$lng)

summary(events$lat)
summary(events$lng)

table(is.na(events.orig$lat) & !is.na(events.orig$city))
cities.to.locate <- unique(events.orig[is.na(events.orig$lat) & 
  !is.na(events.orig$city), "city"])
index.to.locate <- is.na(events.orig$lat) &  !is.na(events.orig$city)
cities.to.locate[cities.to.locate == "West Los Angeles"] <- "Los Angeles"
cities.to.locate[cities.to.locate == "Mexico City"] <- "Mexico"

match <- pmatch(cities.to.locate, city_dict$City, dup = TRUE)
city.match <- data.frame(City.orig = cities.to.locate, City = city_dict$City[match])
city.match.1 <- join(city.match, city_dict[,c("City", "Latitude", "Longitude")], type = "inner")

### We can infer 23 latitudes and longitudes unambiguously 

sum(ddply(city.match.1, "City", summarise, length(Latitude))[,2]==1)

### I don't think it's worth doing it anymore.

events_preprocessed <- events
save("events_preprocessed",file="data/events_preprocessed.RData")