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

#replace strings of whitespaces, or emtpy strings with NA
users$locale <- replaceEmpty(users$locale)
users$location <- replaceEmpty(users$location)
#replace invalid locale data "id_ID" with NA
users$locale[!(users$locale %in% locale_vocab$Locale)] <- NA
users$location <- gsub("[0-9]","",users$location)
users$joinedAt <- as.Date(users$joinedAt)


# # Some initial function tests on different messy strings
# 
# 
# extractCountry2("Djoka Yogyakarta",world_states,US_states,CA_states)
# extractCountry2("Buenos Aires Brazil",world_states,US_states,CA_states)
# extractCountry2("Toronto Ontario",world_states,US_states,CA_states)
# extractCountry2("Los Angeles California",world_states,US_states,CA_states)
# extractCountry2("Los Angeles 82",world_states,US_states,CA_states)
# extractCountry2("Yogyakarta",world_states,US_states,CA_states)
# extractCountry2("Phnom Penh Phnum Penh",world_states,US_states,CA_states)

# extract Country
extractedCountryInfo <- extractCountry2(users$location,world_states,US_states,CA_states)
remaining_string <- extractedCountryInfo[,1]
remaining_string <- gsub(" *$","",remaining_string)


#check how many remaining strings do not match a city from the dictionary
valid.cities <- (remaining_string %in% cities_list)
tail(users[!valid.cities,],100)

users$unmatched_string <- rep(NA, length(remaining_string))
users$city <- rep(NA, length(remaining_string))
users$unmatched_string[!valid.cities] <- remaining_string[!valid.cities]
users$city[valid.cities] <- remaining_string[valid.cities]
users$region <- extractedCountryInfo[,2]
users$country <- extractedCountryInfo[,3]

# tests of pmatch
# pmatch(c("med","davos"), c("mean", "median", "mode davos","medi"),duplicates.ok=T) # returns 2
# pmatch(c("Invalid", "Los Angeles", "Los Angeles"), c("Los Angeles", "Los Alamos"), dup=TRUE)

#pmatch matches the each city against the BEGINNING of each element in the vector
matches <- pmatch(users$unmatched_string[!is.na(users$unmatched_string)],cities_list, dup = T)
sum(!is.na(matches))
mcity <- rep(NA, length(remaining_string))
mcity[!is.na(users$unmatched_string)] <- cities_list[matches]

#from valid cities infer the missing country using city_dict
# Possible TODO (not straightforward since many cities belong to multiple countries)
# for this data set the number of such cases is zero see below

# where city present and country missing get country from city (if unique)
matches <- match(users$city[!is.na(users$city) & is.na(users$country)], city_dict$City)
sum(!is.na(matches))
users$country[!is.na(users$city) & is.na(users$country)] <- city_dict$Country[matches]




# attach Geo data for users with a valid city and country entry
colstokeep <- c(5,6,7,16,17)
geoinfo <- matrix(NA,ncol=length(colstokeep),nrow=nrow(users))
geoinfo <- as.data.frame(geoinfo)
colnames(geoinfo) <- colnames(city_dict)[colstokeep]
present_entries <- (users$city %in% city_dict$City & 
                    users$country %in% city_dict$Country)
matches <- match(users$city[present_entries],city_dict$City)
geoinfo[present_entries,] <- city_dict[matches,colstokeep]

users_preprocessed <- cbind(users,geoinfo)
print("Some results:")
sum(is.na(users_preprocessed$city) & !is.na(users_preprocessed$country))
sum(!is.na(users_preprocessed$city) & is.na(users_preprocessed$country))
sum(is.na(users_preprocessed$city) | is.na(users_preprocessed$country))


cache("users_preprocessed")
