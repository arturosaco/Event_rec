################################################################
# Purpose: Recode properly all variables from all data objects,
# replace invalid values, replace all empty strings,
# discard unusable variables containing different unprocessed strings
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

#setwd("F:/UCL/Applied ML/Event Project/Event_rec/")
load("data/loadedData.Rdata")
load("data/users_preprocessed.RData")
load("data/events_preprocessed.RData")


users_preprocessed.orig <- users_preprocessed
events.orig <- events
train.orig <- train



replaceEmpty <- function(char.vec)
{
  if (!is.character(char.vec)) {char.vec <- as.character(char.vec)}
  char.vec[grep("^\\s*$", char.vec)] <- NA
  return(char.vec)
}

getDistanceFromLatLonInKm <- function(lat1,lon1,lat2,lon2) 
{
  r <- 6371; # Radius of the earth in km
  dLat <- deg2rad(lat2-lat1) 
  dLon <- deg2rad(lon2-lon1) 
  a <-  sin(dLat/2) * sin(dLat/2) + cos(deg2rad(lat1)) * 
    cos(deg2rad(lat2)) * sin(dLon/2) * sin(dLon/2)
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  d <- r * c # Distance in km
  return(d)
}

deg2rad <- function(deg)
{
  return(deg * (pi/180))
}

# Phnom Penh to Los Angeles
#getDistanceFromLatLonInKm(11.55,105,34.05,-118.2)

# ==============
# = Train data =
# ==============


#events
events.sub <- events_preprocessed[,1:9]

colnames(events.sub) <- c("event_id","creator_id","event_start_time","event_city",
                      "event_state","event_zip","event_country","event_lat",
                      "event_lng")
events.sub <- as.data.frame(events.sub)
    
events.sub$event_id <- factor(events.sub$event_id)
events$event_lat <- as.numeric(as.character(events$event_lat))
events$event_lng <- as.numeric(as.character(events$event_lng))

# events.sub$event_lat[(events.sub$event_lat==1) & (events.sub$event_lng==1)] <- NA
# events.sub$event_lng[(events.sub$event_lat==1) & (events.sub$event_lng==1)] <- NA
# events.sub$event_lat[(events.sub$event_lat==2) & (events.sub$event_lng==2)] <- NA
# events.sub$event_lng[(events.sub$event_lat==2) & (events.sub$event_lng==2)] <- NA

### All the previous assignments are void, to see run next 2 lines
# table((events.sub$event_lat==1) & (events.sub$event_lng==1))
# table((events.sub$event_lat==2) & (events.sub$event_lng==2))

events.sub$event_start_time <- as.Date(events.sub$event_start_time)
events.sub$event_start_time_year <- as.factor(year(events.sub$event_start_time))
events.sub$event_start_time_month <- as.factor(month(events.sub$event_start_time))
events.sub$event_start_time_day <- as.factor(weekdays(events.sub$event_start_time))
summary(events.sub)

events <- events.sub
rm(events.sub)

#users
users <- users_preprocessed[, names(users_preprocessed) != "location"]
colnames(users)[13] <- "timezone_dict"
users$user_id <- as.factor(users$user_id)
users$locale <- as.factor(users$locale)
users$birthyear <- as.factor(users$birthyear)
users$gender <- as.factor(users$gender)
users$timezone <- as.factor(users$timezone)
users$unmatched_string <- as.factor(users$unmatched_string)
users$city <- as.factor(users$city)
users$region <- as.factor(users$region)
users$country <- as.factor(users$country)
users$timezone_dict <- as.factor(users$timezone_dict)
users$Capital <- as.factor(users$Capital)
users$MapReference <- as.factor(users$MapReference)

users$Latitude <- as.numeric(users$Latitude)
users$Longitude <- as.numeric(users$Longitude)


users$joinedAt_year <- as.factor(year(users$joinedAt))
users$joinedAt_month <- as.factor(month(users$joinedAt))
users$joinedAt_day <- as.factor(day(users$joinedAt))

summary(users)

#test
colnames(test)[1] <- "user_id"
colnames(test)[2] <- "event_id"

test$event_id <- as.factor(test$event_id)
test$user_id <- as.factor(test$user_id)
test$invited <- as.logical(test$invited)

colnames(test)[4] <- "event_seen_time"
test$event_seen_time <- as.Date(test$event_seen_time)
test$event_seen_time_year <- as.factor(year(test$event_seen_time))
test$event_seen_time_month <- as.factor(month(test$event_seen_time))
test$event_seen_time_day <- as.factor(weekdays(test$event_seen_time))


#train
colnames(train)[1] <- "user_id"
colnames(train)[2] <- "event_id"

train$event_id <- as.factor(train$event_id)
train$user_id <- as.factor(train$user_id)
train$invited <- as.logical(train$invited)

colnames(train)[4] <- "event_seen_time"
train$event_seen_time <- as.Date(train$event_seen_time)
train$event_seen_time_year <- as.factor(year(train$event_seen_time))
train$event_seen_time_month <- as.factor(month(train$event_seen_time))
train$event_seen_time_day <- as.factor(weekdays(train$event_seen_time))


train$interested.num <- NA
train[train$interested == 1, "interested.num"] <- 1
train[train$not_interested == 1, "interested.num"] <- 0
train[train$interested == 0 & 
  train$not_interested == 0, "interested.num"] <- 0

# remove the answers from the training data

interested.num_train <- train$interested.num
train$interested.num <- as.factor(train$interested.num)
# personally I strongly opt for discarding the original interest values
train <- train[,!names(train) %in% c("interested", "not_interested")]

# once the recoding is done, uncomment:
# save("train","test","users","users.friends","event.attendees","events",
#  file="codedData.Rdata")
