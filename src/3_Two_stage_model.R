library(ProjectTemplate)
load.project()

# =============
# = Read data =
# =============

# train <- read.csv("data/train.csv")
# test <- read.csv("data/test.csv")
# users <- read.csv("data/users.csv", stringsAsFactors = FALSE)
# users.friends <- read.csv("data/user_friends.csv", stringsAsFactors = FALSE)
# att <- read.csv("data/event_attendees.csv", stringsAsFactors = FALSE)

load("data/stage_2.Rdata")

# ========================
# = Process friends file =
# ========================

### Un-list friends file and get a data.frame with the info


# system.time({friends.1 <- melt.ish.1(users.friends)})
# names(friends.1) <- c("original.user", "user_id")
# cache("friends.1")


### Take the subset for which we have information in the users table
### this is a SMALL subset, less than 1% of friends have info in the users table

# friends.2 <- friends.1[friends.1$user_id %in% users$user_id, ]
# cache("friends.2")

# users$user_id <- factor(users$user_id)

### Join with the users table

# friends.3 <-  users[users$user_id %in% 
#   friends.2$user_id, ]
# cache("friends.3")

### Un-list the attendance file

# event.attendees.yes <- melt.ish(event.attendees[, c("event", "yes")])
# event.attendees.no <- melt.ish(event.attendees[, c("event", "no")])
# event.attendees.maybe <- melt.ish(event.attendees[, c("event", "maybe")])
# event.attendees.invited <- melt.ish(event.attendees[, c("event", "invited")])

### Construct the numeric interest variable 

# event.attendees.maybe$int.num <- 1
# event.attendees.no$int.num <- -1
# event.attendees.yes$int.num <- 2
# event.attendees.invited$int.num <- 0

### Put together the pieces of the attendance file and join 
### with the friends table (which also contains the users table) 

# event.attendees.1 <- rbind(event.attendees.maybe, event.attendees.no, event.attendees.yes, event.attendees.invited)
# cache("att.1")

load("cache/friends.3.RData")
load("cache/att.1.RData")
friends.4 <- join(att.1[att.1$user_id %in% friends.3$user_id, ], friends.3)

### Join friends table with events table

names(friends.4)[1] <- "event_id"
friends.4$user_id <- as.character(friends.4$user_id)
events$event_id <- as.character(events$event_id)
friends.4$event_id <- as.character(friends.4$event_id)
events$user_id <- NULL
events$country <- NULL
fr.ev <- join(friends.4[!friends.4$user_id %in% 
  union(train$user_id, test$id$user_id)
  , ], events, by = "event_id")

names(fr.ev)[names(fr.ev) == "Latitude"] <- "user_lat"
names(fr.ev)[names(fr.ev) == "Longitude"] <- "user_lng"

### There are 55K rows of info that we are not using!

# ==========================
# = Code for grouping year =
# ==========================
groupYear <- function(birthyear){
  out <- birthyear - (birthyear %% 5)
  factor(out, seq(min(birthyear), max(birthyear), by = 5))
}

deg2rad <- function(deg) (deg*pi/180)
getDistanceLatLong <- function(row)
{
	lat1 <- as.numeric(row["user_lat"])
	lat2 <- as.numeric(row["event_lat"])
	lon1 <- as.numeric(row["user_lng"])
	lon2 <- as.numeric(row["event_lng"])
	r <- 6371
	dLat <- deg2rad(lat2-lat1) 
	dLon <- deg2rad(lon2-lon1); 
  	a <-  sin(dLat/2) * sin(dLat/2) + cos(deg2rad(lat1)) * 
          cos(deg2rad(lat2)) * sin(dLon/2) * sin(dLon/2)
  	c <- 2 * atan2(sqrt(a), sqrt(1-a))
  	d <- r * c # Distance in km
  	return(d)
}

# =========
# = Model =
# =========

fr.ev$birthyear <- as.numeric(as.character(fr.ev$birthyear))
fr.ev$timezone <- factor(fr.ev$timezone)

fr.ev$user_lat <- as.character(fr.ev$user_lat)
fr.ev$user_lng <- as.character(fr.ev$user_lng)
fr.ev$event_lat <- as.character(fr.ev$event_lat)
fr.ev$event_lng <- as.character(fr.ev$event_lng)

fr.ev[!is.na(fr.ev$user_lng)&(fr.ev$user_lng == ""), "user_lng"] <- NA
fr.ev[!is.na(fr.ev$user_lat)&(fr.ev$user_lat == ""), "user_lat"] <- NA
fr.ev[!is.na(fr.ev$event_lng)&(fr.ev$event_lng == ""), "event_lng"] <- NA
fr.ev[!is.na(fr.ev$event_lat)&(fr.ev$event_lat == ""), "event_lat"] <- NA

fr.ev.1 <- fr.ev[!is.na(fr.ev$birthyear) & !is.na(fr.ev$user_lat) & !is.na(fr.ev$event_lat), ]

fr.ev.1$birthyear <- groupYear(fr.ev.1$birthyear)
fr.ev.1$distance <- apply(fr.ev.1, 1, getDistanceLatLong)

fr.ev.1$user_lat <- as.numeric(fr.ev.1$user_lat)
fr.ev.1$user_lng <- as.numeric(fr.ev.1$user_lng)
fr.ev.1$event_lat <- as.numeric(fr.ev.1$event_lat)
fr.ev.1$event_lng <- as.numeric(fr.ev.1$event_lng)

formu <- as.formula(paste("int.num ~ birthyear + gender + c_other+ distance + user_lat + ",
                                          "user_lng + event_lat + event_lng + event_start_time_year + ",
                                          "event_start_time_month + event_start_time_day + ", 
                                          paste("c_", 1:100, sep = "", collapse = "+") ))
X <- model.matrix(formu, data = fr.ev.1)

mod.cv <- cv.glmnet(x = X, y = fr.ev.1$int.num, family = "gaussian", nfolds = 10)
mod <- glmnet(x = X, y = fr.ev.1$int.num, family = "gaussian", lambda = mod.cv$lambda.min)


# =========
# = Prediction =
# =========

train.fr.ev <- rbind(train[,setdiff(names(train),"interested.num")], test)
users$user_id <- as.numeric(as.character(users$user_id))
train.fr.ev <- join(train.fr.ev[,c("user_id","event_id","distance")],
                           users[,c("user_id","birthyear","gender","Latitude","Longitude")])
events$event_id <- as.numeric(as.character(events$event_id))
train.fr.ev <- join(train.fr.ev, 
                           events[,c("event_id","event_lat","event_lng","event_start_time_year",
                   						  "event_start_time_month","event_start_time_day",
                   						  "c_other",paste("c_", 1:100, sep=""))])
names(train.fr.ev)[names(train.fr.ev) == "Latitude"] <- "user_lat"
names(train.fr.ev)[names(train.fr.ev) == "Longitude"] <- "user_lng"
train.fr.ev$birthyear <- as.numeric(as.character(train.fr.ev$birthyear))
train.fr.ev$birthyear[!is.na(train.fr.ev$birthyear)] <- groupYear(train.fr.ev$birthyear[!is.na(train.fr.ev$birthyear)])

train.fr.ev$int.num <- rep(0, nrow(train.fr.ev))

predX <- model.matrix(formu, data = train.fr.ev)
pred <- predict(mod, predX, type="link")