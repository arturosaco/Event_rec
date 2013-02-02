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

friends.4 <- join(att.1[att.1$user_id %in% friends.3$user_id, ], friends.3)

### Join friends table with events table

names(friends.4)[1] <- "event_id"
friends.4$user_id <- as.character(friends.4$user_id)
events$event_id <- as.character(events$event_id)
friends.4$event_id <- as.character(friends.4$event_id)
events$user_id <- NULL
events$country <- NULL
fr.ev <- join(friends.4, events, by = "event_id")

# ====================
# = Set verification =
# ====================

length(unique(train$user))
length(intersect(train$user, users.friends$user))
# All the users in the train file have info in the users-friends table

length(intersect(intersect(train$user, users.friends$user), users$user_id))
# All have info in both the user and user-friends table

### Moving on to the users friends
# number of users that are friends with someone in the training set
length(unique(friends.1$friend))

# number of those that have info in the user table
length(intersect(friends.1$friend, users$user_id))

# number of those that appear at least once in the attendance table
length(intersect(intersect(friends.1$friend, users$user_id), att.1$user_id))

# number of unique events in the attendance file
# related to users who are friends with someone in the train set
# and who have info in the user table 

length(unique(att.1[att.1$user_id %in% 
  intersect(friends.1$friend, users$user_id), 
  "event"]))

# number of those that appear in the training set

length(intersect(unique(att.1[att.1$user_id %in% 
  intersect(friends.1$friend, users$user_id), 
  "event"]), train$event))


# all of those should appear in the events table

table(intersect(unique(att.1[att.1$user_id %in% 
  intersect(friends.1$friend, users$user_id), 
  "event"]), train$event) %in% events$event_id)

# and they do

#Lists of users ids and events ids that should be on the friends-events table

users.ids.all <- intersect(intersect(friends.1$friend, 
  users$user_id), att.1$user_id)
events.ids.all <- intersect(unique(att.1[att.1$user_id %in% 
  intersect(friends.1$friend, users$user_id), 
  "event"]), train$event)

# =========
# = Model =
# =========

fr.ev$birthyear <- as.numeric(as.character(fr.ev$birthyear))
fr.ev$timezone <- factor(fr.ev$timezone)

fr.ev$lat <- as.character(fr.ev$lat)
fr.ev$lng <- as.character(fr.ev$lng)

fr.ev[fr.ev$lng == "", "lng"] <- NA
#  paste("NA_", 1:sum(fr.ev$lng == ""), sep = "")
fr.ev[fr.ev$lat == "", "lat"] <- NA
#  paste("NA_", 1:sum(fr.ev$lat == ""), sep = "")

fr.ev$lat <- scale(as.numeric(fr.ev$lat))
fr.ev$lng <- scale(as.numeric(fr.ev$lng))

fr.ev.1 <- fr.ev[!is.na(fr.ev$birthyear) & !is.na(fr.ev$lat), ]

formu <- as.formula(paste("int.num ~ timezone + birthyear + gender + c_other+
 lat + lng + ", 
  paste("c_", 1:100, sep = "", collapse = "+")))
X <- model.matrix(formu, data = fr.ev.1)

mod.cv <- cv.glmnet(x = X, y = fr.ev.1$int.num, family = "gaussian",
  nfolds = 10)

mod <- glmnet(x = X, y = fr.ev.1$int.num, family = "gaussian", 
  lambda = mod.cv$lambda.min)
mod.1 <- lm(formu, data = fr.ev.1)

# ========
# = TODO =
# ========

# - group the birthyear in groups of 5 and add a missning class, treat as factor

