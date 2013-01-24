library(ProjectTemplate)
load.project()

# =============
# = Read data =
# =============

train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")
users <- read.csv("data/users.csv", stringsAsFactors = FALSE)
users.friends <- read.csv("data/user_friends.csv", stringsAsFactors = FALSE)
att <- read.csv("data/event_attendees.csv", stringsAsFactors = FALSE)

### Take a subset of the friends table that only includes the users in the 
### train file

users.friends.sub <- users.friends[users.friends$user %in% train$user, ]

# ========================
# = Process friends file =
# ========================

### Un-list friends file and get a data.frame with the info

friends <- lapply(1:nrow(users.friends.sub), function(row.int){
    friend.int <- strsplit(users.friends.sub[row.int, 2], " ")[[1]]
    cbind(user = users.friends.sub[row.int, 1],
     friend = friend.int)
  })
friends.1 <- as.data.frame(do.call(rbind, friends))

### Take the subset for which we have information in the users table

friends.2 <- friends.1[unique(friends.1[,2]) %in% users$user_id, ]
names(friends.2) <- c("original.user", "user_id")
users$user_id <- factor(users$user_id)

### Join with the users table

friends.3 <- join(friends.2, users)

### Un-list the attendance file

att.yes <- melt.ish(att[, c("event", "yes")])
att.no <- melt.ish(att[, c("event", "no")])
att.maybe <- melt.ish(att[, c("event", "maybe")])
att.invited <- melt.ish(att[, c("event", "invited")])

### Construct the numeric interest variable 

att.maybe$int.num <- 1
att.no$int.num <- -1
att.yes$int.num <- 2
att.invited$int.num <- 0

### Put together the pieces of the attendance file and join with the friends table
### (which also contains the users table) 

att.1 <- rbind(att.maybe, att.no, att.yes, att.invited, att.invited)
friends.4 <- join(att.1[att.1$user_id %in% friends.3$user_id, ],
 friends.3, type = "inner")
friends.5 <- friends.4[!is.na(friends.4$locale), ]

### Join friends table with events table

names(friends.5)[1] <- "event_id"
events$user_id <- as.character(events$user_id)
friends.5$user_id <- as.character(friends.5$user_id)
events$event_id <- as.character(events$event_id)
friends.5$event_id <- as.character(friends.5$event_id)

fr.ev <- join(friends.5, events)

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


