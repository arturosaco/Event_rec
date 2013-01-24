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
friends.1 <- do.call(rbind, friends)

### Take the subset for which we have information in the users table

friends.2 <- as.data.frame(friends.1[unique(friends.1[,2]) %in% users$user_id, ])
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
