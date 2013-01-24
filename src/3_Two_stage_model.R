library(PojectTemplate)
load.project()

train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")

users <- read.csv("data/users.csv", stringsAsFactors = FALSE)
users.friends <- read.csv("data/user_friends.csv", stringsAsFactors = FALSE)


table(test$user %in% users$user_id)
table(test$user %in% users.friends$user)

users.friends.sub <- users.friends[users.friends$user %in% train$user, ]

friends <- lapply(1:nrow(users.friends.sub), function(row.int){
    friend.int <- strsplit(users.friends.sub[row.int, 2], " ")[[1]]
    cbind(user = users.friends.sub[row.int, 1],
     friend = friend.int)
  })
friends.1 <- do.call(rbind, friends)

friends.2 <- as.data.frame(friends.1[unique(friends.1[,2]) %in% users$user_id, ])
names(friends.2) <- c("original.user", "user_id")
users$user_id <- factor(users$user_id)
friends.3 <- join(friends.2, users)

att <- read.csv("data/event_attendees.csv")
