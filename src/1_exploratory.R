library('ProjectTemplate')
load.project()

# ========
# = Data =
# ========

train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")
users <- read.csv("data/users.csv", stringsAsFactors = FALSE)
users.friends <- read.csv("data/user_friends.csv", stringsAsFactors = FALSE)

train$timestamp <- read.date(train$timestamp)
test$timestamp <- read.date(test$timestamp)
users$joinedAt <- read.date(users$joinedAt)

# =================
# = General stuff =
# =================

#we have many more user profiles in "users" than users in "test" and "train"
nrow(test)
length(unique(test$user))
nrow(train)
length(unique(train$user))
#sanity checks
nrow(att)
length(unique(att$event))
nrow(users)
length(unique(users$user_id))

#none of the users in test is in train (we have no exact user history)
head(train)
head(test)
train[unique(test$user) %in% unique(train$user),]

#all users from train and test are provided with a profile
train[!(unique(train$user) %in% users$user_id),]
test[!(unique(test$user) %in% users$user_id),]


# ===========================
# = Explore attendance data =
# ===========================

att.num <- data.frame(event = att$event,
  yes.num = countIDs(att$yes),
  maybe.num = countIDs(att$maybe),
  no.num = countIDs(att$no),
  invited.num = countIDs(att$invited))

att.num.m <- melt(att.num, id.vars = c("event", "invited.num"))
events.weird.id <- att.num.m[att.num.m$invited.num < att.num.m$value, "event"]

events[events$event_id %in% events.weird.id, ]
att.num.m <- att.num.m[att.num.m$invited.num > 0, ]
att.num.m$prop <- att.num.m$value / att.num.m$invited.num

# =========================
# = Explore training data =
# =========================

train$interested.num <- NA
train[train$interested == 1, "interested.num"] <- 1
train[train$not_interested == 1, "interested.num"] <- -1
train[train$interested == 0 & train$not_interested == 0, "interested.num"] <- 0

### Non unique combinations of event + user

anom <- train.mat <- dcast(train, event + user ~ ., value.var = "interested.num")
names(anom) <- c("event", "user", "count")
anom <- anom[anom$count > 1, ]

train.anom <- train[train$event %in% anom$event & train$user %in% anom$user, ]
train.anom.1 <- ddply(train.anom, c("user", "event"), function(sub){
    data.frame(inv = length(unique(sub$invited)),
      int = length(unique(sub$interested.num)))
  })

### Every repeated record in the training set has the same values for interest/
### invited attributes, they differ in the time stamp

train.sub <- train
train.sub$timestamp <- NULL

train.sub <- unique(train.sub)
train.mat <- acast(train.sub, event ~ user, value.var = "interested.num",
  fill = 0)


# ======================
# = Exploration events =
# ======================


words <- cbind(event.att$event_id, 
  as.matrix(event.att[, grep("c_", names(event.att), value = TRUE)]))
words.m <- melt(words, id.vars = "event_id")
words.m$value <- as.numeric(words.m$value)
res <- ddply(words.m, "variable", function(sub){
    as.numeric(summary(as.numeric(sub$value)))
  })

names(res) <- c("variable", c("min", "firstQ", "median", "mean", "thirdQ", "max"))
res$variable <- reorder(res$variable, res$mean, decreasing = TRUE)
ggplot(res[res$variable!= "c_other" & res$mean > median(res$mean), ],
  aes(x = variable, y = mean, ymin = 
  firstQ, ymax = thirdQ)) +
  geom_point() + geom_linerange() + coord_flip()

summary(apply(words[,-1], 1, sum))
table(apply(words[,-1], 1, sum) > 120)
princomp(words[,c(-1, -ncol(words))])


corrgram(words[,c(-1, -ncol(words))], order=NULL, lower.panel=panel.shade,
  upper.panel=NULL, text.panel=panel.txt,
  main="Car Milage Data (unsorted)")

# =============================
# = Exploration users friends =
# =============================

class(users.friends)
dim(users.friends)
names(users.friends)

#check how many users are listed as friends
users.friends$friends<-as.character(users.friends$friends)
users.friends$friends<-strsplit(users.friends$friends,' ')

# Determination of the number of friends, for the first entry
number.friends<-length(users.friends$friends[[1]])

# Reading the index first friend
ind.friends.first<-as.numeric(users.friends$friends[[1]][1])

# Reading the index of the last friend
ind.friends.last<-as.numeric(users.friends$friends[[1]][number.friends])

number.friends
ind.friends.first
ind.friends.last

#create a 2 column matrix (User, Users friends) instead of a data.frame
# crashed R - vector size exceeeds 720 MB
#vect.users.friends <- do.call(rbind, users.friends$friends)



