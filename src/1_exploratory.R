library('ProjectTemplate')
load.project()

train <- read.csv("data/train.csv")
att <- read.csv("data/event_attendees.csv", stringsAsFactors = FALSE)
users <- read.csv("data/users.csv", stringsAsFactors = FALSE)
users.fr <- read.csv("data/user_friends.csv", stringsAsFactors = FALSE)
events <- read.csv("data/events.csv", stringsAsFactors = FALSE, 
  nrows = 10000)

# ===========================
# = Explore attendance data =
# ===========================


countIDs <- function(col){
  col.1 <- strsplit(col, " ")
  sapply(col.1, length)
}

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

words <- events[, c("event_id",grep("c_", names(events), value = TRUE))]
words.m <- melt(words, id.vars = "event_id")
res <- ddply(words.m, "variable", function(sub){
    as.numeric(summary(sub$value))
  })

names(res) <- c("variable", c("min", "firstQ", "median", "mean", "thirdQ", "max"))
res$variable <- reorder(res$variable, res$mean, decreasing = TRUE)
ggplot(res[res$variable!= "c_other", ], aes(x = variable, y = mean, ymin = firstQ, ymax = thirdQ)) +
  geom_point() + geom_linerange() + coord_flip()

summary(apply(words[,-1], 1, sum))
table(apply(words[,-1], 1, sum) > 120)
princomp(words[,c(-1, -ncol(words))])


corrgram(words[,c(-1, -ncol(words))], order=NULL, lower.panel=panel.shade,
  upper.panel=NULL, text.panel=panel.txt,
  main="Car Milage Data (unsorted)")