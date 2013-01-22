library(ProjectTemplate)
load.project()

# =========================
# = Read/pre-process data =
# =========================

att <- read.csv("data/event_attendees.csv", stringsAsFactors = FALSE)
events.aux <- read.csv("data/events.csv", stringsAsFactors = FALSE, 
  nrows = 100)
names.aux <- names(events.aux)
train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")

ev.ids <- union(union(att$event, train$event), test$event)
rm(att, test)
gc()

file <- "data/events.csv"

f <- file(file,'r')
invisible(readLines(f, n = 1))
ev.temp <- readLines(f)
ev.id.temp <- gsub(',.*', "", ev.temp)
out <- ev.temp[ev.id.temp %in% ev.ids]
out <- strsplit(out, ",")
out <- do.call(c, out)
out <- as.data.frame(matrix(out, nrow = length(out) / length(names.aux), 
  ncol = length(names.aux), byrow = TRUE, dimnames = list(NULL, names.aux)))
close(f)
for(int in grep("c_", names(out), value = TRUE)){
  out[, int] <- as.numeric(out[, int])
}
event.att <- out
names(train)[2] <- "event_id"
train$event_id <- factor(train$event_id)
library(reshape2)
train.1 <- join(train, event.att)

train.1$interested.num <- NA
train.1[train.1$interested == 1, "interested.num"] <- 1
train.1[train.1$not_interested == 1, "interested.num"] <- -1
train.1[train.1$interested == 0 & 
  train.1$not_interested == 0, "interested.num"] <- 0

train.1$lat <- as.numeric(train.1$lat)
train.1$lng <- as.numeric(train.1$lng)

# =========
# = Model =
# =========

### Train

library(glmnet)
formu <- as.formula(paste("event_id ~ ",
 paste(c("city", "state", "country",
  "lat", "lng", "c_other", paste("c", 1:100, sep = "_")), collapse = " + ")))
X <- model.matrix(formu, data = train.1)

# 10-fold cross validation

cv.mod <- cv.glmnet(x = X, y = train.1$interested.num, family = "gaussian",
  nfolds = 10)

#Lamabda min out of 10-fold cross validation
#0.005364677

mod <- glmnet(x = X, y = train.1$interested.num, family = "gaussian", 
  lambda = 0.005364677)

### Predictions

test <- read.csv("data/test.csv")
names(test)[2] <- "event_id"
test$event_id <- factor(test$event_id)

aux <- table(event.att$event_id)
aux.1 <- event.att[event.att$event_id %in% names(aux[aux>1]), ]
aux.1[order(aux.1$event_id), ]
aux.2 <- ddply(aux.1, "event_id", function(sub){
  sub[1, ]
  })

event.att.red <- event.att[event.att$event_id %in% 
  setdiff(event.att$event_id, aux.1$event_id), ]

test.1 <- join(test, rbind(event.att.red, aux.2), type = "inner")

test.1$lat <- as.numeric(test.1$lat)
test.1$lng <- as.numeric(test.1$lng)

X.new <- model.matrix(formu, data = test.1)

preds <- predict(mod, newx = X.new)
preds.1 <- data.frame(user = test.1$user, event = test.1$event_id,
  pred = preds)

# ==============
# = Submission =
# ==============

sub <- preds.1[preds.1$user == "1776192", ]
preds.2 <- ddply(preds.1, "user", function(sub){
  paste(sub[order(sub$s0, decreasing = TRUE), "event"], collapse = ", ")  
  })
names(preds.2) <- c("User", "Events")
write.csv(preds.2, file = "predictions/baseline.csv")