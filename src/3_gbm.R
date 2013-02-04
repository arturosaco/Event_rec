library(ProjectTemplate)
load.project()

library(doMC)
registerDoMC(4)
getDoParWorkers()

# =============
# = Functions =
# =============

fit.gbm.model <- function(data.big, interaction.par, max.n.trees, 
    shrinkage.par, distribution.par, formu.par = formu){
  X <- model.matrix(formu.par, data = data.big)
  Y <- factor(data.big$interested.num)

  mod.gbm <- gbm.fit(x = X, y = Y,
    distribution = distribution.par, 
    n.trees = max.n.trees,
    interaction.depth = interaction.par,
    shrinkage = shrinkage.par, 
    verbose = TRUE)

  fit <- data.frame(data.big[, c("user_id", "event_id")], fit = mod.gbm$fit)
  mod.gbm
}

test.gbm.model <- function(data.small, model, range.trees){
  X.new <- model.matrix(formu, data = data.small)
  ldply(round(seq(from = range.trees[1], to = range.trees[2], length = 15)),
  function(n.trees){
    preds <- data.frame(data.small[, c("user_id", "event_id", "interested.num")],
      preds = predict(object = model, newdata = X.new, n.trees = n.trees))
    mapk.user <- ddply(preds, "user_id", function(sub){
        mapk(200, actual = sub[sub$interested.num == 1, "event_id"],
            predicted = sub[order(sub$preds, decreasing = TRUE), "event_id"])
      })
    data.frame(n.trees = n.trees, score = mean(mapk.user$V1))
  })
}

### Local TODO
# - Compare results of model with/without distance
#    compare models locally
# - Try dimensionality reduction on word counts

# ================
# = Prepare data =
# ================

load("data/stage_2.Rdata")
library(gbm)
library(Metrics)

att.yes <- read.csv(file = "data/SAS files/user-event-friendsYes.csv")
att.no <- read.csv(file = "data/SAS files/user-event-friendsNo.csv")
att.maybe <- read.csv(file = "data/SAS files/user-event-friendsMaybe.csv")
att.invited <- read.csv(file = "data/SAS files/user-event-friendsInvited.csv")

names(att.yes)[3] <- "count"
names(att.no)[3] <- "count"
names(att.maybe)[3] <- "count"
names(att.invited)[3] <- "count"
att <- rbind(data.frame(att.yes, count.type  = "yes"),
      data.frame(att.no,  count.type = "no"),
      data.frame(att.maybe, count.type  = "maybe"),
      data.frame(att.invited, count.type  = "invited"))
names(att)[names(att) == "event"] <- "event_id"
table(train$user_id %in% att$user_id)


att.c <- dcast(att, user_id + event_id ~ count.type, value.var = "count")
att.c[is.na(att.c)] <- 0
names(att.c)[names(att.c) %in% c("yes", "no", "maybe", "invited")] <-
  paste("friends", 
    names(att.c)[names(att.c) %in% c("yes", "no", "maybe", "invited")],
    sep = ".")

events$event_id <- as.numeric(as.character(events$event_id))
events$creator_id <- as.numeric(as.character(events$creator_id))

train.1 <- join(train, events)

# =================================================
# = K-fold cross validation with objective metric =
# =================================================

k <- 10
partition <- data.frame(user_id = unique(train$user_id),
 partition = sample(1:k, length(unique(train$user_id)), replace = TRUE))
formu <- as.formula(paste("interested.num ~",
    paste(c("event_seen_time",
      "event_seen_time_month", "event_seen_time_day",
      "event_start_time_year", "event_start_time_month",
      "event_start_time_day", "c_other", "distance", "birthyear",
      "gender", "joinedAt_day", "joinedAt_month", "joinedAt_year",
      "friends.yes", "friends.no", "friends.maybe", "friends.invited + "),
       collapse = " + "),
    paste("c_", 1:100, collapse = "+", sep = "")))

### Couldn't use "event_seen_time_year" since it has only one value

part.scores <- ldply(1:k, function(part.int){
  data.big <- train.1[train.1$user_id %in% 
    partition[partition$partition != part.int, "user_id"], ]
  data.small <- train.1[train.1$user_id %in% 
    partition[partition$partition == part.int, "user_id"], ]
  mod <- fit.gbm.model(data.big,
     interaction.par = 6,
     shrinkage.par = 0.001,
     max.n.trees = 7500,
     distribution = "adaboost")
  scores <- test.gbm.model(data.small, mod, range.trees = c(100, 7500))
  scores$partition <- part.int
  scores
}, .parallel = TRUE)

score.mean <- ddply(part.scores, "n.trees", summarise, score = mean(score))
ggplot(score.mean, aes(x = n.trees, y = -score)) + geom_line()

# ================
# = Single model =
# ================

mod.gbm <- fit.gbm.model(data = train.1,
  interaction.par = 6,
  shrinkage.par = 0.001,
  max.n.trees = 15000,
  distribution = "bernoulli")

train.1$event_seen_time <- as.numeric(train.1$event_seen_time)
cv.gbm <- gbm(formula = formu, data = train.1,
  distribution = "bernoulli",
  interaction.depth = 6,
  shrinkage = 0.001, 
  n.trees = 10000,
  cv.fold = 10)


X <- model.matrix(formu, data = train.2)
Y <- train.1$interested.num

grid <- data.frame(expand.grid(.n.trees = 5000)), 
  .shrinkage = c(0.1, 0.01, 0.001), .interaction.depth = 1:10))
tune.gbm <- train(x = X, y = Y, 
                 method = "gbm", 
                 tuneGrid = grid)
                 

tune.gbm <- tune(gbm, train.x = formu, data = train.1,
  shrinkage = c(0.001), interaction.depth = 1:10,
  n.trees = 10000, n.minobsinnode = 2:15, distribution = "bernoulli")

# ====================
# = Make predictions =
# ====================

test.1 <- join(test, events)
X.new <- model.matrix(formu, data = test.1)
preds <- data.frame(test.1[, c("user_id", "event_id")],
    preds = predict(object = mod.gbm, newdata = X.new, n.trees = 7500))

# ==============
# = Submission =
# ==============

preds.1 <- ddply(preds, "user_id", function(sub){
    paste(sub[order(sub$preds, decreasing = TRUE), "event_id"], collapse = ", ")  
  })

names(preds.1) <- c("User", "Events")
write.csv(preds.1, file = "predictions/gbm.csv")

# ==========================
# = Your Comment Goes here =
# ==========================

users$user_id <- as.numeric(as.character(users$user_id))
train.2 <- join(train.1, users[, c("user_id", "birthyear", "gender",
  "joinedAt_year", "joinedAt_month", "joinedAt_day")])
train.3 <- join(train.2, att.c[att.c$user_id %in% train.2$user_id & 
  att.c$event_id %in% train.2$event_id,])

train.3[, grep("friends.", names(train.3), value = TRUE)][
  is.na(train.3[, grep("friends.", names(train.3), value = TRUE)])] <- 0

train.3$birthyear <- as.numeric(as.character(train.3$birthyear))

aux.year <- data.frame(birthyear = min(train.3$birthyear, na.rm = TRUE):
  max(train.3$birthyear, na.rm = TRUE), year.group = NA)

init <- 1905
for(k in 1:nrow(aux.year)){
  if(aux.year[k, 1] %% 5 == 0){
    init <- aux.year[k, 1]
  }
  aux.year[k, 2] <- init
}
aux.year$year.group <- factor(aux.year$year.group)

train.4 <- join(train.3, aux.year)



# ==================
# = Stratified gbm =
# ==================

train.red <- na.omit(train.4[, !names(train.4) %in% c("event_city",
  "event_state", "event_zip", "event_country", "event_lat", "event_lng")])

formu.red <-  as.formula(paste("user_id ~",
    paste(c("event_seen_time",
      "event_seen_time_month", "event_seen_time_day",
      "event_start_time_year", "event_start_time_month",
      "event_start_time_day", "c_other", "distance", "birthyear",
      "gender", "joinedAt_day", "joinedAt_month",
      "friends.yes", "friends.no", "friends.maybe", "friends.invited + "),
       collapse = " + "),
    paste("c_", 1:100, collapse = "+", sep = "")))

mod.gbm.red <- mod.gbm
mod.gbm.red <- fit.gbm.model(data = train.red,
  interaction.par = 6,
  shrinkage.par = 0.001,
  max.n.trees = 15000,
  distribution = "bernoulli",
  formu.par = formu.red)

formu.comp <- as.formula(paste("user_id ~",
    paste(c("event_seen_time",
      "event_seen_time_month", "event_seen_time_day",
      "event_start_time_year", "event_start_time_month",
      "event_start_time_day", "c_other",
      "gender", "joinedAt_day", "joinedAt_month",
      "friends.yes", "friends.no", "friends.maybe", "friends.invited + "),
       collapse = " + "),
    paste("c_", 1:100, collapse = "+", sep = "")))


mod.gbm.comp <- fit.gbm.model(data = train.4,
  interaction.par = 6,
  shrinkage.par = 0.001,
  max.n.trees = 15000,
  distribution = "bernoulli",
  formu.par = formu.comp)


# ===============================
# = Make stratified predictions =
# ===============================


test.1 <- join(test, events)

test.2 <- join(test.1, users[, c("user_id", "birthyear", "gender",
  "joinedAt_year", "joinedAt_month", "joinedAt_day")])
test.3 <- join(test.2, att.c[att.c$user_id %in% test.2$user_id & 
  att.c$event_id %in% test.2$event_id,])

test.3[, grep("friends.", names(test.3), value = TRUE)][
  is.na(test.3[, grep("friends.", names(test.3), value = TRUE)])] <- 0

test.3$birthyear <- as.numeric(as.character(test.3$birthyear))

test.4 <- join(test.3, aux.year)

test.red <- test.4[!is.na(test.4$distance) & !is.na(test.4$birthyear),]
test.comp <- test.4[!test.4$user_id %in% test.red$user_id, ]

X.new.red <- model.matrix(formu.red, data = test.red)
X.new.comp <- model.matrix(formu.comp, data = test.comp)

preds.red <- data.frame(test.red[, c("user_id", "event_id")],
    preds = predict(object = mod.gbm.red, newdata = X.new.red, n.trees = 15000),
    id = "red")

preds.comp <- data.frame(test.comp[, c("user_id", "event_id")],
    preds = predict(object = mod.gbm.comp, newdata = X.new.comp, n.trees = 15000),
    id = "comp")

preds <- rbind(preds.red, preds.comp)


preds.1 <- ddply(preds, "user_id", function(sub){
    paste(sub[order(sub$preds, decreasing = TRUE), "event_id"], collapse = ", ")  
  })

names(preds.1) <- c("User", "Events")
write.csv(preds.1, file = "predictions/gbm_strat.csv")





