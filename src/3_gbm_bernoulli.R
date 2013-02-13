library(ProjectTemplate)
load.project()

# library(doMC)
# registerDoMC(4)
# getDoParWorkers()

# =============
# = Functions =
# =============
options(na.action = na.pass)
fit.gbm.model <- function(data, interaction.par, max.n.trees, 
    shrinkage.par, distribution.par, formu.par = formu){  
  X <- model.matrix(formu.par, data = data, options(na.action = "na.pass"))
  Y <- as.numeric(as.character(data$interested.num))
  gc()
  col.index <- apply(X, 2, function(x){
    out <- FALSE
    ind.aux <- length(unique(x))
    out <- ind.aux > 1
    if(ind.aux == 2 & sum(is.na(unique(x))) > 0){
      out <- FALSE
    }
    out
  })
  print(ncol(X[, col.index]))
  mod.gbm <- gbm.fit(x = X[, col.index], y = Y,
    distribution = distribution.par, 
    n.trees = max.n.trees,
    interaction.depth = interaction.par,
    shrinkage = shrinkage.par, 
    verbose = TRUE)
  fit <- data.frame(data[, c("user_id", "event_id")], fit = mod.gbm$fit)
  list(mod = mod.gbm, vars.index = col.index)
}

test.gbm.model <- function(data, model, range.trees){
  X.new <- model.matrix(formu, data = data)
  ldply(round(seq(from = range.trees[1], to = range.trees[2], length = 15)),
  function(n.trees){
    preds <- data.frame(data[, c("user_id", "event_id", "interested.num")],
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
load(file = "cache/att.c.RData")

# att.yes <- read.csv(file = "data/SAS files/user-event-friendsYes.csv")
# att.no <- read.csv(file = "data/SAS files/user-event-friendsNo.csv")
# att.maybe <- read.csv(file = "data/SAS files/user-event-friendsMaybe.csv")
# att.invited <- read.csv(file = "data/SAS files/user-event-friendsInvited.csv")

# names(att.yes)[3] <- "count"
# names(att.no)[3] <- "count"
# names(att.maybe)[3] <- "count"
# names(att.invited)[3] <- "count"
# att <- rbind(data.frame(att.yes, count.type  = "yes"),
#       data.frame(att.no,  count.type = "no"),
#       data.frame(att.maybe, count.type  = "maybe"),
#       data.frame(att.invited, count.type  = "invited"))
# names(att)[names(att) == "event"] <- "event_id"
# table(train$user_id %in% att$user_id)


# att.c <- dcast(att, user_id + event_id ~ count.type, value.var = "count")
# att.c[is.na(att.c)] <- 0
# names(att.c)[names(att.c) %in% c("yes", "no", "maybe", "invited")] <-
#   paste("friends", 
#     names(att.c)[names(att.c) %in% c("yes", "no", "maybe", "invited")],
#     sep = ".")

# cache("att.c")
events$event_id <- as.numeric(as.character(events$event_id))
events$creator_id <- as.numeric(as.character(events$creator_id))

train.1 <- join(train, events)

users$user_id <- as.numeric(as.character(users$user_id))
names(users)[names(users) == "country"] <- "user_country"
names(users)[names(users) == "city"] <- "user_city"

levels.user_city <- c(unique(as.character(users$user_city)), "other")
levels.user_country <- c(unique(as.character(users$user_country)), "other")
levels.event_city <- c(unique(as.character(events$event_city)), "other")
levels.event_country <- c(unique(as.character(events$event_country)), "other")

train.2 <- join(train.1, users[, c("user_id", "birthyear", "gender",
  "joinedAt_month", "joinedAt_day", "user_country",
  "user_city")])
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

train.4[is.na(train.4$event_city), "event_city"] <- "other"
train.4[is.na(train.4$event_country), "event_country"] <- "other"
train.4$user_city <- as.character(train.4$user_city)
train.4$user_country <- as.character(train.4$user_country)
train.4[is.na(train.4$user_city), "user_city"] <- "other"
train.4[is.na(train.4$user_country), "user_country"] <- "other"
train.4$user_city <- factor(train.4$user_city, levels = 
  levels.user_city)
train.4$user_country <- factor(train.4$user_country, 
  levels = levels.user_country)
train.4$event_city <- factor(train.4$event_city, 
  levels = levels.event_city)
train.4$event_country <- factor(train.4$event_country, 
  levels = levels.event_country)

train.4$u.id <- 1:nrow(train.4)
load(file = "cache/pascal/evAtt.invited.labels.RData")
load(file = "cache/pascal/evAtt.maybe.labels.RData")
load(file = "cache/pascal/evAtt.no.labels.RData")
load(file = "cache/pascal/evAtt.yes.labels.RData")

evAtt.invited.labels$event_id <- rownames(evAtt.invited.labels)
evAtt.yes.labels$event_id <- rownames(evAtt.yes.labels)
evAtt.no.labels$event_id <- rownames(evAtt.no.labels)
evAtt.maybe.labels$event_id <- rownames(evAtt.maybe.labels)

evAtt.invited.labels$event_id <- as.character(as.numeric(evAtt.invited.labels$event_id))
evAtt.yes.labels$event_id <- as.character(as.numeric(evAtt.yes.labels$event_id))
evAtt.no.labels$event_id <- as.character(as.numeric(evAtt.no.labels$event_id))
evAtt.maybe.labels$event_id <- as.character(as.numeric(evAtt.maybe.labels$event_id))

groupYear <- function(birthyear){
  out <- birthyear - (birthyear %% 5)
  factor(out, seq(min(out, na.rm = TRUE), max(out, na.rm = TRUE), 
    by = 5))
}

evAtt.invited.labels[, 2] <- groupYear(as.numeric(evAtt.invited.labels[, 2]))
evAtt.yes.labels[, 2] <- groupYear(as.numeric(evAtt.yes.labels[, 2]))
evAtt.no.labels[, 2] <- groupYear(as.numeric(evAtt.no.labels[, 2]))
evAtt.maybe.labels[, 2] <- groupYear(as.numeric(evAtt.maybe.labels[, 2]))

evAtt.invited.labels[, 3] <- factor(evAtt.invited.labels[, 3])
evAtt.yes.labels[, 3] <- factor(evAtt.yes.labels[, 3])
evAtt.no.labels[, 3] <- factor(evAtt.no.labels[, 3])
evAtt.maybe.labels[, 3] <- factor(evAtt.maybe.labels[, 3])

train.5 <- join(train.4, evAtt.invited.labels)
train.6 <- join(train.5, evAtt.no.labels)
train.7 <- join(train.6, evAtt.yes.labels)
train.8 <- join(train.7, evAtt.maybe.labels)

# ============
# = Clusters =
# ============

#get graph clusters
# load("cache/events_graph.Rdata")
# events.fc <- fc

load("cache/user_graph.RData")

# event_clusters <- membership(events.fc)
# cluster.events <- as.data.frame(cbind(as.numeric(names(event_clusters)),as.numeric(event_clusters)))
# colnames(cluster.events) <- c("event_id","eventcluster")

users_clusters <- membership(fc.users)
cluster.users <- as.data.frame(cbind(as.numeric(users$user_id),as.numeric(users_clusters)))
colnames(cluster.users) <- c("user_id","usercluster")
cluster.users$usercluster <- factor(cluster.users$usercluster)

train.9 <- join(train.8, cluster.users, by="user_id", type="left")

#train.10 <- join(train.9, cluster.events, by="event_id", type="left")


# #test <- join(test,cluster.events,by="event_id",type="left")

# # ====================
# # = Cross-validation =
# # ====================

# grid <- expand.grid(interaction.par = 6, max.n.trees = 3000,
#   shrinkage.par = c(0.1))

# formu.comp <- as.formula(paste("interested.num ~",
#     paste(c("0", "event_seen_time",
#       "event_seen_time_month", "event_seen_time_day",
#       "event_start_time_month",
#       "event_start_time_day", "c_other", "event_city", "event_country",
#       "gender", "joinedAt_day", "joinedAt_month", "user_city", 
#       "user_country",
#       "friends.yes", "friends.no", "friends.maybe",
#       "friends.invited", "year.group", "InvBirthyear", "InvGender",
#       "NoBirthyear", "NoGender", "YesBirthyear", "YesGender", "MaybeBirthyear",
#       "MaybeGender", "distance", "event_lat", "event_lng +"),
#        collapse = " + "),
#     paste("c_", 1:100, collapse = "+", sep = "")))

# cv.wrap <- function(interaction.par, formu.par, max.n.trees, 
#   shrinkage.par, n.fold = 10, data){
#   data$event_seen_time <- as.numeric(data$event_seen_time)
#   data$event_city <- factor(as.character(data$event_city))
#   data$user_city <- factor(as.character(data$user_city))
#   data$usercluster <- factor(as.character(data$usercluster))
#   data$interested.num <- as.numeric(as.character(data$interested.num))
#   gbm.cv <- gbm(formula = formu.par, data = data,
#     n.trees = max.n.trees,
#     interaction.depth = interaction.par,
#     shrinkage = shrinkage.par, 
#     verbose = TRUE,
#     distribution = "bernoulli",
#     cv.folds = n.fold)
#   # data.frame(best.n = gbm.perf(gbm.cv, method = "cv"), 
#   #   error = gbm.cv$cv.error[gbm.perf(gbm.cv, method = "cv")])
#   data.frame(n = 1:max.n.trees, error = gbm.cv$cv.error)
# }

# cv.results <- ddply(grid, c("interaction.par", "shrinkage.par", "max.n.trees"),
#   function(pars){
#     cv.wrap(interaction.par = pars$interaction.par, formu.par = formu.comp,
#       max.n.trees = pars$max.n.trees, shrinkage.par = pars$shrinkage.par,
#       data = train.9)
#   }, .progress = "text")

# # load("cache/cv_results.RData")
# # cv.results.red$id <- "red"
# # cv.results.comp$id <- "comp"
# # cv.results <- rbind(cv.results.comp, cv.results.red)

# ggplot(cv.results, aes(x = n, y = (error), colour = factor(shrinkage.par),
#   group = factor(shrinkage.par))) +   geom_line()


# ggplot(cv.results, aes(x = factor(shrinkage.par), y = error, 
#   colour = factor(interaction.par), group = factor(interaction.par))) +
#   geom_line() + 
#   geom_point(aes(size = best.n)) 

# ggplot(cv.results[cv.results$interaction.par == 4, ], 
#   aes(x = best.n, y = error, colour = factor(shrinkage.par))) +
#   geom_point() + facet_wrap(~id) 
  
### Parameter selection (interpreting cv.error as error)
# interaction.depth = 6
# n.trees ~ 350-400 for reduced model
# n.trees ~ 300-350 for complete model
# shrinkage = 0.1

### Parameter selection (interpreting cv.error as MAP)
# interaction.depth = 4
# shrinkage = 0.001
# n.trees ~ 250 for reduced model
# n.trees ~ 450 for complete model

# ===========
# = Fit gbm =
# ===========

formu.comp <- as.formula(paste("user_id ~",
    paste(c("0", "event_seen_time",
      "event_seen_time_month", "event_seen_time_day",
      "event_start_time_month",
      "event_start_time_day", "c_other", "event_city", "event_country",
      "gender", "joinedAt_day", "joinedAt_month", "user_city", 
      "user_country",
      "friends.yes", "friends.no", "friends.maybe",
      "friends.invited", "year.group", "InvBirthyear", "InvGender",
      "NoBirthyear", "NoGender", "YesBirthyear", "YesGender", "MaybeBirthyear",
      "MaybeGender",  "distance", "event_lat", "event_lng +"),
       collapse = " + "),
    paste("c_", 1:100, collapse = "+", sep = "")))
rm(users.friends, event.attendees, train, train.1, train.2, train.3, train.4,
  train.5, train.6, train.7, train.8, att.c, events, g.users, users, 
  evAtt.invited.labels, evAtt.yes.labels, evAtt.no.labels, evAtt.maybe.labels,
  cluster.users, fc.users, weights.users, test)
gc()
### CV

mod.gbm.comp <- fit.gbm.model(data = train.9,
  interaction.par = 6,
  shrinkage.par = 0.1,
  max.n.trees = 3000,
  formu.par = formu.comp,
  distribution.par = "bernoulli")


mod.gbm.comp$mod <- gbm.more(mod.gbm.comp$mod, n.new.trees = 1300)
# ===============================
# = Make stratified predictions =
# ===============================

events$event_id <- as.character(events$event_id)
test$event_id <- as.character(test$event_id)

test.1 <- join(test, events)

test.2 <- join(test.1, users[, c("user_id", "birthyear", "gender",
  "joinedAt_month", "joinedAt_day","user_city","user_country")])

test.3 <- join(test.2, att.c[att.c$user_id %in% test.2$user_id & 
  att.c$event_id %in% test.2$event_id,])
test.3[, grep("friends.", names(test.3), value = TRUE)][
  is.na(test.3[, grep("friends.", names(test.3), value = TRUE)])] <- 0
test.3$birthyear <- as.numeric(as.character(test.3$birthyear))

test.4 <- join(test.3, aux.year)
test.4$user_city <- as.character(test.4$user_city)
test.4$user_country <- as.character(test.4$user_country)
test.4$event_city <- as.character(test.4$event_city)
test.4$event_country <- as.character(test.4$event_country)

test.4[is.na(test.4$event_city), "event_city"] <- "other"
test.4[is.na(test.4$event_country), "event_country"] <- "other"
test.4[is.na(test.4$user_city), "user_city"] <- "other"
test.4[is.na(test.4$user_country), "user_country"] <- "other"
test.4$user_city <- factor(test.4$user_city, 
  levels = levels.user_city)
test.4$user_country <- factor(test.4$user_country,
 levels = levels.user_country)
test.4$event_city <- factor(test.4$event_city, 
  levels = levels.event_city)
test.4$event_country <- factor(test.4$event_country, 
  levels = levels.event_country)
test.4$u.id <- 1:nrow(test.4)

test.5 <- join(test.4, evAtt.invited.labels)
test.6 <- join(test.5, evAtt.no.labels)
test.7 <- join(test.6, evAtt.yes.labels)
test.8 <- join(test.7, evAtt.maybe.labels)
test.9 <- join(test.8, cluster.users, by="user_id", type="left")


X.new <- cbind(model.matrix(formu.comp, test.9), "event_seen_time_month4" = 0)
ind.aux <- mod.gbm.comp$vars.index
rm(test.1, test.2, test.3)
gc()
#load("cache/mod.gbm.comp.RData")
preds <- data.frame(test.9[, c("user_id", "event_id")],
    preds = predict(object = mod.gbm.comp$mod, 
      newdata = X.new[, ind.aux[intersect(names(ind.aux), colnames(X.new))]],
      n.trees = 300))    

ggplot(preds, aes(x = plogis(preds))) + geom_histogram() 
preds.1 <- ddply(preds, "user_id", function(sub){
    paste(sub[order(sub$preds, decreasing = TRUE), "event_id"], collapse = ", ")  
  })

names(preds.1) <- c("User", "Events")
write.csv(preds.1, file = "predictions/gbm_bernoulli.csv")



# ==========================
# = Split the training set =
# ==========================

# train.red <- na.omit(train.4[, !names(train.4) %in% c("event_state", 
#   "event_zip", "event_lat", "event_lng")])
# train.comp <- train.4[!train.4$u.id %in% train.red$u.id, ]
# train.red.1 <- train.red[!train.red$user_id %in% 
#   intersect(train.red$user_id, train.comp$user_id), ]
# train.comp.1 <- train.4[!train.4$u.id %in% train.red.1$u.id, ]


# ==================
# = Split test set =
# ==================

# test.red <- na.omit(test.4[, !names(test.4) %in% c("event_state", 
#   "event_zip", "event_lat", "event_lng")])
# test.comp <- test.4[!test.4$u.id %in% test.red$u.id, ]
# test.red.1 <- test.red[!test.red$user_id %in% 
#   intersect(test.red$user_id, test.comp$user_id), ]
# test.comp.1 <- test.4[!test.4$u.id %in% test.red.1$u.id, ]

# X.new.red <- model.matrix(formu.red, data = test.red)
# X.new.comp <- model.matrix(formu.comp, data = test.comp)

# aux.red <- intersect(names(mod.gbm.red$vars.index), colnames(X.new.red))
# aux.comp <- intersect(names(mod.gbm.comp$vars.index), colnames(X.new.comp))

# X.new.red <- X.new.red[, aux.red]
# X.new.comp <- X.new.comp[, aux.comp]

# aux.red <- mod.gbm.red$vars.index[names(mod.gbm.red$vars.index) %in%
#  colnames(X.new.red)]
# aux.comp <- mod.gbm.comp$vars.index[names(mod.gbm.comp$vars.index) %in%
#  colnames(X.new.comp)]