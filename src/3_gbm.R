library(ProjectTemplate)
load.project()


library(doMC)
registerDoMC(4)
getDoParWorkers()

# =============
# = Functions =
# =============

fit.gbm.model <- function(data.big, interaction.par, max.n.trees, 
  shrinkage.par, distribution.par){
  formu <- as.formula(paste("interested.num ~",
    paste(c("event_seen_time",
      "event_seen_time_month", "event_seen_time_day",
      "event_start_time_year", "event_start_time_month",
      "event_start_time_day", "c_other +"), collapse = " + "),
    paste("c_", 1:100, collapse = "+", sep = "")))

  ### Couldn't use "event_seen_time_year" since it has only one value

  X <- model.matrix(formu, data = data.big)
  Y <- factor(data.big$interested.num)

  mod.gbm <- gbm.fit(x = X, y = Y,
    distribution = distribution.par, 
    n.trees = max.n.trees,
    interaction.depth = interaction.par,
    shrinkage = shrinkage.par, 
    verbose = FALSE)

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
# - Implement k-fold cross validation (with the "correct" metric) to
# - Compare results of model with/without distance
#    compare models locally
# - Try dimensionality reduction on word counts

# ================
# = Prepare data =
# ================

load("data/stage_2.Rdata")
library(gbm)
library(Metrics)

events$event_id <- as.numeric(as.character(events$event_id))
events$creator_id <- as.numeric(as.character(events$creator_id))

train.1 <- join(train, events)

# =================================================
# = K-fold cross validation with objective metric =
# =================================================

k <- 5
partition <- data.frame(user_id = unique(train$user_id),
 partition = sample(1:k, length(unique(train$user_id)), replace = TRUE))


part.scores <- ldply(1:k, function(part.int){
  data.big <- train.1[train.1$user_id %in% 
    partition[partition$partition != part.int, "user_id"], ]
  data.small <- train.1[train.1$user_id %in% 
    partition[partition$partition == part.int, "user_id"], ]
  mod <- fit.gbm.model(data.big,
     interaction.par = 5,
     shrinkage.par = 0.001,
     max.n.trees = 1000,
     distribution = "adaboost")
  scores <- test.gbm.model(data.small, mod, range.trees = c(10, 1000))
  scores$partition <- part.int
  scores
}, .progress = "text")

score.mean <- ddply(part.scores, "n.trees", summarise, score = mean(score))
ggplot(score.mean, aes(x = n.trees, y = score)) + geom_line()



# ==============
# = Submission =
# ==============


ddply(fit[1:10, ], "user_id", function(sub){
    paste(sub[order(sub$fit, decreasing = TRUE), "event_id"], collapse = ", ")  
  })