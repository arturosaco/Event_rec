library(ProjectTemplate)
load.project()

p.resp <- read.csv("data/public_leaderboard_solution.csv")
load("data/stage_2.Rdata")
p.resp$Usage <- NULL
p.resp$interested.num <- 1
names(p.resp) <- c("user_id", "event_id", "interested.num")
test.1 <- join(test, p.resp)
test.1[is.na(test.1$interested.num), "interested.num"] <- 0