library(Metrics)

# situation there are n = 5 events in total
# the truth is the user is interested in t = 3 of them
# we set k = 200 but it does not matter since the function apk sets k = min(k,length(prediction))

truth <- list(c("E1","E2","E3"))

# exact prediction
prediction <- list(c("E1","E2","E3"))
mapk(k,truth,prediction)

# the only thing that matters is that events from the truth set (interested set)
# appear in the first t places in the prediction 

# the order doesn't matter
prediction <- list(c("E2","E3","E1"))
mapk(k,truth,prediction)

# the events in the remaining positions do not matter (as long as we get all true events right)
prediction <- list(c("E2","E1","E3","E4","E5"))
mapk(k,truth,prediction)

# we have to try and place all events from the truth set as close to the first position as possible
prediction <- list(c("E2","E1","E4","E3","E5"))
mapk(k,truth,prediction)
prediction <- list(c("E2","E1","E4","E5","E2"))
mapk(k,truth,prediction)