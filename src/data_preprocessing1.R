################################################################
# Purpose: Parse the strip the event file to relevant events, store
# all files as R objects for future preprocessing
################################################################


library(reshape2)
library(plyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(data.table)
library(glmnet)
library(chron)
library(stringr)

att <- read.csv("data/event_attendees.csv", stringsAsFactors = FALSE)
events.aux <- read.csv("data/events.csv", stringsAsFactors = FALSE, 
  nrows = 10)
names.aux <- names(events.aux)

### Read the events csv sequentially and filter out those events that are NOT
### in the attendance file 

train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")

ev.ids <- union(union(att$event, train$event), test$event)
rm(att, train, test)
gc()

system.time({
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

  events <- out
})

train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")
users <- read.csv("data/users.csv", stringsAsFactors = FALSE)
users.friends <- read.csv("data/user_friends.csv", stringsAsFactors = FALSE)
event.attendees <- read.csv("data/event_attendees.csv", stringsAsFactors = FALSE)

save("train","test","users","users.friends","event.attendees","events",
  file="data/loadedData.Rdata")
