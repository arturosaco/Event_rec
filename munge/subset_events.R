
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
putNAs <- function(var){
    var <- as.character(var)
    var[var == ""] <- NA
    var
  }
# for(int in c("country", "city", "state", "zip")){
#   out[, int] <- putNAs(out[, int])
# }  
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
  for(int in c(grep("c_", names(out), value = TRUE))){
    out[, int] <- as.numeric(out[, int])
  }
  events <- out
})
