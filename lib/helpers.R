read.date <- function(date){
  date <- as.character(date)
  date <- ifelse(date == "", NA, date)
  time.aux <- strsplit(
    gsub("(\\+.*|Z)", "", date), " |T")
  time.aux <- do.call(rbind, time.aux)
  time <- chron(dates = time.aux[, 1], times = time.aux[, 2], 
    format =  c(dates = "y-m-d", times = "h:m:s"))
  time
}

countIDs <- function(col){
  col.1 <- strsplit(col, " ")
  sapply(col.1, length)
}

date.FromTimeStamp <- function(TimeStamp)
{
  resDate <- strsplit(TimeStamp,"T")  
  resDate <- as.Date(resDate[[1]][1]) 
}

hour.FromTimeStamp <- function(TimeStamp)
{
  resHour <- strsplit(TimeStamp,"T")
  resHour <- strsplit(resHour[[1]][2],":")
  resHour <- as.character(resHour[[1]][1]) 
}
