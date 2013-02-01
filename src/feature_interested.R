# Functions to compute the number of users in the training file
# interested in the events received
library(plyr)

featureCountInterested <- function(event_ids)
{
	frame_ids <- data.frame(event_id=event_ids)
	subset <- merge(train[train$interested==1,],frame_ids)
	int <- count(subset,vars="event_id")
	return(data.frame(event_id=int$event_id,num_int=int$freq))
}

featureCountNotInterested <- function(event_ids)
{
	frame_ids <- data.frame(event_id=event_ids)
	subset <- merge(train[train$interested==0,],frame_ids)
	notint <- count(subset,vars="event_id")
	return(data.frame(event_id=notint$event_id,num_notint=notint$freq))
}


# Code to include the features in the training set
int <- featureCountInterested(train$event_id)
notint <- featureCountNotInterested(train$event_id)

train <- merge(train,int)
train <- merge(train,notint)