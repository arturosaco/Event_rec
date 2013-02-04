# LIBRARIES
library(igraph)


#FUNCTIONS
split_field <- function(field) lapply(field,function(x)unlist(strsplit(x," ")));

matrixFriends <- function(user_id,listFriends)
{
	col1 <- matrix(rep(user_id,length(listFriends)),ncol=1)
	col2 <- matrix(listFriends,ncol=1)
	return(cbind(col1,col2))
}

# This function uses the function getDistance in file distances.r
getDistanceUsers <- function(user1_ind, user2_ind)
{
	return(getDistanceLatLong(users$Latitude[user1_ind],users$Longitude[user1_ind],users$Latitude[user2_ind],users$Longitude[user2_ind]));
}

# Users interests similarity
getInterestSimilarity <- function(user1_ind, user2_ind)
{
	user1 <- train$user_id==users$user_id[user1_ind];
	user1.events <- data.frame(event_id=train$event_id[user1],interest1=train$interested.num[user1])
    user2 <- train$user_id==users$user_id[user2_ind];
	user2.events <- data.frame(event_id=train$event_id[user2],interest2=train$interested.num[user2])
	join.events <- join(user1.events, user2.events, by="event_id", type="full")
	if ( nrow(join.events) == 0 )
		return(0)
	return(sum(!is.na(join.events$interest1) &
	                  !is.na(join.events$interest2) &
	                  (join.events$interest1==join.events$interest2))
	           /nrow(join.events))
}

# Common events
getEventsTogether <- function(user1_ind, user2_ind)
{
	yes1 <- yes[yes[,2] == user1_ind,]
	yes2 <- yes[yes[,2] == user2_ind,]
	yesRatio <- sum(!is.element(yes1,yes2)) / (nrow(yes1)+nrow(yes2))
	return(yesRatio)
}

#SCRIPT
# Extract friends matrix
load("data/stage_2.Rdata")
users.friends$friends <- split_field(users.friends$friends)
save(users.friends,file="data/friends_split.Rdata")
friends.flat <- NULL
for (i in 1:nrow(users.friends))
{
	print(i)
	l <- unlist(users.friends$friends[i]) 
	listFriends <- l[is.element(l,users$user_id)]
	friends.flat <- rbind(friends.flat,matrixFriends(users.friends$user[i],listFriends))
}
friends.flat[,1] <- as.numeric(friends.flat[,1])
friends.flat[,2] <- as.numeric(friends.flat[,2])
friends.flat.ids <- apply(friends.flat,2,function(x)match(x,users$user_id))
friends.flat.ids <- t(apply(friends.flat.ids,1,sort))
friends.flat.ids <- unique(friends.flat.ids)
save(friends.flat,friends.flat.ids,file="data/friends_flat.Rdata")

# Geographical distances for vertices in the graph
distances <- apply(friends.flat.ids,1,function(x)getDistanceUsers(x[1],x[2]))
# Deleting relationships with unknown distance
#friends.flat.ids <- friends.flat.ids[!is.na(distances),]
#distances <- distances[!is.na(distances)]

# Ratio of events with the same interest
ratio <- apply(friends.flat.ids,1,function(x)getInterestSimilarity(users$user_id[x[1]],users$user_id[x[2]]))

# Events together
event.attendees$yes <- split_field(event.attendees$yes)
event.attendees$maybe <- split_field(event.attendees$maybe)
event.attendees$no <- split_field(event.attendees$no)
save(event.attendees,file="data/eventatt_split.Rdata")
yes <- NULL
no <- NULL
maybe <- NULL
for (i in 1:nrow(event.attendees))
{
	print(i)
	l <- unlist(event.attendees$yes[i]) 
	list <- l[is.element(l,users$user_id)]
	yes <- rbind(yes,matrixFriends(event.attendees$event_id[i],list))
	l <- unlist(event.attendees$no[i]) 
	list <- l[is.element(l,users$user_id)]
	no <- rbind(no,matrixFriends(event.attendees$event_id[i],list))
	l <- unlist(event.attendees$maybe[i]) 
	list <- l[is.element(l,users$user_id)]
	maybe <- rbind(maybe,matrixFriends(event.attendees$event_id[i],list))
}
yes[,2] <- match(yes[,2],users$user_id)
no[,2] <- match(no[,2],users$user_id)
maybe[,2] <- match(maybe[,2],users$user_id)
save(yes,no,maybe,file="data/eventatt_flat.Rdata")



save(distances,ratio,common.events,file="data/weights.Rdata")


common.events <- apply(friends.flat.ids,1,)

# Weights for the edges
weights <- distances/max(distances) + (1-ratio) + 1e-5

# Generating the graph
g <- graph.edgelist(friends.flat.ids, directed=FALSE)

# Clustering
fc <- fastgreedy.community(g,weights=weights)
# membership(fc)
# sizes(fc)

save(friends.flat.ids,distances,ratio,fc,file="data/graph.Rdata")

# Model
train.clusters <- train
train.clusters$cluster_id <- membership(fc)[match(train$user_id,users$user_id)]
train.clusters$interested.num <- as.numeric(as.character(train.clusters$interested.num))

train.2 <- ddply(train.clusters,.variables=c("cluster_id","event_id"),.fun=function(x)data.frame(interested=mean(x$interested.num)),.progress="text")


apply()