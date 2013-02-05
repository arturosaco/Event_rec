# LIBRARIES
library(igraph)
library(plyr)


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
getRatio <- function(m,user1_ind, user2_ind)
{
	one <- matrix(m[m[,2] == user1_ind,],ncol=2)
	two <- matrix(m[m[,2] == user2_ind,],ncol=2)
	if ( (nrow(one)==0) || (nrow(two)==0) )
		return(NA)
	return(2*sum(is.element(one[,1],two[,1])) / (nrow(one)+nrow(two)))
}
getRatioEventsTogether <- function(user1_ind, user2_ind)
{
	yesRatio <- getRatio(yes,user1_ind, user2_ind)
	noRatio <- getRatio(no,user1_ind, user2_ind)
	maybeRatio <- getRatio(maybe,user1_ind, user2_ind)
	result <- 0
	number <- 3
	if ( is.na(yesRatio) )
	{
		number <- number - 1
		yesRatio <- 0
	}
	if ( is.na(noRatio) )
	{
		number <- number - 1
		noRatio <- 0
	}
	if ( is.na(maybeRatio) )
	{
		number <- number - 1
		maybeRatio <- 0
	}
	if ( number == 0 )
		return(0)
	return((yesRatio+noRatio+maybeRatio)/number)
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
common.interest <- apply(friends.flat.ids,1,function(x)getInterestSimilarity(x[1],x[2]))

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

common.attendance <- apply(friends.flat.ids,1,function(x)getRatioEventsTogether(x[1],x[2]))
save(distances,common.interest,common.attendance,file="data/weights.Rdata")

# Weights for the edges
distances[!is.na(distances)] <- distances[!is.na(distances)]/max(distances[!is.na(distances)])
distances[is.na(distances)] <- rep(1,sum(is.na(distances)))
weights <- distances + 0.5*(1-common.interest) + 0.5*(1-common.attendance)

# Generating the graph
g <- graph.edgelist(friends.flat.ids, directed=FALSE)

# Clustering
fc <- fastgreedy.community(g,weights=weights)
# membership(fc)
# sizes(fc)

save(g,fc,weights,file="data/graph.Rdata")

# Model
train.new <- data.frame(event_id=train$event_id)
train.new$cluster_id <- membership(fc)[match(train$user_id,users$user_id)]
train.new$interested.num <- as.numeric(as.character(train$interested.num))
train.new <- ddply(train.new,.variables=c("cluster_id","event_id"),.fun=function(x)data.frame(interested=mean(x$interested.num)),.progress="text")

test.new <- data.frame(event_id=test$event_id)
test.new$cluster_id <- membership(fc)[match(test$user_id,users$user_id)]
test.new <- unique(test.new)

# Mean coordinates for the clusters
clusters <- data.frame(cluster_id=1:length(sizes(fc)))
clusters$cluster_lat <- unlist(lapply(clusters$cluster_id,function(x)mean(users$Latitude[!is.na(users$Latitude)&membership(fc)==x])))
clusters$cluster_lng <- unlist(lapply(clusters$cluster_id,function(x)mean(users$Longitude[!is.na(users$Longitude)&membership(fc)==x])))

# Generate the data frame to use in the model
events.temp <- events[,c(1,8:113)]
events.temp$event_id <- as.numeric(as.character(events$event_id))

train.clusters <- join(train.new,clusters,type="inner")
train.clusters <- join(train.clusters,events.temp,type="inner")
test.clusters <- join(test.new,clusters,type="inner")
test.clusters <- join(test.clusters,events.temp,type="inner")

train.clusters$event_start_time_year <- as.numeric(as.character(train.clusters$event_start_time_year))
train.clusters$event_start_time_month <- as.numeric(as.character(train.clusters$event_start_time_month))

test.clusters$event_start_time_year <- as.numeric(as.character(test.clusters$event_start_time_year))
test.clusters$event_start_time_month <- as.numeric(as.character(test.clusters$event_start_time_month))

save(train.clusters,test.clusters,file="data/data_clusters.Rdata")

# Modelling
formula <- as.formula(paste("interested ~",
								  # 8:ncol(train.clusters): without coordinates
								  # 4:ncol(train.clusters): with coordinates
                                  paste(names(train.clusters)[8:ncol(train.clusters)], collapse="+")))

model <- lm(formula, data=train.clusters)
pred <- predict(model, test.clusters)