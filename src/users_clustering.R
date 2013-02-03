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

getDistanceUsers <- function(user1_ind, user2_ind)
{
	return(getDistanceLatLong(users$Latitude[user1_ind],users$Longitude[user1_ind],users$Latitude[user2_ind],users$Longitude[user2_ind]));
}


#SCRIPT
# Extract friends matrix
users.friends$friends <- split_field(users.friends$friends)
save(users.friends,file="data/friends_split.RData")
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

# Geographical distances for vertices in the graph
distances <- apply(friends.flat.ids,1,function(x)getDistanceUsers(x[1],x[2]))
# Deleting relationships with unknown distance
distances <- distances[!is.na(distances)] + 1e-3
friends.flat.ids <- friends.flat.ids[!is.na(distances),]
save(friends.flat,friends.flat.ids,distances,file="data/graph.RData")

# Generating the graph
g <- graph.edgelist(friends.flat.ids, directed=FALSE)

# Clustering
fc <- fastgreedy.community(g,weights=distances)
# membership(fc)
# sizes(fc)