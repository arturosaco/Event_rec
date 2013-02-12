library(plyr)
library(mgcv)
library(chron)
library(glmnet)
library(randomForest)
library(kernlab)
library(igraph)

setwd("f:/UCL/Applied ML/Event Project/Event_rec/")
load("data/stage_2.Rdata")
load("data/events_graph.Rdata")
load("data/graph.RData")

#get graph clusters
load("data/events_graph.Rdata")
events.fc <- fc

load("data/graph.RData")
users.fc <- fc

event_clusters <- membership(events.fc)
cluster.events <- as.data.frame(cbind(as.numeric(names(event_clusters)),as.numeric(event_clusters)))
colnames(cluster.events) <- c("event_id","eventcluster")

user_clusters <- membership(users.fc)
cluster.user <- as.data.frame(cbind(as.numeric(users$user_id),as.numeric(user_clusters)))
colnames(cluster.user) <- c("user_id","usercluster")


train <- join(train,cluster.events,by="event_id",type="left")
test <- join(test,cluster.events,by="event_id",type="left")
train <- join(train,cluster.users,by="user_id",type="left")
test <- join(test,cluster.users,by="user_id",type="left")


getUserCommunity <- function(user_id,users)
{
com <- users$user_id[users$usercluster == getCluster(user_id,users)]  
return(com)
}

getUserCluster <- function(user_id,users)
{
cluster <- users$usercluster[users$user_id == user_id]
return(cluster)
}

getEventCommunity <- function(event_id,cluster.events)
{
  com <- cluster.events$event_id[cluster.events$eventcluster == getEventCluster(event_id,cluster.events)]  
  return(com)
}

getEventCluster <- function(event_id,cluster.events)
{
  cluster <- cluster.events$eventcluster[cluster.events$event_id == event_id]
  return(cluster)
}

getInterestcounts <- function(x,train,cluster.user,cluster.events)
{
  nUsersperClust <-  length(getUsers(x[1],cluster.user))
  nEventsperClust <- length(getEvents(x[2],cluster.events))
  # pick all lines from train where both come up and count number of interested
  noOfInterested <- sum(as.numeric(train$interested.num)[(train$usercluster == x[1]) & (train$eventcluster == x[2])],na.rm=T)
  return(c(noOfInterested,nUsersperClust,nEventsperClust))
}


getUsers <- function(user_cluster,cluster.user){return(cluster.user$user_id[cluster.user$usercluster==user_cluster])}
getEvents <- function(event_cluster,cluster.event){return(cluster.event$event_id[cluster.event$eventcluster==event_cluster])}

communities <- cbind(test$usercluster,test$eventcluster)
communities <- communities[complete.cases(communities),]
communities <- uniquecombs(communities)
colnames(communities) <- c("usercluster","eventcluster")

int.counts <- t(apply(communities,1,getInterestcounts,train,cluster.user,cluster.events))
colnames(int.counts) <- c("clusterInterestCount","userclustersize","eventclustersize")

int.votes <- as.data.frame(cbind(communities,int.counts))

#save("int.votes",file="data/communityInterest.RData")

#test_new <- join(test,int.votes,by=c("usercluster","eventcluster"))
#train_new <- join(train,int.votes,by=c("usercluster","eventcluster"))


