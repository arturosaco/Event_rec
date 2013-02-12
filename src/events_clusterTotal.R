# This file looks at clusters of events from train.csv in which a user was interested
# Each two events that had interested==1 for the same user are connected by an edge
# The edges are weighted by the number of times(users) they were JOINTLY considered as interesting
#
# MEANING:
# which means that events grouped compactly represent clusters where, given a user is interested
# in one event in the cluster, he would probably like all the event from the cluster
#
# OUTPUT
# cluster.eventsTrain.byInterest containing event_id and cluster label


library(igraph)
library(mgcv)

load("data/stage_2.Rdata")

#events clusters by interest 
train$interested.num <- as.numeric(as.character(train$interested.num))

#take only rows where user is interested in event
train.interested <- train[train$interested.num == 1,]
train.interested <- train.interested[,1:2]


train.interested <- train.interested[ order(train.interested[,1]), ]

#only keep rows where a user was interested in multiple events
tabusers <- table(train.interested$user_id)
listofusers <- as.numeric(names(tabusers)[as.numeric(tabusers) > 1 ])
train.interested <- train.interested[train.interested$user_id %in% listofusers,]

#order the events subset for each user to assure we get the same combinations for each user
#when we create combinations out of them
train.interested <- train.interested[ order(train.interested$user_id),]
train.interested$event_id <- do.call(c,tapply(train.interested$event_id,train.interested$user_id,sort))

events.graph.mat <- t(do.call(cbind,tapply(train.interested$event_id,train.interested$user_id,combn,2)))
save("events.graph.mat",file="data/events.graph.mat.int.RData")
#load("events.graph.mat.RData")
combs <- uniquecombs(events.graph.mat)
combs.ind <- attr(combs,"index")
count.combs <- as.numeric(table(combs.ind))

boxplot(as.numeric(count.combs))
weights <- count.combs
summary(weights)
combs <- as.data.frame(cbind(combs,weights))
colnames(combs) <- c("V1","V2","weight")

#create att edge list
load(file = "cache/att.1.RData")
att.yes <- att.1[att.1$int.num == 2, ]
att.yes$int.num <- NULL
names(att.yes) <- c("event_id", "user_id")
att.yes <- att.yes[,2:1]

att.yes <- att.yes[ order(att.yes[,1]), ]

#only keep rows where a user was interested in multiple events
tabusers <- table(att.yes$user_id)
listofusers <- as.numeric(names(tabusers)[as.numeric(tabusers) > 1 ])
att.yes <- att.yes[att.yes$user_id %in% listofusers,]

#order the events subset for each user to assure we get the same combinations for each user
#when we create combinations out of them
att.yes <- att.yes[ order(att.yes$user_id),]
att.yes$event_id <- do.call(c,tapply(att.yes$event_id,att.yes$user_id,sort))

events.graph.mat.att <- t(do.call(cbind,tapply(att.yes$event_id,att.yes$user_id,combn,2)))
save("events.graph.mat.att",file="data/events.graph.mat.att.RData")
#load("events.graph.mat.RData")
combs.att <- uniquecombs(events.graph.mat.att)
combs.att.ind <- attr(combs.att,"index")
count.combs.att <- as.numeric(table(combs.att.ind))

boxplot(as.numeric(count.combs.att))
weights.att <- count.combs.att
summary(weights.att)
combs.att <- as.data.frame(cbind(combs.att,weights.att))
colnames(combs.att) <- c("V1","V2","weight")

#combine edge lists
colnames(combs)[3] <- "w1"
colnames(combs.att)[3] <- "w2"
cluster.data <- join(combs,combs.att,by=c("V1","V2"),type="full")

cluster.data[is.na(cluster.data[,3]),3] <- rep(0,sum(is.na(cluster.data[,3])))
cluster.data[is.na(cluster.data[,4]),4] <- rep(0,sum(is.na(cluster.data[,4])))

cluster.data$weight <- cluster.data$w1 + cluster.data$w2
cluster.data$w1 <- NULL
cluster.data$w2 <- NULL


G <- graph.data.frame(cluster.data,directed=FALSE)

#plot
b <- betweenness(G)
V(G)$size <- 10*b/max(b)

# Clustering
#example from: http://www.sixhat.net/finding-communities-in-networks-with-r-and-igraph.html
#using  Grivan-Newman algorithm
# ebc <- edge.betweenness.community(G, directed=F)
# 
# # Now we have the merges/splits and we need to calculate the modularity
# # for each merge for this we'll use a function that for each edge
# # removed will create a second graph, check for its membership and use
# # that membership to calculate the modularity
# mods <- sapply(0:ecount(G), function(i){
#   G2 <- delete.edges(G, ebc$removed.edges[seq(length=i)])
#   cl <- clusters(G2)$membership
#   modularity(G2,cl)
# })
# 
# # we can now plot all modularities
# plot(mods, pch=20)
# 
# # Now, let's color the nodes according to their membership
# G2<-delete.edges(G, ebc$removed.edges[seq(length=which.max(mods)-1)])
# V(G)$color=clusters(G2)$membership
# 
# # Let's choose a layout for the graph
# G$layout <- layout.fruchterman.reingold
# 
# # plot it
# plot(G, vertex.label=NA)

fc <- fastgreedy.community(G)

save(b,G,fc,file="data/events_graph.Rdata")

#plot
com<-community.to.membership(G, fc$merges, steps= which.max(fc$modularity)-1)
V(G)$color <- com$membership+1
G$layout <- layout.fruchterman.reingold
plot(G, vertex.label=NA)


#PCA using some graph metrics
cent<-data.frame(deg=degree(G), clo=closeness(G), bet=betweenness(G),eig=evcent(G)$vector)
head(cent)
cor(cent)
z <- scale(cent)
res <- princomp(z)
summary(res)
biplot(res,cex=0.5)


# get labels
event_clusters <- membership(fc)
cluster.eventsTrain.byJointInterest <- cbind(as.numeric(names(event_clusters)),as.numeric(event_clusters))
colnames(cluster.eventsTrain.byJointInterest) <- c("event_id","clusterByJointInterest")
save("cluster.eventsTrain.byJointInterest",file="data/cluster.eventsTrain.byJointInterest.RData")

