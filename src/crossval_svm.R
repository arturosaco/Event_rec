library(plyr)
library(mgcv)
library(chron)
library(glmnet)
library(randomForest)
library(kernlab)
library(RRF)
library(igraph)
library(Metrics)

getPredfromMatrix <- function(results){  
  for (cModelpred in 3:ncol(results)){
    preds <- ddply(results[,c(1,2,cModelpred)],
                   "user_id", function(sub){
                     paste(sub[order(sub[,3], decreasing = TRUE), "event_id"],
                           collapse = ", ")})  
    names(preds) <- c("User", "Events")
  }
  return(preds)
}


allVarsPresent <- function(x, want) { res <- all(want[as.logical(want)]==x[as.logical(want)]); return(res) }

allVarsContained <- function(want,x) { res <- all(want[as.logical(want)]==x[as.logical(want)]); return(res) }

largestCommonSubset <- function(test.unmatched,train,test,min.data)
{
  #print(test.unmatched)
  #drop all the variables which for which we don't have sufficient training points
  nTest <- sum(test.unmatched)
  test <- test[as.logical(test.unmatched),]
  bin.train <- !is.na(train)
  bin.test <- !is.na(test)
  countsPerVarTrain <- colSums(bin.train)
  countsPerVarTest <- colSums(bin.test)
  
  model.row <- (countsPerVarTest==nTest)
  model.row.train <- (countsPerVarTrain>min.data)
  model.row <- (model.row & model.row.train)
  #print(model.row)
  samefeat.index.train <- apply(bin.train,1,allVarsPresent,model.row)
  if(sum(model.row)>0 & sum(samefeat.index.train)>min.data){
    train.subset <- samefeat.index.train
    test.subset <- test.unmatched
    model.vars <- model.row
    matched <- T
    return(list(train.subset=train.subset,
                test.subset=test.subset,
                model.vars=model.vars,
                matched=matched))
  } else {
    matched <- F
    return(list(matched=matched))
  }
}


#setwd("/media/2067-6665/UCL/Applied ML/Event Project/Event_rec/")
setwd("f:/UCL/Applied ML/Event Project/Event_rec/")

source("../../devel_event/subsets2.R")
load("data/stage_2.Rdata")

#get graph clusters
load("data/events_graph.Rdata")
events.fc <- fc

load("data/graph.RData")
users.fc <- fc

event_clusters <- membership(events.fc)
cluster.events <- as.data.frame(cbind(as.numeric(names(event_clusters)),as.numeric(event_clusters)))
colnames(cluster.events) <- c("event_id","eventcluster")

users_clusters <- membership(users.fc)
cluster.users <- as.data.frame(cbind(as.numeric(users$user_id),as.numeric(users_clusters)))
colnames(cluster.users) <- c("user_id","usercluster")

users <- join(users,cluster.users,by="user_id",type="left")
train <- join(train,cluster.events,by="event_id",type="left")
test <- join(test,cluster.events,by="event_id",type="left")

#get SAS counts


att.counts <- read.csv(file = "data/SAS files/user_attendance_counts.csv")
att.friends <- read.csv(file = "data/SAS files/user-event-friendsCounts.csv")
colnames(att.friends)[2] <- "event_id"

train <- join(train,att.counts,by="user_id",type="left")
test <- join(test,att.counts,by="user_id",type="left")
train <- join(train,att.friends,by=c("user_id","event_id"),type="left")
test <- join(test,att.friends,by=c("user_id","event_id"),type="left")




#TODO: shuffle into preprocessing in stage_3
users$joinedAt <- as.Date(users$joinedAt)
users$joinedAt_day <- as.factor(users$joinedAt_day)
users$timezone <- NULL
users$timezone_dict <- NULL

#create response var vector and remove to column so that it's identical with test
interested.num.full <- train$interested.num
train <- train[,-8]

test1 <- join(test,users,type="left")
train1 <- join(train,users,type="left")
test2 <- join(test1,events,type="left")
train2 <- join(train1,events,type="left")


#remove any empty rows
test2 <- test2[rowSums(is.na(test2)) != ncol(test2),]
train2 <- train2[rowSums(is.na(train2)) != ncol(train2),]

#sapply(train2, function(x)length(unique(x)))


# get id's so we can build the final prediction vectors (it's important that test was sorted before!)
test_model_id.full <- test2[,c(1,2)]
train_model_id.full <- train2[,c(1,2)]


#remove unwanted columns (at this point we don't care about the levels of factors)
unwanted.cols <- c("creator_id","user_id", "event_id","unmatched_string","locale","event_zip")
train_model.full <- train2[,!(colnames(train2) %in% unwanted.cols)]
test_model.full <- test2[,!(colnames(test2) %in% unwanted.cols)]

#sort test - required by the subset function
bin.test.full <- !is.na(test_model)
bin.test.full <- bin.test*1
#start by ordering the test set by number of vars
orderbyNumberofVars <- order(rowSums(bin.test.full),decreasing=T)
test_model.full <- test_model.full[orderbyNumberofVars,]

orderbyNumberofVarsCols <- order(colSums(bin.test.full),decreasing=T)
test_model.full <- test_model.full[,orderbyNumberofVarsCols]
train_model.full <- train_model.full[,orderbyNumberofVarsCols]

nValidation <- 10
nModels <- 5
mapk.res <- matrix(0,nrow=nValidation,ncol=nModels)

for (cValidationRun in 1:10){
  
  ##########################################
  # pick a new subset from training
  ##########################################
  
  userlist <- unique(train_model_id.full$user_id)
  test_users_ids <- sample(userlist,round(length(userlist)*0.3))
  testintrain.index <- (train_model_id.full$user_id %in% test_users_ids)
  
  test_model <- train_model.full[testintrain.index,]
  train_model <- train_model.full[!testintrain.index,]
  y.test <- interested.num.full[testintrain.index]
  interested.num <- interested.num.full[!testintrain.index]
  test_model_id <- train_model_id.full[testintrain.index,]
  train_model_id <- train_model_id.full[!testintrain.index,]
  
  truth <- cbind(test_model_id,y.test)
  colnames(truth) <- c("user_id","event_id","truth")
  
  truth.list <- getPredfromMatrix(truth)
  
  
  ##########################################
  # run the modelling as usually
  ##########################################
  
  #subs <- greedySubsetPartitioning(train_model,test_model,8000)
  lcs <- largestCommonSubset(rep(1,nrow(test_model)),train_model,test_model,6000)
  subs$resList$train.subset <- as.data.frame(matrix(lcs$train.subset,nrow=1))
  subs$resList$test.subset <- as.data.frame(matrix(lcs$test.subset,nrow=1))
  subs$resList$model.vars <- as.data.frame(matrix(lcs$model.vars,nrow=1))
  subs$unmatched.test <- numeric()
  
  
  #get a summary of the created subsetting
  eval.subs <- evalSubsetting(subs)
  eval.subs
  
  #merge subsets with only one test row to general model
  if (!all(eval.subs$test.subs.count>1)){
    onlyonetest <- (eval.subs$test.subs.count==1)
    if(sum(onlyonetest)>1){
      subs$resList$test.subset[length(eval.subs$test.subs.count),] <- as.logical(subs$resList$test.subset[length(eval.subs$test.subs.count),] + colSums(subs$resList$test.subset[onlyonetest,]))
    }else{
      subs$resList$test.subset[length(eval.subs$test.subs.count),] <- as.logical(subs$resList$test.subset[length(eval.subs$test.subs.count),] + subs$resList$test.subset[onlyonetest,])
    }
    subs$resList$test.subset <- subs$resList$test.subset[-which(onlyonetest),]
    subs$resList$train.subset <- subs$resList$train.subset[-which(onlyonetest),]
    subs$resList$model.vars <- subs$resList$model.vars[-which(onlyonetest),]
    eval.subs <- evalSubsetting(subs)
    eval.subs
  }
  
  
  
  #take all the unmatched rows and rows which don't have enough training examples and repackage them
  # into a secondary testing set
  
  
  
  # MODELLING
  
  #n.models is the number of subsets - each subset we train and predict a new model
  n.models <- nrow(subs$resList$train.subset)
  #pred.types contains number of algorithms we would like to use
  pred.types <- 5
  preds <- matrix(0,nrow=nrow(test_model),ncol=pred.types)
  print(paste("Calculating for subsets: ",n.models,sep=""))
  for (cModel in 1:n.models){
    print(cModel)
    ##########################################################
    # Create the model matrix
    ##########################################################
    X.train <- numeric()
    X.test <- numeric()
    #create subsets with the corresponding logical vector
    binindex.train <- as.logical(subs$resList$train.subset[cModel,])
    binindex.test <- as.logical(subs$resList$test.subset[cModel,])
    feature.setup <- as.logical(subs$resList$model.vars[cModel,])
    
    #get the subset from model data
    train_sel <- train_model[binindex.train,feature.setup]
    test_sel <- test_model[binindex.test,feature.setup]
    if (sum(colSums(is.na(train_sel))) > 1) {stop("NA's in train sel")}
    if (sum(colSums(is.na(test_sel))) > 1) {stop("NA's in test sel")}
    
    
    factcount.train <- sapply(train_sel, function(x)length(unique(x)))
    factcount.test <- sapply(test_sel, function(x)length(unique(x)))
    #remove variables with only one factor in them so we can build the model matrix
    if(nrow(test_sel) > 1){
      test_clean <- test_sel[, factcount.test > 1] 
      X.test <- model.matrix(~., data = test_clean)
      X.test <- X.test[,(apply(X.test,2,function (x) length(unique(x)))>1)]
    }else{
      test_clean <- test_sel
      X.test <- test_clean
    }
    train_clean <- train_sel[,factcount.train > 1]  
    
    #create the model matrix
    X.train <- model.matrix(~., data = train_clean)
    
    #remove dummies containing a single value (all zeros)
    X.train <- X.train[,(apply(X.train,2,function (x) length(unique(x)))>1)]
    
    
    # remove dummies not present in both variables
    # we have to keep an interesect of the factor levels from both sets and drop the rest
    features.train <- colnames(X.train)
    features.test <- colnames(X.test)
    common.features <- intersect(features.test,features.train)
    X.train <- X.train[,colnames(X.train) %in% common.features]
    X.test <- X.test[,colnames(X.test) %in% common.features]
    
    #remove empty strings from colnames so that we can build formulas automatically
    colnames(X.train) <- gsub("\\s","",colnames(X.train))
    colnames(X.test) <- gsub("\\s","",colnames(X.test))
    colnames(X.train) <- gsub("[']","",colnames(X.train))
    colnames(X.test) <- gsub("[']","",colnames(X.test))
    colnames(X.train) <- gsub("[,]","",colnames(X.train))
    colnames(X.test) <- gsub("[,]","",colnames(X.test))
    # colnames(X.train) <- gsub("[,]","",colnames(X.train))
    # colnames(X.test) <- gsub("[,]","",colnames(X.test))
    
    
    # get the response vector
    y <- interested.num[binindex.train] 
    
    ##########################################################
    # Training and prediction using different algos
    ##########################################################
    
    mod.ksvm <- ksvm(x=X.train,y=y,prob.model=T,C=1,eps=0.1)
    ksvm.preds <- predict(mod.ksvm,X.test,type="prob")
    preds[binindex.test,1] <- ksvm.preds[,2]
    mod.ksvm <- ksvm(x=X.train,y=y,prob.model=T,C=0.5,eps=0.1)
    ksvm.preds <- predict(mod.ksvm,X.test,type="prob")
    preds[binindex.test,2] <- ksvm.preds[,2]
    mod.ksvm <- ksvm(x=X.train,y=y,prob.model=T,C=0.1,eps=0.1)
    ksvm.preds <- predict(mod.ksvm,X.test,type="prob")
    preds[binindex.test,3] <- ksvm.preds[,2]
    mod.ksvm <- ksvm(x=X.train,y=y,prob.model=T,C=0.05,eps=0.1)
    ksvm.preds <- predict(mod.ksvm,X.test,type="prob")
    preds[binindex.test,4] <- ksvm.preds[,2]
    mod.ksvm <- ksvm(x=X.train,y=y,prob.model=T,C=0.01,eps=0.1)
    ksvm.preds <- predict(mod.ksvm,X.test,type="prob")
    preds[binindex.test,5] <- ksvm.preds[,2]
    
    
  }
  
  
  results <- cbind(test_model_id, preds)
  results <- as.data.frame(results)
  
  
  colnames(results)[3] <- "cv1"
  colnames(results)[4] <- "cv2"
  colnames(results)[5] <- "cv3"
  colnames(results)[6] <- "cv4"
  colnames(results)[7] <- "cv5"
  
  for (cMod in 3:ncol(results)){
    preds.list <- getPredfromMatrix(results[,c(1,2,cMod)])
    mapk.res[cValidationRun,cMod-2] <- mapk(k=200,truth.list$Events ,preds.list$Events)
  }
  
}


mapk.svm <- mapk.res
#avg.mapk.lasso_01to0005 <- colMeans(mapk.lasso)
avg.mapk.svm <- colMeans(mapk.rrf)
plot(avg.mapk.svm,typ="l")
#matplot(t(rbind(avg.mapk.rrf_030005,avg.mapk.lasso_01to0005)),typ="l")