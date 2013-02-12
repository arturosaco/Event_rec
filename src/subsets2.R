
library(plyr)
library(mgcv)
library(chron)
library(glmnet)
library(randomForest)
#######################################        
# Function        
# greedySubsetPartitioning()
#        
# INPUT: train - the train data frame containing user_id, event_id and some features in each row
#        test - the test data frame, same as train
#        min.data.perFeature - the minimum number of training examples per feature (column)
#        of train. (does not take into account the number of levels contained in a factor variable
#
#       !!train and test must both contain the same columns, starting with, user_id, event_id
#       test must be row sorted according to the number of non-empty columns per row!!
#        
#       OUTPUT: resList - a data frame containing:
#        resList$train.subset - a matrix where each row corresponds to a logical vector of length nrow(train),
#        indicating whether a particular row of train should be present in a subset
#        resList$test.subset - a matrix where each row corresponds to a logical vector of length nrow(test),
#        indicating whether a particular row of test should be present in a subset
#        resList$model.vars - matrix, where each row is a logical vector of length = ncol(test)=nrow(train),
#        indicating which variables(columns) of train and test should be present in the model
#        
#        test.unmatched - a vector containing the id's of rows from test, that could not be matched by a training subset
#        for the current threshold set by min.num.threshold, should only be non-empty for high threshold values

allVarsPresent <- function(x, want) { res <- all(want[as.logical(want)]==x[as.logical(want)]); return(res) }

allVarsContained <- function(want,x) { res <- all(want[as.logical(want)]==x[as.logical(want)]); return(res) }


evalSubsetting <- function(reslist)
{
  indices <- reslist$resList
  unmatched.count <- nrow(reslist$test.unmatched)
  test.subs.count <- rowSums(indices$test.subset)
  train.subs.count <- rowSums(indices$train.subset)
  var.count <- rowSums(indices$model.vars)
  return(list(unmatched.count=unmatched.count,test.subs.count=test.subs.count,
              train.subs.count=train.subs.count,var.count=var.count))
}

genRandomMat <- function(cols=4,rows=10,na.count=15)
{  
  t.vect <- sample(cols*rows)
  t.vect[sample(cols*rows,na.count)] <- NA
  res <- matrix(t.vect,ncol=cols)
  return(res)
}

getPresentCols <- function(x) {res <- which(!is.na(x)); return(res)}

sameFeatureRows <- function(x,present.cols)
{
  res <-   sum(!is.na(x)[present.cols])==length(present.cols)
  return(res)
}

joinSameModels <- function(resList)
{
  merged <- list()
  uniqueModels <- uniquecombs(resList$model.vars)  
  sameModelInd <- attr(uniqueModels,"index")
  
  dat <- cbind(sameModelInd,as.data.frame(resList$test.subset))
  mergeddat <- aggregate( . ~ sameModelInd, data = dat, sum)
  mergeddat$sameModelInd <- NULL 
  
  merged$resList$test.subset <- as.matrix(mergeddat)
  
  dat <- cbind(sameModelInd,as.data.frame(resList$train.subset))
  mergeddat <- aggregate( . ~ sameModelInd, data = dat, unique)
  mergeddat$sameModelInd <- NULL
  
  merged$resList$train.subset <- as.matrix(mergeddat)
  
  
  dat <- cbind(sameModelInd,as.data.frame(resList$model.vars))
  mergeddat <- aggregate( . ~ sameModelInd, data = dat, unique)
  mergeddat$sameModelInd <- NULL
  
  merged$resList$model.vars <- as.matrix(mergeddat)
  return(merged)
}



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


# train <- genRandomMat(4,10,10)
# test <- genRandomMat(4,10,10)
# bin.ind <- !is.na(test)
# test <- test[order(rowSums(bin.ind),decreasing=T),]
# min.data.perFeature <- 0.01
# 
# train <- train_model
# test <- test_model
# min.data <- 5000
#min.data.perFeature <- 100
greedySubsetPartitioning <- function(train,test,min.data)
{
  resList <- list()
  test.unmatched <- numeric()
  #create logic matrices as indicators for missing values
  bin.train <- !is.na(train)
  bin.test <- !is.na(test)
  bin.test <- bin.test*1
  bin.train <- bin.train*1
  #start by ordering the test set by number of vars
  #orderbyNumberofVars <- order(rowSums(bin.test),decreasing=T)
  #bin.test <- bin.test[orderbyNumberofVars,]
  
  totalDatapoints <- nrow(bin.test)
  #test <- test[orderbyNumberofVars,]
  orig.test <- test
  orig.test.indexer <- 1:nrow(bin.test)
  orig.test.ind <- logical()
  zero.vec <- rep(F,nrow(bin.test))
  samefeat.index.test.orig <- logical()
  
  #print(test)
  #print(train)

  #get the frequency of different variable counts in train
  varLevels <- as.numeric(names(table(rowSums(bin.train))))
  #print(varLevels)
  pb <- txtProgressBar(min = 0, max = totalDatapoints, style = 3)
  
  #   
  while (nrow(bin.test)>0){
    
    #grab the first row from test
    model.row <- bin.test[1,]
    model.vars <- which(as.logical(bin.test[1,]))
    nVars <- sum(model.row)
    
    #check for all rows having the same features in order to create a subset
    #works beacause all the variable richer rows are already removed from test
    samefeat.index.test <- apply(bin.test,1,allVarsPresent,model.row)
    
    
    #find a corresponding or a variable richer subset in the training data
    samefeat.index.train <- apply(bin.train,1,allVarsPresent,model.row)
    #print(samefeat.index.train)
    
    #check number of vars in the test row
    curVarLevel <- sum(model.row)
    #print(curVarLevel)
 
    available.training.points <- sum(samefeat.index.train)
    #print(available.training.points)
    
    
    if (available.training.points >= min.data){
      #print("CONDITION 1")    
      #print("Positive match for var config in train")
      #print("Rows from train that match:")
      #print(samefeat.index.train)
      #print("Reducing test by variables:")
      #print(sum(samefeat.index.test))
      
      
      #eliminate the subset from test data
      test.subset <- test[samefeat.index.test,]
      test <- test[!samefeat.index.test,]
      bin.test <- bin.test[!samefeat.index.test,]
      
      #store the test index in terms of the original index for the results
      orig.test.ind <- orig.test.indexer[samefeat.index.test]
      orig.test.indexer <- orig.test.indexer[!samefeat.index.test]
      samefeat.index.test.orig <- zero.vec
      samefeat.index.test.orig[orig.test.ind] <- T
      
      train.subset <- train[samefeat.index.train,]
      #FILL IN:
      # train the model
      #print("Training model")
      #print(train.subset)
      #print("Training model INDEX")
      #print(train[samefeat.index.train,])
      #print("Predicting for:")
      #print(test.subset)
      #print("Testing model INDEX")
      #print(orig.test[samefeat.index.test.orig,])
      #print("model.row")
      #print(model.row)
      #print("reduced training")
      #print(train[samefeat.index.train,as.logical(model.row)])
      #print("reduced testing")
      #print(orig.test[samefeat.index.test.orig,as.logical(model.row)])
      
      #print("ORIG INDEXING")
      #print("orig.test.indexer")
      #print(orig.test.indexer)
      #print("orig.test.ind")
      #print(orig.test.ind)
      #print("samefeat.index.test.orig")
      #print(samefeat.index.test.orig)
      #print("samefeat.index.test.orig[orig.test.ind]")
      #print(samefeat.index.test.orig[orig.test.ind])
      if (!all(complete.cases(train[as.logical(samefeat.index.train),
                                    as.logical(model.row)]))) {
        print("NA's in training subset")}
      if (!all(complete.cases(orig.test[as.logical(samefeat.index.test.orig),
                                        as.logical(model.row)]))) {
        print("NA's in testing subset")}
      
      resList$train.subset <- rbind(resList$train.subset,as.logical(samefeat.index.train))
      resList$test.subset <- rbind(resList$test.subset,as.logical(samefeat.index.test.orig))
      resList$model.vars <- rbind(resList$model.vars,as.logical(model.row))
      
      #resList[[length(resList)+1]] <- list(train.subset=train.subset,test.subset=test.subset,model.vars=model.vars)
      # if the training set does not offer any possibility to eliminate variables to find a fitting subset
      # remove the row from testing and store it in unmatched
      
    } else {
      #if (!(sum(samefeat.index.train) >= nVars*min.data.perFeature) & !(max(varLevels) < curVarLevel)){
       
      #print("CONDITION 2")

      available.varLevel <- varLevels[varLevels < curVarLevel]
      parse.varLevels <- available.varLevel[length(available.varLevel):1] 
      datapointsNOTfound <- T
      old.model.row <- model.row
      
      #print(curVarLevel)
      #print(varLevels)
      #print(available.varLevel)
      #print("parse levels")
      #print(parse.varLevels)
      # go deeper level by level until a sufficiently large block is found
      if (!is.na(parse.varLevels[1])){
        for (cLevel in parse.varLevels){      
          #print("var level")
          #print(cLevel)
          #print(bin.train)
          #print(rowSums(bin.train)==cLevel)
          cLevel.training.subset <- bin.train[rowSums(bin.train)==cLevel,]
          #dirty patch since R converts a one row matrix automatically to a vector
          if (class(cLevel.training.subset)=="numeric") {cLevel.training.subset <- matrix(cLevel.training.subset,nrow=1)}
          if (nrow(cLevel.training.subset)>1){
            #get unique var combinations for given var level
            #print(cLevel.training.subset)
            #print(nrow(cLevel.training.subset))
            rowcombin <- uniquecombs(cLevel.training.subset)
            combinCount <- attr(rowcombin,"index")
            # count frequencies of the unique variable setups we have on this var level
            tabcombinCount <- table(combinCount)
            combin.labels <- as.numeric(names(tabcombinCount))
            combin.counts <- as.numeric(tabcombinCount)
            
            #check which unique combinations are contained in the model row
            cLevel.combs.containedinModelRow <- apply(rowcombin,1,allVarsContained,model.row)
            
            #if at least one var combination is contained in model row
            if (sum(cLevel.combs.containedinModelRow) > 0){
              #pick the most frequently occuring combination contained in model row        
              present.labels <- (1:nrow(rowcombin))[cLevel.combs.containedinModelRow]
              relevant.table <- tabcombinCount[names(tabcombinCount) %in% as.character(present.labels)]
              maxtabval <- max(relevant.table)
              maxlabel <- as.numeric(names(relevant.table[relevant.table==maxtabval])[1])
              model.row <- rowcombin[maxlabel,]
              #get the number of newly available training points after variable reduction
              available.training.points <- available.training.points + maxtabval
              
              #check if the number of training points is enough
                if (available.training.points >= min.data){
                  #print("reduced match found!!!")
                  #print(model.row)
                  #print(old.model.row)
                  
                  datapointsNOTfound <- F
                  
                  #find a corresponding or a variable richer subset in the training data using the reduced.model
                  samefeat.index.train <- apply(bin.train,1,allVarsPresent,model.row)
                  
                  # find test set index
                  samefeat.index.test1 <- apply(t(apply(bin.test,1,"==",old.model.row)),1,all)
                  samefeat.index.test2 <- apply(t(apply(bin.test,1,"==",model.row)),1,all)
                  samefeat.index.test <- (samefeat.index.test1 | samefeat.index.test2)
                  
                  
                  
                  test.subset <- test[as.logical(samefeat.index.test),]
                  test <- test[!samefeat.index.test,]
                  bin.test <- bin.test[!samefeat.index.test,]
                  
                  #store the test index in terms of the original index for the results
                  #print("orig.test.ind")
                  #print(orig.test.ind)
                  #print("orig.test.indexer")
                  #print(orig.test.indexer)
                  #print(samefeat.index.test)
                  #print(samefeat.index.test.orig)
                  
                  orig.test.ind <- orig.test.indexer[as.logical(samefeat.index.test)]
                  orig.test.indexer <- orig.test.indexer[!samefeat.index.test]
                  samefeat.index.test.orig <- zero.vec
                  samefeat.index.test.orig[orig.test.ind] <- T
                               
                  #FILL IN:
                  # train the model
    
                  #print("Training model INDEX")
                  #print(train[as.logical(samefeat.index.train),])
                  
                  #print("Predicting for:")
                  #print(test.subset)
    
    
                  #print("model.row")
                  #print(model.row)
                  
                  #print("reduced training")
                  #print(train[as.logical(samefeat.index.train),as.logical(model.row)])
                  #print("reduced testing")
                  #print(orig.test[as.logical(samefeat.index.test.orig),as.logical(model.row)])
                  
                  #print("ORIG INDEXING")
                  #print("orig.test.indexer")
                  #print(orig.test.indexer)
                  #print("orig.test.ind")
                  #print(orig.test.ind)
                  #print("samefeat.index.test.orig")
                  #print(samefeat.index.test.orig)
                  #print("samefeat.index.test.orig[orig.test.ind]")
                  #print(samefeat.index.test.orig[orig.test.ind])
                  
                  if (!all(complete.cases(train[as.logical(samefeat.index.train),
                                                as.logical(model.row)]))) {
                    print("NA's in training subset")}
                  if (!all(complete.cases(orig.test[as.logical(samefeat.index.test.orig),
                                                    as.logical(model.row)]))) {
                    print("NA's in testing subset")}
                  
                  
                  resList$train.subset <- rbind(resList$train.subset,as.logical(samefeat.index.train))
                  resList$test.subset <- rbind(resList$test.subset,as.logical(samefeat.index.test.orig))
                  resList$model.vars <- rbind(resList$model.vars,as.logical(model.row))
                  #end the for loop, we have reduced the set enough
                  break
            } 
            } 
            }            
      }
      }
      #if we looped through all the combinations in training and didn't manage to find any reduced subset
      #above the threshold put it into the unmatched set and do an exhaustive search
      if (datapointsNOTfound){
        
        
        orig.test.ind <- orig.test.indexer[samefeat.index.test]
        orig.test.indexer <- orig.test.indexer[!samefeat.index.test]
        samefeat.index.test.orig <- zero.vec
        samefeat.index.test.orig[orig.test.ind] <- T
        
        test.unmatched <- rbind(test.unmatched,samefeat.index.test.orig)
        test <- test[!samefeat.index.test,]
        bin.test <- bin.test[!samefeat.index.test,]
        
      }
      # pick out the most frequent columns as candidates for the reduced feature set
      }

    #dirty patch since R converts a one row matrix automatically to a vector
    if (class(bin.test)=="numeric") {bin.test <- matrix(bin.test,nrow=1); test <- matrix(test,nrow=1)}
    setTxtProgressBar(pb, totalDatapoints-nrow(test))
    
    }
    #join test rows that have the same model
    #resList <- joinSameModels(resList)
    
    # if we had unmatched test rows, try to match all using a common subset
    #print(length(test.unmatched)>0)
    #print(test.unmatched)
    if(length(test.unmatched)>0){
      if (nrow(test.unmatched)>1) {test.unmatched <- as.logical(colSums(test.unmatched))}
      n.unmatched <- sum(test.unmatched)
      lcs <- largestCommonSubset(test.unmatched,train,orig.test,min.data)
      if (lcs$matched==T){
        resList$train.subset <- rbind(resList$train.subset,as.logical(lcs$train.subset))
        resList$test.subset <- rbind(resList$test.subset,as.logical(lcs$test.subset))
        resList$model.vars <- rbind(resList$model.vars,as.logical(lcs$model.vars))
        test.unmatched <- numeric()
        print(paste(n.unmatched," rows from train subset using largest common subset",sep=""))
      } else {
        print(paste(n.unmatched," rows from train NOT matched using largest common subset",sep=""))
      }
    }
  
    close(pb)
  
    return(list(resList=resList,test.unmatched=test.unmatched))
}

# 
#   train <- genRandomMat(4,10,10)
#   test <- genRandomMat(4,10,10)
#   bin.ind <- !is.na(test)
#   test <- test[order(rowSums(bin.ind),decreasing=T),]
#   debug(greedySubsetPartitioning)
# debug(largestCommonSubset)
#  subs <- greedySubsetPartitioning(train,test,1)
