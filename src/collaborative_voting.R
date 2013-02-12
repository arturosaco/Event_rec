load("data/graph.Rdata")
load("data/event_graph.Rdata")


community.label 
#for each user community


#split into users in train.csv(train a separate model) and not in train.csv(use general model)

#create a training set for users from training.csv

#train a model with the training set

#for each user from test.csv in the community fetch their events -> predict for these

#get predictions for users in train.csv

#get predictions for users not in train.csv

#combine predictions and calculate community average