setwd("/Users/sergio/Documents/kaggle/src");

#---------------------------------------------
#-               FUNCTIONS                   -
#---------------------------------------------
split_field <-
function(field) lapply(field,function(x)unlist(strsplit(x," ")));
 
# index: 1 for yes, 2 for maybe, 3 for invited and 4 for no
count_friends <- 
function(user,event,index) sum(sapply(user$friends,function(x)is.element(x,event[index+1])));

#---------------------------------------------
#-                 SCRIPT                    -
#---------------------------------------------
# Loading CSVs
friends <- read.csv("../data/user_friends.csv", stringsAsFactors = FALSE);
att <- read.csv("../data/event_attendees.csv", stringsAsFactors = FALSE);
train <- read.csv("../data/train.csv", stringsAsFactors = FALSE);

events <- intersect(att$event,train$event);
users <- intersect(friends$user,train$user);

# Splitting fields with values separated with spaces
friends$friends <- split_field(friends$friends);
att$yes <- split_field(att$yes);
att$maybe <- split_field(att$maybe);
att$invited <- split_field(att$invited);
att$no <- split_field(att$no);

# Indices for (user,event)
users_events <- data.frame(user_id=rep(1:length(friends$user),length(att$event)),
                           event_id=unlist(lapply(1:length(att$event),function(x)rep(x,length(friends$user)))));

time_start <- Sys.time()
print(time_start)
result <-
apply(users_events,1,function(x)
					   c(friends[x[1],]$user,
                         att[x[2],]$event,
                         # yes
                         count_friends(friends[x[1],],att[x[2],],1),
                         # maybe
                         count_friends(friends[x[1],],att[x[2],],2),
                         # invited
                         count_friends(friends[x[1],],att[x[2],],3),
                         # no
                         count_friends(friends[x[1],],att[x[2],],4)
                        )
     );
time_end <- Sys.time()
print(time_end)
print(time_end-time_start)