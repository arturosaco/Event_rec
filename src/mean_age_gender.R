library(ProjectTemplate)
load.project()

# =============
# = Functions =
# =============

# x is the male/female vector(tupple) of an event (use in tapply)
labelMF <- function(x){
  ifelse(length(x[x=="female"])>length(x[x=="male"]),"female","male")
}

# ================
# = Data munging =
# ================

load("data/stage_2.Rdata")

## Un-list the attendance file
event.attendees.yes <- melt.ish(event.attendees[, c("event_id", "yes")])
event.attendees.no <- melt.ish(event.attendees[, c("event_id", "no")])
event.attendees.maybe <- melt.ish(event.attendees[, c("event_id", "maybe")])
event.attendees.invited <- melt.ish(event.attendees[, c("event_id", "invited")])
## well, this was slow, jeez...

##join each data.frame with users to get mean age-gender of event
users$user_id <- factor(users$user_id)
#keep those event attendees that we have user data for
event.attendees.yes <- event.attendees.yes[event.attendees.yes$user_id %in% users$user_id, ]
event.attendees.no <- event.attendees.no[event.attendees.no$user_id %in% users$user_id, ]
event.attendees.maybe <- event.attendees.maybe[event.attendees.maybe$user_id %in% users$user_id, ]
event.attendees.invited <- event.attendees.invited[event.attendees.invited$user_id %in% users$user_id, ]

#turn events to factors from numeric (to call tapply later)
event.attendees.yes$event <- factor(event.attendees.yes$event)
event.attendees.no$event <- factor(event.attendees.no$event)
event.attendees.maybe$event <- factor(event.attendees.maybe$event)
event.attendees.invited$event <- factor(event.attendees.invited$event)

#merge with users, birthyear and gender
evAtt.yes.uData <- merge(event.attendees.yes,users[,c("user_id", "birthyear", "gender", "Latitude", "Longitude")])
evAtt.no.uData <- merge(event.attendees.no,users[,c("user_id", "birthyear", "gender", "Latitude", "Longitude")])
evAtt.maybe.uData <- merge(event.attendees.maybe,users[,c("user_id", "birthyear", "gender", "Latitude", "Longitude")])
evAtt.invited.uData <- merge(event.attendees.invited,users[,c("user_id", "birthyear", "gender", "Latitude", "Longitude")])

#fix "None" label
evAtt.yes.uData$birthyear[evAtt.yes.uData$birthyear=="None"] <- NA
evAtt.no.uData$birthyear[evAtt.no.uData$birthyear=="None"] <- NA
evAtt.maybe.uData$birthyear[evAtt.maybe.uData$birthyear=="None"] <- NA
evAtt.invited.uData$birthyear[evAtt.invited.uData$birthyear=="None"] <- NA

#convert factors to numbers quickly
evAtt.yes.uData$birthyear <- as.numeric(levels(evAtt.yes.uData$birthyear))[evAtt.yes.uData$birthyear]
evAtt.no.uData$birthyear <- as.numeric(levels(evAtt.no.uData$birthyear))[evAtt.no.uData$birthyear]
evAtt.maybe.uData$birthyear <- as.numeric(levels(evAtt.maybe.uData$birthyear))[evAtt.maybe.uData$birthyear]
evAtt.invited.uData$birthyear <- as.numeric(levels(evAtt.invited.uData$birthyear))[evAtt.invited.uData$birthyear]

#order sets by event
evAtt.yes.uData <- evAtt.yes.uData[order(evAtt.yes.uData$event),]
evAtt.no.uData <- evAtt.no.uData[order(evAtt.no.uData$event),]
evAtt.maybe.uData <- evAtt.maybe.uData[order(evAtt.maybe.uData$event),]
evAtt.invited.uData <- evAtt.invited.uData[order(evAtt.invited.uData$event),]

#tapply to a)find means by ignoring NA's b)vote for male/female
evAtt.yes.labels <- data.frame(unique(evAtt.yes.uData$event),
                               tapply(evAtt.yes.uData$birthyear,evAtt.yes.uData$event,mean,na.rm=TRUE),
                               tapply(evAtt.yes.uData$gender,evAtt.yes.uData$event,labelMF),
                               tapply(evAtt.yes.uData$Latitude,evAtt.yes.uData$event,mean,na.rm=TRUE),
                               tapply(evAtt.yes.uData$Longitude,evAtt.yes.uData$event,mean,na.rm=TRUE))
colnames(evAtt.yes.labels) <- c("event_id","YesBirthyear","YesGender","YesMeanLat","YesMeanLon")

evAtt.no.labels <- data.frame(unique(evAtt.no.uData$event),
                              tapply(evAtt.no.uData$birthyear,evAtt.no.uData$event,mean,na.rm=TRUE),
                              tapply(evAtt.no.uData$gender,evAtt.no.uData$event,labelMF),
                              tapply(evAtt.no.uData$Latitude,evAtt.no.uData$event,mean,na.rm=TRUE),
                              tapply(evAtt.no.uData$Longitude,evAtt.no.uData$event,mean,na.rm=TRUE))
colnames(evAtt.no.labels) <- c("event_id","NoBirthyear","NoGender","NoMeanLat","NoMeanLon")

evAtt.maybe.labels <- data.frame(unique(evAtt.maybe.uData$event),
                                 tapply(evAtt.maybe.uData$birthyear,evAtt.maybe.uData$event,mean,na.rm=TRUE),
                                 tapply(evAtt.maybe.uData$gender,evAtt.maybe.uData$event,labelMF),
                                 tapply(evAtt.maybe.uData$Latitude,evAtt.maybe.uData$event,mean,na.rm=TRUE),
                                 tapply(evAtt.maybe.uData$Longitude,evAtt.maybe.uData$event,mean,na.rm=TRUE))
colnames(evAtt.maybe.labels) <- c("event_id","MaybeBirthyear","MaybeGender","MaybeMeanLat","MaybeMeanLon")

evAtt.invited.labels <- data.frame(unique(evAtt.invited.uData$event),
                                   tapply(evAtt.invited.uData$birthyear,evAtt.invited.uData$event,mean,na.rm=TRUE),
                                   tapply(evAtt.invited.uData$gender,evAtt.invited.uData$event,labelMF),
                                   tapply(evAtt.invited.uData$Latitude,evAtt.invited.uData$event,mean,na.rm=TRUE),
                                   tapply(evAtt.invited.uData$Longitude,evAtt.invited.uData$event,mean,na.rm=TRUE))
colnames(evAtt.invited.labels) <- c("event_id","InvBirthyear","InvGender","InvMeanLat","InvMeanLon")

save("evAtt.yes.labels",file="data/evAtt.yes.labels2.RData")
save("evAtt.no.labels",file="data/evAtt.no.labels2.RData")
save("evAtt.maybe.labels",file="data/evAtt.maybe.labels2.RData")
save("evAtt.invited.labels",file="data/evAtt.invited.labels2.RData")