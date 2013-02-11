library(ProjectTemplate)
load.project()

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
evAtt.yes.uData <- merge(event.attendees.yes,users[,c("user_id", "birthyear", "gender")])
evAtt.no.uData <- merge(event.attendees.no,users[,c("user_id", "birthyear", "gender")])
evAtt.maybe.uData <- merge(event.attendees.maybe,users[,c("user_id", "birthyear", "gender")])
evAtt.invited.uData <- merge(event.attendees.invited,users[,c("user_id", "birthyear", "gender")])

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

#tapply to find means by ignoring NA's
foo <- tapply(evAtt.yes.uData$birthyear,evAtt.yes.uData$event,mean,na.rm=TRUE)
foo <- tapply(evAtt.no.uData$birthyear,evAtt.no.uData$event,mean,na.rm=TRUE)
foo <- tapply(evAtt.maybe.uData$birthyear,evAtt.maybe.uData$event,mean,na.rm=TRUE)
foo <- tapply(evAtt.invited.uData$birthyear,evAtt.invited.uData$event,mean,na.rm=TRUE)