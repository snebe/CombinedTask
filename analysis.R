##load functions
source("R/functions.R")

## import data from log files.
data <- read.delim("R/data/test.log", header = FALSE)

#give data column headings
colnames(data) <- c("time", "type", "text")

##ID
participant <- c("test")
## version
version <- c("AA")

#get the value in the time column for each row where a left of right keypress is made
leftRtimes <- data$time[data$text == "Keypress: c"]
rightRtimes <- data$time[data$text == "Keypress: m"]


### CONTINGENCY VARIATION DATA EXTRACTION #####
#get the value in the time column for each row where a left or right outcome is earned
cv.leftOtimes <- data$time[data$text == "earnA"]
cv.rightOtimes <- data$time[data$text == "earnB"]

#start times for each contingency variation trial
cv.start.times <- lapply(contingency.start.text, findTime)

#end times for each contingency variation trial
cv.end.times <- as.list(sapply(contingency.end.text, findTime))

#create a list of start and end times for each contingency variation trial
cv.times <- mapply(cv.start.times, cv.end.times, FUN = list, SIMPLIFY = FALSE)

#count number of responses in each trial 
#left responses
left.cv.rs <- lapply(cv.times, countResp, y=leftRtimes)
#right responses
right.cv.rs <- lapply(cv.times, countResp, y=rightRtimes)

cv.resp.lr <- rbind(left.cv.rs, right.cv.rs)


#count number of outcomes in each trial
#left outcomes
left.cv.os <- lapply(cv.times, countResp, y=cv.leftOtimes)
#right outcomes
right.cv.os <- lapply(cv.times, countResp, y=cv.rightOtimes)

cv.outcomes.lr <- rbind(left.cv.os, right.cv.os)

#find left and right contingency ratings for each trial
# left ratings
left.rating.position <- as.list(findPos(end.ratingA) - 1)
left.ratings <- lapply(left.rating.position, findValue)

#right.ratings
right.rating.position <- as.list(findPos(end.ratingB) - 1)
right.ratings <- lapply(right.rating.position, findValue)

cv.ratings.lr <- rbind(left.ratings, right.ratings)

## convert left and right to high and low contingency
#high and low contingency data by trial
trial.cv.ratings <- findContingency(cv.ratings.lr, "ratings")
trial.cv.outcomes <- findContingency(cv.outcomes.lr, "outcomes")
trial.cv.responses <- findContingency(cv.resp.lr, "responses")

#find mean high and low contingency data
mean.cv.ratings <- rowMeans(trial.cv.ratings)
mean.cv.outcomes <- rowMeans(trial.cv.outcomes)
mean.cv.responses <- rowMeans(trial.cv.responses)

##assign left and right responses as devalued or non-devalued based on task version
# if version is XA, LEFT outcome is devalued
# if version is XB, RIGHT outcome is devalued

if((substr(version, 2, 2)) == "A"){
  devalRtimes <- leftRtimes
  nondevalRtimes <- rightRtimes
} else if ((substr(version, 2,2)) == "B"){
  devalRtimes <- rightRtimes
  nondevalRtimes <- leftRtimes
} else {
  "Invalid Version Entered"
}


### EXTINCTION DATA EXTRACTION ###
ext.times <- c(findTime(ext.start), findTime(ext.end))

#count number of responses made during extinction
deval.ext.rs <- countResp(ext.times, y=devalRtimes)
nondeval.ext.rs <- countResp(ext.times, y=nondevalRtimes)

### REACQUISITION DATA EXTRACTION ###
reacq.times <- c(findTime(reacq.start), findTime(reacq.end))

#count number of left and right responses made during reacquisition
deval.reacq.rs <- countResp(reacq.times, y=devalRtimes)
nondeval.reacq.rs <- countResp(reacq.times, y=nondevalRtimes)


###CREATE DATA FRAME OF OUTPUT####
df <- data.frame(participant, deval.ext.rs, nondeval.ext.rs, deval.reacq.rs, nondeval.reacq.rs)