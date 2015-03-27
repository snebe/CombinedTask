##load functions
source("R/functions.R")

##create list of participant vectors that are defined in the functions file
ID <- list(NW001) # add extra participant vectors here

#Create empty vectors for your data
#These will be filled in with each participant's data for that value

participant <- character(length = length(ID))
high.resp.mean <- numeric(length = length(ID))
low.resp.mean <- numeric(length = length(ID))
high.os.mean <- numeric(length = length(ID))
low.os.mean <- numeric(length = length(ID))
high.rating.mean <- numeric(length = length(ID))
low.rating.mean <- numeric(length = length(ID))
ext.deval.resp <- numeric(length = length(ID))
ext.nondeval.resp <- numeric(length = length(ID))
re.deval.resp <- numeric(length = length(ID))
re.nondeval.resp <- numeric(length = length(ID))

#Run the data analysis for each participant
## import data from log files.
data <- read.delim(NW001[1], header = FALSE)
version <- NW001[2]

#give data column headings
colnames(data) <- c("time", "type", "text")

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

# #insert individual participant values into premade vectors
# participant[as.numeric(i[4])] <- i[[3]]
# high.resp.mean <- 
# low.resp.mean <- 
# high.os.mean <- 
# low.os.mean <-
# high.rating.mean <- 
# low.rating.mean <- 
# ext.deval.resp <- deval.ext.rs
# ext.nondeval.resp <- nondeval.ext.rs
# re.deval.resp <- deval.reacq.rs
# re.nondeval.resp <- deval.reacq.rs
# }



###CREATE DATA FRAME OF OUTPUT####
df <- data.frame(participant, deval.ext.rs, nondeval.ext.rs, deval.reacq.rs, nondeval.reacq.rs)

## EXPORT DATA FRAME TO EXCEL
#export group data
dir.output <- 'R/output' # sets output folder
write.csv(df, file = file.path(dir.output, "group_deval.csv"), row.names = FALSE)

