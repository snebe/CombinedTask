##This script extracts and analyses specific and general transfer data from the Pavlovian-Instrumental Transfer task
##Before you run this you need:
#The log files for each participant in the 'R/data' folder
#Participant log files all named '(whateveryouridis).log'
#The packages that R uses installed
#-if you try to run this without the appropriate packages installed it wont work
#-you can use install.packages() to download these
#-this may take a bit of fiddling if your uni's firewall doesn't like you doing this
#You will need to create individual participant ID vectors in the 'R/functions.R' script 


source("R/functions.R")  #this loads the functions that will be needed for analysis

#Create a list of all of the participant ID vectors that are created in the 'functions.R' script

ID <- list(NW001, NW002, NW003) # add extra participant vectors here

#Create empty vectors for your data
#These will be filled in with each participant's data for that value

participant <- character(length = length(ID))
high.resp.mean <- numeric(length = length(ID))
low.resp.mean <- numeric(length = length(ID))
high.os.mean <- numeric(length = length(ID))
low.os.mean <- numeric(length = length(ID))
high.rating.mean <- numeric(length = length(ID))
low.rating.mean <- numeric(length = length(ID))
instru.deval.resp <- numeric(length = length(ID))
instru.nondeval.resp <- numeric(length = length(ID))
instru.deval.os <- numeric(length = length(ID))
instru.nondeval.os <- numeric(length = length(ID))
ext.deval.resp <- numeric(length = length(ID))
ext.nondeval.resp <- numeric(length = length(ID))
re.deval.resp <- numeric(length = length(ID))
re.nondeval.resp <- numeric(length = length(ID))


#loop through the data extraction for each participant
for(i in ID){
  data <- read.delim(i[[1]], header = FALSE) #uses the path in the participant ID vector to read the log file
  version <- i[[2]] #looks for the version in the second item of the participant vector
  
  #give data column headings
  colnames(data) <- c("time", "type", "text")
  
  #find the time that each left and right response was made
  #this subsets the time column for each row where a left or right keypress is made
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
  left.cv.rs <- as.numeric(lapply(cv.times, countResp, y=leftRtimes)) # creates a vector of number of left Rs in each CV trial
  #right responses
  right.cv.rs <- as.numeric(lapply(cv.times, countResp, y=rightRtimes))
  
  cv.resp.lr <- rbind(left.cv.rs, right.cv.rs)
  
  #count number of outcomes in each trial
  #left outcomes
  left.cv.os <- as.numeric(lapply(cv.times, countResp, y=cv.leftOtimes))  # creates a vector of number of left Os in each CV trial
  #right outcomes
  right.cv.os <- as.numeric(lapply(cv.times, countResp, y=cv.rightOtimes))
  
  cv.outcomes.lr <- rbind(left.cv.os, right.cv.os)
  
  #find left and right contingency ratings for each trial
  # left ratings
  left.rating.position <- as.list(findPos(end.ratingA) - 1)
  left.ratings <- as.numeric(lapply(left.rating.position, findValue)) # creates a vector of left Ratings in each CV trial
  
  #right.ratings
  right.rating.position <- as.list(findPos(end.ratingB) - 1)
  right.ratings <- as.numeric(lapply(right.rating.position, findValue))
  
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
  
  leftOtimes <- findTime(snackA.text)
  rightOtimes <- findTime(snackB.text)
  
  
  if((substr(version, 2, 2)) == "A"){
    devalRtimes <- leftRtimes
    nondevalRtimes <- rightRtimes
    devalOtimes <- leftOtimes
    nondevalOtimes <- rightOtimes
  } else if ((substr(version, 2, 2)) == "B"){
    devalRtimes <- rightRtimes
    nondevalRtimes <- leftRtimes
    devalOtimes <- rightOtimes
    nondevalOtimes <- leftOtimes
  } else {
    "Invalid Version Entered"
  }
  
  ###INSTRUMENTAL TRAINING DATA EXTRACTION###
  instru.times <- c(findTime(instru.start), findTime(instru.end))
  
  deval.instru.rs <- countResp(instru.times, y=devalRtimes) # counts number of responses on the action that will be associated with the devalued outcome
  nondeval.instru.rs <- countResp(instru.times, y=nondevalRtimes)
  
  deval.instru.os <- countResp(instru.times, y=devalOtimes)
  nondeval.instru.os <- countResp(instru.times, y=nondevalOtimes)
  
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
  
  
  #insert individual participant values into premade vectors
  participant[as.numeric(i[4])] <- i[[3]]
  high.resp.mean[as.numeric(i[4])] <- mean.cv.responses[1]
  low.resp.mean[as.numeric(i[4])] <- mean.cv.responses[2]
  high.os.mean[as.numeric(i[4])] <- mean.cv.outcomes[1]
  low.os.mean[as.numeric(i[4])] <- mean.cv.outcomes[2]
  high.rating.mean[as.numeric(i[4])] <- mean.cv.ratings[1]
  low.rating.mean[as.numeric(i[4])] <- mean.cv.ratings[2]
  instru.deval.resp[as.numeric(i[4])] <- deval.instru.rs
  instru.nondeval.resp[as.numeric(i[4])] <- nondeval.instru.rs
  instru.deval.os[as.numeric(i[4])] <- deval.instru.os
  instru.nondeval.os[as.numeric(i[4])] <- nondeval.instru.os
  ext.deval.resp[as.numeric(i[4])] <- deval.ext.rs
  ext.nondeval.resp[as.numeric(i[4])] <- nondeval.ext.rs
  re.deval.resp[as.numeric(i[4])] <- deval.reacq.rs
  re.nondeval.resp[as.numeric(i[4])] <- nondeval.reacq.rs
}


###CREATE DATA FRAME OF OUTPUT####
df_deval <- data.frame(participant,instru.deval.resp, instru.nondeval.resp, instru.deval.os, instru.nondeval.os, ext.deval.resp, ext.nondeval.resp, re.deval.resp, re.nondeval.resp)

df_cv <- data.frame(participant, high.resp.mean, low.resp.mean, high.os.mean, low.os.mean, high.rating.mean, low.rating.mean)

## EXPORT DATA FRAME TO EXCEL
#export group data
dir.output <- 'R/output' # sets output folder
write.csv(df_deval, file = file.path(dir.output, "group_deval.csv"), row.names = FALSE)

write.csv(df_cv, file = file.path(dir.output, "group_cv.csv"), row.names = FALSE)

