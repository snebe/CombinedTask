#text that signals the start of each contingency variation trial
contingency.start.text <- list("New trial (rep=0, index=0): {u'upperB': 20, u'lowerB': 1, u'lowerA': 1, u'upperA': 6, u'Order': 1}", 
                           "New trial (rep=0, index=1): {u'upperB': 6, u'lowerB': 1, u'lowerA': 1, u'upperA': 20, u'Order': 2}", 
                           "New trial (rep=0, index=2): {u'upperB': 6, u'lowerB': 1, u'lowerA': 1, u'upperA': 20, u'Order': 3}", 
                           "New trial (rep=0, index=3): {u'upperB': 20, u'lowerB': 1, u'lowerA': 1, u'upperA': 6, u'Order': 4}", 
                           "New trial (rep=0, index=4): {u'upperB': 6, u'lowerB': 1, u'lowerA': 1, u'upperA': 20, u'Order': 5}", 
                           "New trial (rep=0, index=5): {u'upperB': 20, u'lowerB': 1, u'lowerA': 1, u'upperA': 6, u'Order': 6}")

#text that signals the end of each contingency variation trial
contingency.end.text <- list("RatingScale rateA_scale: reset()")

#text signalling the end of each contingency rating
end.ratingA <- c("end rating A")
end.ratingB <- c("end rating B")

#text signalling the start and end of instrumental training
instru.text <- c("instru_instructions: autoDraw = False", "instru_finaltext: autoDraw = True")

#text signalling the start and end of extinction trial
ext.start <- c("extinction_start: autoDraw = False")
ext.end <- c("extinction_text_2: autoDraw = True")

#text signalling the start and end of reacquisition 
reacq.start <- c("extinction_start_2: autoDraw = False")
reacq.end <- c("instrufeedback_A_2: image = \'M&M.png\'")


#find times of text strings
findTime <- function(x){
  data$time[data$text == x]
}

#counts number of responses within a range
countResp <- function(x, y){
  c(sum(y > x[1] & y < x[2]))
}

#find position of an element in a vector
findPos <- function(x){
  which(data$text == x)
}

#returns the numeric value of the object at set position
findValue <- function(x){
  as.numeric(levels(data$text[x])[data$text[x]])
}

#corrects left and right to high and low

findContingency <- function(x, y){
  matrix(as.numeric(x[1,1], x[2,2], x[2,3], x[1,4], x[2,5], x[1,6],
           x[2,1], x[1,2], x[1,3], x[2,4], x[1,5], x[2,6]),
    nrow = 2,
    ncol = 6,
    dimnames = list(c(paste0("highc.", y), paste0("lowc.", y))))
}